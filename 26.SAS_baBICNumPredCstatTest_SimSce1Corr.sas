***********************************************************************************************************************************************************************************;
*Program: 26.SAS_baBICNumPredCstatTest_SimSce1Corr                                                                                                                                 ;                                                               
*Purpose: Compute summary statistics for Number of predictors and C-statistic for baBIC method in Scenario 1 simulated testing correlated data                                     ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2020.10.21																				                                                                               ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
libname outdata "path";

options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/
*****************************************************Define Training data with models and Testing simulated data ********************************************************;
%let trainmod=outdata.baBICsim500corrSce1;
%let testdata=savedata.sim500corrTest;

*QC: Check trainmod;
ods select all;
proc means data=&trainmod n mean clm stderr median p25 p75 maxdec=4;
 var numVarsfinsim VARSEC C_avg C_adl C_iadl C_walk C_death iauc_avg BIC_avg;
run;
proc print data=&trainmod (obs=20); run;

*QC: Check testdata;
proc contents data=&testdata; run;


*****************************************************PREPARE TESTING SIMULATED DATA FOR MACRO ********************************************************;

proc sort data=&testdata out=simdata; by newid; run;/*11062000 observations and 5 variables.*/

*Merge with original dataset to get the covariates;
data simdata2; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) 
        simdata;
  by newid;
proc sort; by sim outcome newid; run;
/*11,062,000 observations and 55+5=60 variables.*/

/*Change format of simulated dataset from long where outcomes are stacked to wide where outcomes are merged*/
*ADL;
data adl;
 set simdata2 (rename=(time=time_adldepdth status=status_adldepdth));
 where outcome="adl";
proc sort; by sim newid; run; /*2,765,500 observations and 60 variables*/
*IADL;
data iadl (keep=sim newid time_iadldifdth status_iadldifdth);
 set simdata2 (rename=(time=time_iadldifdth status=status_iadldifdth));
 where outcome="iadl";
proc sort; by sim newid; run; /*2,765,500 observations and 4 variables.*/
*Walk;
data walk (keep=sim newid time_walkdepdth status_walkdepdth);
 set simdata2 (rename=(time=time_walkdepdth status=status_walkdepdth));
 where outcome="walk";
proc sort; by sim newid; run; /*2,765,500 observations and 4 variables.*/
*Death;
data death (keep=sim newid time2death death);
 set simdata2 (rename=(time=time2death status=death));
 where outcome="death";
proc sort; by sim newid; run; /*2,765,500 observations and 4 variables.*/

data simdata3;
 merge adl iadl walk death;
 by sim newid;
proc sort; by sim newid; run; /*2,765,500 observations and 60+6=66 variables*/

proc delete data=simdata simdata2 adl iadl walk death; run; quit;

*Define macro variables;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth status_iadldifdth status_walkdepdth death;
%let ALLTIME=time_adldepdth time_iadldifdth time_walkdepdth time2death;
%let ALLLABEL= adl iadl walk death; /*labels for outcomes*/
proc sql noprint; select max(sim) format 3. into :S from simdata3; quit; /*create macro variable with total number of simulated datasets*/
%put "&S"; 


***************************************************** Calculate C-stat for each outcome using the baBIC models from simulated datasets ******************************************************************;
options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

%macro cstat;
 %do i=1 %to &S;
 /*For each simulated dataset define VARNAME as the variables selected in baBIC model of training data*/
  data _null_;
   set &trainmod (keep=sim VARINMODEL);
   where sim=&i;
   call symputx ('VARNAME' ,VARINMODEL);
  run;

  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the first id of ith simulation*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the last id of ith simulation*/

  data simdata4;
   set simdata3 (FIRSTOBS=&fobs OBS=&lobs);
  run;
  sasfile WORK.simdata4 load;

  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the jth outcome, jth time, jth label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);

	proc phreg data = simdata4 CONCORDANCE=HARRELL; 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output CONCORDANCE=concord ;
    run;

	data CTABLE_&label;
     set concord (keep= estimate rename=(estimate=c_&label));
	 length VARINMODEL $1000;
     sim=&i;
	 VARINMODEL="&VARNAME";
    run;

	proc delete data=concord; run; quit;

  %end; /*j loop*/

  sasfile WORK.simdata4 close;
  proc delete data=simdata4; run; quit;

  data ctable; 
    merge CTABLE_adl CTABLE_iadl(drop=VARINMODEL) CTABLE_walk(drop=VARINMODEL) CTABLE_death(drop=VARINMODEL);
	by sim;
    C_avg=mean(c_adl,c_iadl, c_walk, c_death);
  run;

  proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

  proc append base=ctablesim data=ctable force; run;
  proc delete data=ctable; run; quit;

  %end; /*i loop*/
%mend cstat;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%cstat;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

proc sort data=ctablesim out=outdata.sim&S.baBICtest_c; by sim; run;

PROC EXPORT DATA=outdata.sim&S.baBICtest_c
            OUTFILE= "path\sim&S.baBICtest_c.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of baBIC training simulated models on baBIC testing simulated data;
proc means data=outdata.sim&S.baBICtest_c stackodsoutput n mean clm stderr maxdec=4; 
 var C_adl C_iadl C_walk C_death C_avg;
 ods output summary=outdata.sim&S.baBICtest_cmeans(rename=(Mean=Mean_Sim_baBICtesting));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.sim&S.baBICtest_cmeans
            OUTFILE= "path\sim&S.baBICtest_Cmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


proc delete data=ctablesim simdata3; run; quit;



