***********************************************************************************************************************************************************************************;
*Program: 31.SAS_baBICNumPredCstatTest_SimSce2corig                                                                                                                                ;                                                               
*Purpose: Compute summary statistics for number of predictors and C-statistic for baBIC method in simulations of Scenario 2 with case-study censoring and testing data             ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2021.02.25																				                                                                               ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
libname outtrain "path";
libname outtest "path";

*****************************************************Define Training data with models and Testing simulated data ********************************************************;
%let trainmodels=outtrain.bicsimbabic_Sce2corig;
%let testdata=outtest.sim500testIndcorig;

data &trainmodels;
 set &trainmodels;
 numVarsfinsim=countw(VARINMODEL, '');
run;

proc print data=&trainmodels (obs=20); run;

*Get mean of number of predictors;
proc means data=&trainmodels stackodsoutput n mean clm stderr p50 p25 p75 maxdec=4; 
 var numVarsfinsim C_adl C_iadl C_walk C_death C_avg;
 ods output summary=outtrain.bicsimbabic_Sce2corig_stats;
run;

PROC EXPORT DATA= outtrain.bicsimbabic_Sce2corig_stats
            OUTFILE= "path\bicsimbabic_Sce2corig_stats.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Custom percentiles: https://blogs.sas.com/content/iml/2013/10/23/percentiles-in-a-tabular-format.html;
proc stdize data=&trainmodels PctlMtd=ORD_STAT outstat=&trainmodels._percentile pctlpts=2.5, 97.5;
 var numVarsfinsim ;
run;
/*Specify the PCTLMTD= option so that the algorithm uses the traditional "sort the data" algorithm for computing percentiles,
rather than a newer one-pass algorithm. Although the one-pass algorithm is very fast and well-suited for computing the median, 
it is not recommended for computing extreme percentiles such as the 2.5th and 97.5th.*/
 
data &trainmodels._percentile;
 set &trainmodels._percentile;
 where _type_ =: 'P';
run;

proc print data=&trainmodels._percentile noobs; run;



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
   set &trainmodels (keep=sim VARINMODEL);
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

  proc append base=c_baBICSce2corig data=ctable force; run;
  proc delete data=ctable; run; quit;

  %end; /*i loop*/
%mend cstat;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%cstat;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

proc sort data=c_baBICSce2corig out=outtest.c_baBICSce2corig; by sim; run;

*Summary stats of baBIC training simulated models on baBIC testing simulated data;
proc means data=outtest.c_baBICSce2corig stackodsoutput n mean clm stderr maxdec=4; 
 var C_adl C_iadl C_walk C_death C_avg;
 ods output summary=outtest.c_baBICSce2corig_means (rename=(Mean=Mean_Sim_baBICtesting));
run;

PROC EXPORT DATA= outtest.c_baBICSce2corig_means
            OUTFILE= "path\c_baBICSce2corig_means.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Custom percentiles baBIC;
proc stdize data=outtest.c_baBICsce2corig PctlMtd=ORD_STAT outstat=outtest.c_baBICsce2corig_percentiles pctlpts=2.5, 97.5;
 var C_adl C_iadl C_walk C_death ;
run;
/*Specify the PCTLMTD= option so that the algorithm uses the traditional "sort the data" algorithm for computing percentiles,
rather than a newer one-pass algorithm. Although the one-pass algorithm is very fast and well-suited for computing the median, 
it is not recommended for computing extreme percentiles such as the 2.5th and 97.5th.*/
 
data outtest.c_baBICsce2corig_percentiles;
 set outtest.c_baBICsce2corig_percentiles;
 where _type_ =: 'P';
 C_adl=round(C_adl,0.01);
 C_iadl=round(C_iadl,0.01);
 C_walk=round(C_walk,0.01);
 C_death=round(C_death,0.01);
 format C_adl C_iadl C_walk C_death 5.2;
run;

proc print data=outtest.c_baBICsce2corig_percentiles noobs; run;




