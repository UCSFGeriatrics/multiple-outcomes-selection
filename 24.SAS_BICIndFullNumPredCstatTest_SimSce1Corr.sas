***********************************************************************************************************************************************************************************;
*Program: 24.SAS_BICIndFullNumPredCstatTest_SimSce1Corr                                                                                                                            ;                                                               
*Purpose: Compute summary statistics for Number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in Scenario 1 simulated testing correlated data   ;                                     
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
%let trainmod_ind=outdata.bicnewsim500corrby;
%let testdata=savedata.sim500corrTest;

*QC: Check trainmod;
ods select all;
proc means data=&trainmod_ind n mean clm stderr maxdec=4;
 var  numVarsfinsim numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath C_adl C_iadl C_walk C_death;
run;
proc print data=&trainmod_ind (obs=5); run;

*QC: Check testdata;
proc contents data=&testdata; run;

*Derive intersect and union model from the &trainmod_ind;
data trainmod (drop=delims i x: n i temp);
  set &trainmod_ind (drop=numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath);
  length  union intersect $1000;

 array x{39} $ 32; /*maximum of 39 predictors in Full model*/

 n=0; 

 do i=1 to countw(VARINMODEL_adl,' ');
  temp=scan(VARINMODEL_adl,i,' ');
  if temp not in x then do; n+1; x{n}=temp; end;
 end;

 do i=1 to countw(VARINMODEL_iadl,' ');
  temp=scan(VARINMODEL_iadl,i,' ');
  if temp not in x then do; n+1; x{n}=temp; end;
  else if temp in x and indexw(VARINMODEL_walk,temp) and indexw(VARINMODEL_death,temp) then intersect=catx(' ',intersect,temp);
 end;

 do i=1 to countw(VARINMODEL_walk,' ');
  temp=scan(VARINMODEL_walk,i,' ');
  if temp not in x then do; n+1; x{n}=temp; end;
 end;

 do i=1 to countw(VARINMODEL_death,' ');
  temp=scan(VARINMODEL_death,i,' ');
  if temp not in x then do; n+1; x{n}=temp; end;
 end;

 union=catx(' ',of x{*});

 delims = ' ';
 numVarsfinsim_union=countw(union, delims);
 numVarsfinsim_intersect=countw(intersect, delims);

 numVarsfinsimadl=countw(VARINMODEL_adl, delims);
 numVarsfinsimiadl=countw(VARINMODEL_iadl, delims);
 numVarsfinsimwalk=countw(VARINMODEL_walk, delims);
 numVarsfinsimdeath=countw(VARINMODEL_death, delims);

run;

*QC;
proc means data=trainmod n mean clm stderr maxdec=4 median p25 p75;
 var  numVarsfinsim numVarsfinsim_union numVarsfinsim_intersect numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath C_adl C_iadl C_walk C_death;
run;

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

***************************************************** Calculate C-stat for each outcome using the individual, union, intersect, full models from training data on testing simulated data ******************************************************************;
options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/


%macro c_all(predictors=, common_pred=, model=);
 %do i=1 %to &S;
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the first id of ith simulation*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the last id of ith simulation*/

  data simdata4;
   set simdata3 (FIRSTOBS=&fobs OBS=&lobs);
  run;
  sasfile WORK.simdata4 load;

 /* For each simulated dataset define VARNAME as in the individual/union/intersect/full */

   %if &common_pred=no %then %do;
	  %do j=1 %to &NUMOUTCOMES;
	    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the jth outcome, jth time, jth label*/
	    %let TIME=%scan(&ALLTIME,&j);
		%let LABEL=%scan(&ALLLABEL,&j);
		%let PREDICTOR=%scan(&predictors,&j);

		data _null_;
	      set trainmod (keep=sim &PREDICTOR);
	      where sim=&i;
	      call symputx ('VARNAME' , &PREDICTOR);
	     run;

		proc phreg data = simdata4 CONCORDANCE=HARRELL; 
	      class &VARNAME;
	      model &time*&outcome(0) = &VARNAME;
		  ods output CONCORDANCE=concord ;
	    run;

		data CTABLE_&label;
	     set concord (keep= estimate rename=(estimate=C_&label));
		 length VARINMODEL_&label $1000;
	     sim=&i;
		 VARINMODEL_&label="&VARNAME";
	    run;

		proc delete data=concord; run; quit;

     %end; /*j loop*/

	 data ctable; 
       merge CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death;
	   by sim;
     run;

	proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

   %end; /*&common pred DO*/


   %else %if &common_pred=yes %then %do;

     %if &model=union or &model=intersect %then %do;
	  data _null_;
	   set trainmod (keep=sim &predictors);
	   where sim=&i;
	   call symputx ('VARNAME' , &predictors);
	  run;
	 %end;
     %else %if &model=full %then %do;
	  %let VARNAME=&predictors;
	 %end;

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
	     set concord (keep= estimate rename=(estimate=C_&label));
		 length VARINMODEL $1000;
	     sim=&i;
		 VARINMODEL="&VARNAME";
	    run;

		proc delete data=concord; run; quit;

	 %end; /*j loop*/

	 data ctable; 
       merge CTABLE_adl CTABLE_iadl(drop=VARINMODEL) CTABLE_walk(drop=VARINMODEL) CTABLE_death(drop=VARINMODEL);
	   by sim;
       C_avg=mean(C_adl, C_iadl, C_walk, C_death);
     run;

	proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

   %end; /*&common pred DO*/

  sasfile WORK.simdata4 close;
  proc delete data=simdata4; run; quit;

  proc append base=sim500&model.test_c data=ctable force; run;
  proc delete data=ctable; run; quit;

  %end; /*i loop*/

%mend c_all;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_all (predictors=VARINMODEL_adl VARINMODEL_iadl VARINMODEL_walk VARINMODEL_death, common_pred=no, model=ind);

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_all (predictors=union, common_pred=yes, model=union);

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_all (predictors=intersect, common_pred=yes, model=intersect);

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_all (predictors=dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
		     FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	         OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
	         qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER, common_pred=yes, model=full);

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;


/*
======MONITORING: 2020-09-21, 12:14======
======MONITORING: 2020-09-21, 12:27======
======MONITORING: 2020-09-21, 12:40======
======MONITORING: 2020-09-21, 12:51======
======MONITORING: 2020-09-21, 13:08======
*/

*Save permanent datasets;
data outdata.sim500indtest_c; set sim500indtest_c; run;
data outdata.sim500uniontest_c; set sim500uniontest_c; run;
data outdata.sim500intersecttest_c; set sim500intersecttest_c; run;
data outdata.sim500fulltest_c; set sim500fulltest_c; run;

*Summary stats of individual training simulated models on baBIC testing simulated data;
proc means data=outdata.sim500indtest_c stackodsoutput n mean clm stderr maxdec=4; 
 var C_adl C_iadl C_walk C_death;
 ods output summary=outdata.sim500indtest_cmeans(rename=(Mean=Mean_Sim_indtesting));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.sim500indtest_cmeans
            OUTFILE= "path\sim500indtest_cmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of union training simulated models on baBIC testing simulated data;
proc means data=outdata.sim500uniontest_c stackodsoutput n mean clm stderr maxdec=4; 
 var C_avg C_adl C_iadl C_walk C_death;
 ods output summary=outdata.sim500uniontest_cmeans(rename=(Mean=Mean_Sim_uniontesting));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.sim500uniontest_cmeans
            OUTFILE= "path\sim500uniontest_cmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of intersect training simulated models on baBIC testing simulated data;
proc means data=outdata.sim500intersecttest_c stackodsoutput n mean clm stderr maxdec=4; 
 var C_avg C_adl C_iadl C_walk C_death;
 ods output summary=outdata.sim500inttest_cmeans(rename=(Mean=Mean_Sim_inttesting));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.sim500inttest_cmeans
            OUTFILE= "path\sim500inttest_cmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of full training simulated models on baBIC testing simulated data;
proc means data=outdata.sim500fulltest_c stackodsoutput n mean clm stderr maxdec=4; 
 var C_avg C_adl C_iadl C_walk C_death;
 ods output summary=outdata.sim500fulltest_cmeans(rename=(Mean=Mean_Sim_fulltesting));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.sim500fulltest_cmeans
            OUTFILE= "path\sim500fulltest_cmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
