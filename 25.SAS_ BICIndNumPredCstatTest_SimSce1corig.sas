***********************************************************************************************************************************************************************************************************;
*Program: 25.SAS_BICIndNumPredCstatTest_SimSce1corig                                                                                                                                                       ;                                                               
*Purpose: Compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersection, and Full methods in simulations of Scenario 1 with case-study censoring and testing data;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                                                       ;
*Finished: 2021.01.28																				                                                                                                       ;
***********************************************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
libname outind "path";
libname outtest "path";

options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

/***********************************************************************************************************************************************/
*Create union and intersection of the best individual models for each outcome in each simulated dataset;
%let S=500;
%let trainmodels=outind.BICsimInd_sce1corig;
%let testdata=outtest.sim500testbabiccorig;

/*Merge all 4 datasets with the best individual model within each outcome for each simulated dataset*/
data BICsim;
   merge outind.BICsimInd_corig_adl outind.BICsimInd_corig_iadl outind.BICsimInd_corig_walk outind.BICsimInd_corig_death;
   by sim;
run;

data &trainmodels  (drop=delims i x: n i temp);
 set BICsim ;
 length  union inters $1000;

 array x{39} $ 32; /*maximum of 39 predictors in Full model*/

 n=0; 

 do i=1 to countw(VARINMODEL_adl,' ');
  temp=scan(VARINMODEL_adl,i,' ');
  if temp not in x then do; n+1; x{n}=temp; end;
 end;

 do i=1 to countw(VARINMODEL_iadl,' ');
  temp=scan(VARINMODEL_iadl,i,' ');
  if temp not in x then do; n+1; x{n}=temp; end;
  else if temp in x and indexw(VARINMODEL_walk,temp) and indexw(VARINMODEL_death,temp) then inters=catx(' ',inters,temp);
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
 numVarsfinsim_inters=countw(inters, delims);

 numVarsfinsimadl=countw(VARINMODEL_adl, delims);
 numVarsfinsimiadl=countw(VARINMODEL_iadl, delims);
 numVarsfinsimwalk=countw(VARINMODEL_walk, delims);
 numVarsfinsimdeath=countw(VARINMODEL_death, delims);

run;

*Summary stats of models from simulated dataset models by outcome;
proc means data=&trainmodels stackodsoutput n mean stderr clm median p25 p75 maxdec=4; 
 var numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath numVarsfinsim_union numVarsfinsim_inters C_adl C_iadl C_walk C_death iauc_adl iauc_iadl iauc_walk iauc_death VARSEC_adl VARSEC_iadl VARSEC_walk VARSEC_death ;
 ods output summary=&trainmodels._stats(rename=(Mean=Mean_sim_Ind));
proc sort; by variable; run;

PROC EXPORT DATA= &trainmodels._stats
            OUTFILE= "path\bicsimind_sce1corig_stats.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc delete data=BICsim; run; quit;


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

***************************************************** Calculate C-stat for each outcome using the individual, union, inters, full models from training data on testing simulated data ******************************************************************;
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

 /* For each simulated dataset define VARNAME as in the individual/union/inters/full */

   %if &common_pred=no %then %do;
	  %do j=1 %to &NUMOUTCOMES;
	    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the jth outcome, jth time, jth label*/
	    %let TIME=%scan(&ALLTIME,&j);
		%let LABEL=%scan(&ALLLABEL,&j);
		%let PREDICTOR=%scan(&predictors,&j);

		data _null_;
	      set &trainmodels (keep=sim &PREDICTOR);
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

     %if &model=union or &model=inters %then %do;
	  data _null_;
	   set &trainmodels (keep=sim &predictors);
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

  proc append base=c_&model.sce1corig data=ctable force; run;
  proc delete data=ctable; run; quit;

  %end; /*i loop*/

%mend c_all;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_all (predictors=VARINMODEL_adl VARINMODEL_iadl VARINMODEL_walk VARINMODEL_death, common_pred=no, model=ind);

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_all (predictors=union, common_pred=yes, model=union);

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_all (predictors=inters, common_pred=yes, model=inters);

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_all (predictors=dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
		     FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	         OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
	         qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER, common_pred=yes, model=full);

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

*Save permanent datasets;
data outtest.c_indsce1corig; set c_indsce1corig; run;
data outtest.c_unionsce1corig; set c_unionsce1corig; run;
data outtest.c_interssce1corig; set c_interssce1corig; run;
data outtest.c_fullsce1corig; set c_fullsce1corig; run;

*Summary stats of individual training simulated models on baBIC testing simulated data;
proc means data=outtest.c_indsce1corig stackodsoutput n mean stderr clm  maxdec=4; 
 var C_adl C_iadl C_walk C_death;
 ods output summary=outtest.c_indsce1corig_means(rename=(Mean=Mean_Sim_indtesting));
proc sort; by variable; run;

PROC EXPORT DATA= outtest.c_indsce1corig_means
            OUTFILE= "path\c_indsce1corig_means.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of union training simulated models on baBIC testing simulated data;
proc means data=outtest.c_unionsce1corig stackodsoutput n mean stderr clm  maxdec=4; 
 var C_avg C_adl C_iadl C_walk C_death;
 ods output summary=outtest.c_unionsce1corig_means(rename=(Mean=Mean_Sim_uniontesting));
proc sort; by variable; run;

PROC EXPORT DATA= outtest.c_unionsce1corig_means
            OUTFILE= "path\c_unionsce1corig_means.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of inters training simulated models on baBIC testing simulated data;
proc means data=outtest.c_interssce1corig stackodsoutput n mean stderr clm  maxdec=4; 
 var C_avg C_adl C_iadl C_walk C_death;
 ods output summary=outtest.c_interssce1corig_means(rename=(Mean=Mean_Sim_inttesting));
proc sort; by variable; run;

PROC EXPORT DATA= outtest.c_interssce1corig_means
            OUTFILE= "path\c_interssce1corig_means.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of full training simulated models on baBIC testing simulated data;
proc means data=outtest.c_fullsce1corig stackodsoutput n mean stderr clm  maxdec=4; 
 var C_avg C_adl C_iadl C_walk C_death;
 ods output summary=outtest.c_fullsce1corig_means(rename=(Mean=Mean_Sim_fulltesting));
proc sort; by variable; run;

PROC EXPORT DATA= outtest.c_fullsce1corig_means
            OUTFILE= "path\c_fullsce1corig_means.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
