***********************************************************************************************************************************************************************************;
*Program: 46a.SAS_CumIncSce3uncorr                                                                                                                                                 ;                                                               
*Purpose: Compute Cumulative Incidence for Scenario 3 simulated training uncorrelated data                                                                                         ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2020.10.21																				                                                                               ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
libname outdata "path";

*****************************************************PREPARE DATA FOR MACRO ********************************************************;

**************** Create dummy variables for categorcal predictors: see example in SASGeneral folder (DummyVariables.doc) ************;
data originaldata;
 set savedata.originaldata ;
proc sort; by newid; run;
/* 5531 observations and 64 variables */


data originaldata;
	array age(10)    (10*0); /*set all the elements of the array= 0 */
	array alcoh(3)   (3*0); 
	array canc(3)    (3*0); 
	array cogdl(3)   (3*0); 
	array cogim(3)   (3*0); 
	array fal(3)     (3*0); 
	array hear(5)    (5*0); 
	array lun(3)     (3*0); 
	array climb(3)   (3*0); 
	array walk(3)    (3*0);
	array bmi(5)     (5*0); 
	array father(4)  (4*0);
	array mother(4)  (4*0);
	array health(5)  (5*0);
	array smoke(3)   (3*0); 
	array strok(3)   (3*0); 

	set originaldata;

    COGDLRC3G=COGDLRC3G+1; /* categorical predictors need to be 1,2,3,etc, instead of 0,1,2 */
	COGIMRC3G=COGIMRC3G+1; 

	if dAGE ne . then age(dAGE) = 1; /* if dAGE = 3, then dAGE(age) = 1 will assign a value of 1 to the third element in the array, i.e., assign 1 to age3 */
	if ALCOHOL ne . then alcoh(ALCOHOL) = 1; 
	if CANCER ne . then canc(CANCER) = 1; 
	if COGDLRC3G ne . then cogdl(COGDLRC3G) = 1; 
	if COGIMRC3G ne . then cogim(COGIMRC3G) = 1; 
	if FALL ne . then fal(FALL) = 1; 
	if HEARING ne . then hear(HEARING) = 1; 
	if LUNG ne . then lun(LUNG) = 1; 
	if OTHERCLIM3G ne . then climb(OTHERCLIM3G) = 1; 
	if OTHERWALK ne . then walk(OTHERWALK) = 1; 
	if qBMI ne . then bmi(qBMI) = 1; 
	if qFAGE ne . then father(qFAGE) = 1; 
	if qMAGE ne . then mother(qMAGE) = 1; 
	if SHLT ne . then health(SHLT) = 1; 
	if SMOKING ne . then smoke(SMOKING) = 1; 
	if STROKE ne . then strok(STROKE) = 1; 

    output;

	if dAGE ne . then age(dAGE) = 0; /* need to reset age3=0 for the rest of the records */
	if ALCOHOL ne . then alcoh(ALCOHOL) =0; 
	if CANCER ne . then canc(CANCER) =0; 
	if COGDLRC3G ne . then cogdl(COGDLRC3G) =0; 
	if COGIMRC3G ne . then cogim(COGIMRC3G) =0; 
	if FALL ne . then fal(FALL) =0; 
	if HEARING ne . then hear(HEARING) =0; 
	if LUNG ne . then lun(LUNG) =0; 
	if OTHERCLIM3G ne . then climb(OTHERCLIM3G) =0; 
	if OTHERWALK ne . then walk(OTHERWALK) =0; 
	if qBMI ne . then bmi(qBMI) =0; 
	if qFAGE ne . then father(qFAGE) =0; 
	if qMAGE ne . then mother(qMAGE) =0; 
	if SHLT ne . then health(SHLT) =0; 
	if SMOKING ne . then smoke(SMOKING) =0; 
	if STROKE ne . then strok(STROKE) =0; 
run;
/*5531 observations and 127 variables.*/


*Leave only id and covariates in originaldata2;
data originaldata2; 
 set originaldata(keep=newid age2-age10 SEX alcoh2 alcoh3 ARTHRITIS canc2 canc3 cogdl2 cogdl3 cogim2 cogim3 DIABETES DRIVE EDU EXERCISE EYE2G
                  fal2 fal3 HEARAID hear2-hear5 HEARTFAILURE HYPERTENSION INCONTINENCE LALONE lun2 lun3 MSTAT OTHERARM
                  OTHERCHAIR climb2 climb3 OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP walk2 walk3 PAIN CESDALL
                  bmi2-bmi5 father2-father4 mother2-mother4 health2-health5 smoke2 smoke3 strok2 strok3 VOLUNTEER);
proc sort; by newid; run;
/*5531 observations and 71 variables.*/


/**************** Get simulated data ************/

proc contents data=originaldata2; run;

proc sort data=savedata.sim500uncorrFullTrain  out=simdata; by newid; run;/*11062000 observations and 5 variables.*/

*Merge with original dataset to get the covariates;
data simdata2; 
  merge originaldata2 simdata;
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

proc freq data=simdata3; tables status_adldepdth status_iadldifdth status_walkdepdth death; run;
proc means data=simdata3 min max; var time_adldepdth time_iadldifdth time_walkdepdth time2death; run;

***************************************************** Compute Cumulative Incidence for each simulated data using the baBIC models from simulated datasets ******************************************************************;

*Define macro variables;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth status_iadldifdth status_walkdepdth death;
%let ALLTIME=time_adldepdth time_iadldifdth time_walkdepdth time2death;
%let ALLLABEL= adl iadl walk death; /*labels for outcomes*/
proc sql noprint; select max(sim) format 3. into :S from simdata3; quit; /*create macro variable with total number of simulated datasets*/
%put "&S";
 
%let VARNAME=age2-age10 SEX alcoh2 alcoh3 ARTHRITIS canc2 canc3 cogdl2 cogdl3 cogim2 cogim3 DIABETES DRIVE EDU EXERCISE EYE2G
                  fal2 fal3 HEARAID hear2-hear5 HEARTFAILURE HYPERTENSION INCONTINENCE LALONE lun2 lun3 MSTAT OTHERARM
                  OTHERCHAIR climb2 climb3 OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP walk2 walk3 PAIN CESDALL
                  bmi2-bmi5 father2-father4 mother2-mother4 health2-health5 smoke2 smoke3 strok2 strok3 VOLUNTEER;

*Covariate data with all 39 predictors;
data covdata_means;
 set outdata.covdata_means (keep=age2-age10 SEX alcoh2 alcoh3 ARTHRITIS canc2 canc3 cogdl2 cogdl3 cogim2 cogim3 DIABETES DRIVE EDU EXERCISE EYE2G
                  fal2 fal3 HEARAID hear2-hear5 HEARTFAILURE HYPERTENSION INCONTINENCE LALONE lun2 lun3 MSTAT OTHERARM
                  OTHERCHAIR climb2 climb3 OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP walk2 walk3 PAIN CESDALL
                  bmi2-bmi5 father2-father4 mother2-mother4 health2-health5 smoke2 smoke3 strok2 strok3 VOLUNTEER);
run;

*Status;
proc freq data=originaldata; tables status_adldepdth status_iadldifdth status_walkdepdth death; run;
 
*Maximum time-to-event;
proc means data=originaldata mean min max; 
 var time_adldepdth time_iadldifdth time_walkdepdth time2death;
run; /*time_walkdepdth, max=15.0278689, rest of outcomes: adl(14.9759563), iadl(14.9349727), death (14.9945280)*/

*Test Comp-risk ADL;
proc phreg data = originaldata;  
  model time_adldepdth*status_adldepdth(0) = &VARNAME /eventcode=1 ;
  baseline out=CumIncPred(keep=time_adldepdth cif) covariates=covdata_means CIF=cif timelist=0 to 14 by 1 ;
run;

options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

%macro CumInc;

*Fit models in original data;
proc phreg data = originaldata;  
  model time_adldepdth*status_adldepdth(0) = &VARNAME /eventcode=1 ;
  baseline out=CumIncPred_adl(keep=time_adldepdth cif rename=(time_adldepdth=time) ) covariates=covdata_means CIF=cif timelist=0 to 14 by 1 ;
run;
proc phreg data = originaldata;  
  model time_iadldifdth*status_iadldifdth(0) = &VARNAME /eventcode=1 ;
  baseline out=CumIncPred_iadl(keep=time_iadldifdth cif rename=(time_iadldifdth=time) ) covariates=covdata_means CIF=cif timelist=0 to 14 by 1 ;
run;
proc phreg data = originaldata;  
  model time_walkdepdth*status_walkdepdth(0) = &VARNAME /eventcode=1 ;
  baseline out=CumIncPred_walk(keep=time_walkdepdth cif rename=(time_walkdepdth=time) ) covariates=covdata_means CIF=cif timelist=0 to 14 by 1 ;
run;
proc phreg data = originaldata;  
  model time2death*death(0) = &VARNAME ;
  baseline out=CumIncPred_death(keep=time2death s rename=(time2death=time) ) covariates=covdata_means survival=s timelist=0 to 14 by 1 ;
run;

data CumIncPred_adl; set CumIncPred_adl; length outcome $8; outcome="adl"; sim=0; run;
data CumIncPred_iadl; set CumIncPred_iadl; length outcome $8; outcome="iadl"; sim=0; run;
data CumIncPred_walk; set CumIncPred_walk; length outcome $8; outcome="walk"; sim=0; run;
data CumIncPred_death (drop=s); set CumIncPred_death; length outcome $8; cif=1-s; outcome="death";  sim=0; run;


 %do i=1 %to &S;
 /*Fit the baBIC model on each simulated dataset to obtain the CumInc */

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

	proc phreg data = simdata4;  
      model &time*&outcome(0) = &VARNAME;
     baseline out=CumIncPred(keep=&time s rename=(&time=time) ) covariates=covdata_means survival=s timelist=0 to 14 by 1 ;
    run;

    data CumIncPred (drop=s); set CumIncPred; length outcome $8; cif=1-s; outcome="&label";  sim=&i; run;

    proc append base=CumIncPred_&label data=CumIncPred force; run;

    proc delete data=CumIncPred; run; quit;
	
  %end; /*j loop*/

  sasfile WORK.simdata4 close;
  proc delete data=simdata4; run; quit;

  %end; /*i loop*/
%mend CumInc;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%CumInc;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

/*
======MONITORING: 2020-09-25, 18:27======
======MONITORING: 2020-09-25, 18:36======
*/

*QC;
ods select all;
proc freq data=CumIncPred_adl; tables sim time; run;
proc means data=CumIncPred_adl ; var cif; run; /*7515=500+1(original)*15*/
proc means data=CumIncPred_adl ; var cif; class sim; run;

proc freq data=CumIncPred_iadl; tables sim time; run;
proc means data=CumIncPred_iadl ; var cif; run; /*7515=500+1(original)*15*/
proc means data=CumIncPred_iadl ; var cif; class sim; run;

proc freq data=CumIncPred_walk; tables sim time; run;
proc means data=CumIncPred_walk ; var cif; run; /*7515=500+1(original)*15*/
proc means data=CumIncPred_walk ; var cif; class sim; run;

proc freq data=CumIncPred_death; tables sim time; run;
proc means data=CumIncPred_death ; var cif; run; /*7515=500+1(original)*15*/
proc means data=CumIncPred_death ; var cif; class sim; run;

*Create permanent datasets;
data outdata.CumIncbaBICuncorrfull_adl; set CumIncPred_adl; run;
data outdata.CumIncbaBICuncorrfull_iadl; set CumIncPred_iadl; run;
data outdata.CumIncbaBICuncorrfull_walk; set CumIncPred_walk; run;
data outdata.CumIncbaBICuncorrfull_death; set CumIncPred_death; run;
*Stack all 4 datasets;
data outdata.CumIncbaBICuncorrfull_all; set CumIncPred_adl CumIncPred_iadl CumIncPred_walk CumIncPred_death; run;

PROC EXPORT DATA=outdata.CumIncbaBICuncorrfull_adl
            OUTFILE= "path\CumIncbaBICuncorrfull_adl.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA=outdata.CumIncbaBICuncorrfull_iadl
            OUTFILE= "path\CumIncbaBICuncorrfull_iadl.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA=outdata.CumIncbaBICuncorrfull_walk
            OUTFILE= "path\CumIncbaBICuncorrfull_walk.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA=outdata.CumIncbaBICuncorrfull_death
            OUTFILE= "path\CumIncbaBICuncorrfull_death.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA=outdata.CumIncbaBICuncorrfull_all
            OUTFILE= "path\CumIncbaBICuncorrfull_all.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




