***********************************************************************************************************************************************************************************;
*Program: 46a.SAS_CumIncSce3corig                                                                                                                                                  ;                                                               
*Purpose: Compute Cumulative Incidence for simulations of Scenario 3 with case-study censoring and training data                                                                   ;                                     
*Statistician: Grisell Diaz-Ramirez																																				   ;
*Finished: 2021.01.28																																							   ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
libname outdata "path";
libname outtrain "path";

*****************************************************PREPARE DATA FOR MACRO ********************************************************;

%let traindata=outtrain.sim500trainFullcorig;

**************** Create dummy variables for categorical predictors: see example in SASGeneral folder (DummyVariables.doc) ************;
data finaldata;
 set savedata.originaldata ;
proc sort; by newid; run;
/* 5531 observations and 64 variables */

/*
Full model:dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
		   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
		   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER

Full model dummy: age2-age10 SEX alcoh2 alcoh3 ARTHRITIS canc2 canc3 cogdl2 cogdl3 cogim2 cogim3 DIABETES DRIVE EDU EXERCISE EYE2G
                  fal2 fal3 HEARAID hear2-hear5 HEARTFAILURE HYPERTENSION INCONTINENCE LALONE lun2 lun3 MSTAT OTHERARM
                  OTHERCHAIR climb2 climb3 OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP walk2 walk3 PAIN CESDALL
                  bmi2-bmi5 father2-father4 mother2-mother4 health2-health5 smoke2 smoke3 strok2 strok3 VOLUNTEER

Full model: dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE COGDLRC3G SMOKING OTHERCLIM3G HEARTFAILURE  LUNG OTHERPUSH qBMI VOLUNTEER
Full dummy model: age2-age10 SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE cogdl2 cogdl3 smoke2 smoke3 climb2 climb3 HEARTFAILURE  lun2 lun3 OTHERPUSH bmi2-bmi5 VOLUNTEER

ADL model: dAGE SEX DRIVE INCONTINENCE EDU DIABETES  EXERCISE  OTHERARM OTHERLIFT OTHERSTOOP
ADL dummy model: age2-age10 SEX DRIVE INCONTINENCE EDU DIABETES  EXERCISE  OTHERARM OTHERLIFT OTHERSTOOP

IADL model: dAGE SEX DRIVE INCONTINENCE EDU OTHERSIT COGDLRC3G SMOKING HEARAID
IADL dummy: age2-age10 SEX DRIVE INCONTINENCE EDU OTHERSIT cogdl2 cogdl3 smoke2 smoke3 HEARAID


WALK model: dAGE SEX DRIVE INCONTINENCE OTHERSIT HYPERTENSION OTHERCLIM3G
WALK dummy model: age2-age10 SEX DRIVE INCONTINENCE OTHERSIT HYPERTENSION climb2 climb3

DEATH model: dAGE SEX DRIVE DIABETES EXERCISE COGDLRC3G SMOKING HYPERTENSION OTHERCLIM3G HEARTFAILURE  LUNG MSTAT  OTHERPUSH OTHERWALK qBMI VOLUNTEER
DEATH dummy model: age2-age10 SEX DRIVE DIABETES EXERCISE cogdl2 cogdl3 smoke2 smoke3 HYPERTENSION climb2 climb3 HEARTFAILURE lun2 lun3 MSTAT  OTHERPUSH walk3 walk3 bmi2-bmi5 VOLUNTEER

*/

proc freq data=finaldata; 
 tables dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
		   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
		   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER;
run;
*16 Categorical predictors: dAGE(10 levels) ALCOHOL(3)  CANCER(3) COGDLRC3G (0,1,2) COGIMRC3G (0,1,2) FALL(3) HEARING(5) LUNG(3) OTHERCLIM3G(3)
                         OTHERWALK(3) qBMI(5) qFAGE(4) qMAGE(4) SHLT(5) SMOKING(3) STROKE(3);

data finaldata;
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

	set finaldata;

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

*QC;
proc freq data=finaldata; tables dAGE*age1*age2*age3*age4*age5*age6*age7*age8*age9*age10 /list; run;
proc freq data=finaldata; tables ALCOHOL*alcoh1*alcoh2*alcoh3 /list; run;
proc freq data=finaldata; tables CANCER*canc1*canc2*canc3 /list; run;
proc freq data=finaldata; tables COGDLRC3G*cogdl1*cogdl2*cogdl3 /list; run;
proc freq data=finaldata; tables COGIMRC3G*cogim1*cogim2*cogim3 /list; run;
proc freq data=finaldata; tables FALL*fal1*fal2*fal3 /list; run;
proc freq data=finaldata; tables HEARING*hear1*hear2*hear3*hear4*hear5 /list; run;
proc freq data=finaldata; tables lung*lun1*lun2*lun3 /list; run;
proc freq data=finaldata; tables OTHERCLIM3G*climb1*climb2*climb3 /list; run;
proc freq data=finaldata; tables OTHERWALK*walk1*walk2*walk3 /list; run;
proc freq data=finaldata; tables qBMI*bmi1*bmi2*bmi3*bmi4*bmi5 /list; run;
proc freq data=finaldata; tables qFAGE*father1*father2*father3*father4 /list; run;
proc freq data=finaldata; tables qMAGE*mother1*mother2*mother3*mother4 /list; run;
proc freq data=finaldata; tables SHLT*health1*health2*health3*health4*health5 /list; run;
proc freq data=finaldata; tables SMOKING*smoke1*smoke2*smoke3 /list; run;
proc freq data=finaldata; tables STROKE*strok1*strok2*strok3 /list; run;


*Leave only id and covariates in finaldata2;
data finaldata2; 
 set finaldata(keep=newid age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT cogdl1 cogdl2 smoke1 smoke2
                             HYPERTENSION climb1 climb2 OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE lun1 lun2 MSTAT
                             OTHERPUSH walk1 walk2 bmi1 bmi2 bmi3 bmi4 VOLUNTEER alcoh1 alcoh2 ARTHRITIS canc1 canc2 cogim1 cogim2 EYE2G
                             fal1 fal2 hear1 hear2 hear3 hear4 LALONE OTHERCHAIR OTHERDIME PAIN CESDALL  father1 father2 father3 mother1 mother2 mother3
                             health1 health2 health3 health4 strok1 strok2);
proc sort; by newid; run;
/*5531 observations and 71 variables.*/


/**************** Get simulated data ************/

proc contents data=finaldata2; run;

proc sort data=&traindata out=simdata; by newid; run;/*11062000 observations and 6 variables.*/

*Merge with original dataset to get the covariates;
data simdata2; 
  merge finaldata2 simdata;
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

***************************************************** Compute Cumulative Incidence for each simulated data using the Full model from case-study data ******************************************************************;

*Define macro variables;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth status_iadldifdth status_walkdepdth death;
%let ALLTIME=time_adldepdth time_iadldifdth time_walkdepdth time2death;
%let ALLLABEL= adl iadl walk death; /*labels for outcomes*/
proc sql noprint; select max(sim) format 3. into :S from simdata3; quit; /*create macro variable with total number of simulated datasets*/
%put "&S";
 
%let VARNAME=age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT cogdl1 cogdl2 smoke1 smoke2
                             HYPERTENSION climb1 climb2 OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE lun1 lun2 MSTAT
                             OTHERPUSH walk1 walk2 bmi1 bmi2 bmi3 bmi4 VOLUNTEER alcoh1 alcoh2 ARTHRITIS canc1 canc2 cogim1 cogim2 EYE2G
                             fal1 fal2 hear1 hear2 hear3 hear4 LALONE OTHERCHAIR OTHERDIME PAIN CESDALL  father1 father2 father3 mother1 mother2 mother3
                             health1 health2 health3 health4 strok1 strok2 ;

*Covariate data only with predictors in Full model;
data covdata_means;
 set outdata.covdata_means (keep=age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT cogdl1 cogdl2 smoke1 smoke2
                             HYPERTENSION climb1 climb2 OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE lun1 lun2 MSTAT
                             OTHERPUSH walk1 walk2 bmi1 bmi2 bmi3 bmi4 VOLUNTEER alcoh1 alcoh2 ARTHRITIS canc1 canc2 cogim1 cogim2 EYE2G
                             fal1 fal2 hear1 hear2 hear3 hear4 LALONE OTHERCHAIR OTHERDIME PAIN CESDALL  father1 father2 father3 mother1 mother2 mother3
                             health1 health2 health3 health4 strok1 strok2 );
run;
proc means data=finaldata mean min max; 
 var age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT cogdl1 cogdl2 smoke1 smoke2
                             HYPERTENSION climb1 climb2 OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE lun1 lun2 MSTAT
                             OTHERPUSH walk1 walk2 bmi1 bmi2 bmi3 bmi4 VOLUNTEER alcoh1 alcoh2 ARTHRITIS canc1 canc2 cogim1 cogim2 EYE2G
                             fal1 fal2 hear1 hear2 hear3 hear4 LALONE OTHERCHAIR OTHERDIME PAIN CESDALL  father1 father2 father3 mother1 mother2 mother3
                             health1 health2 health3 health4 strok1 strok2 ;
run;

*Status;
proc freq data=finaldata; tables status_adldepdth status_iadldifdth status_walkdepdth death;; run;
 
*Maximum time-to-event;
proc means data=finaldata mean min max; 
 var time_adldepdth time_iadldifdth time_walkdepdth time2death;
run; /*time_walkdepdth, max=15.0278689, rest of outcomes: adl(14.9759563), iadl(14.9349727), death (14.9945280)*/

*Test Comp-risk ADL;
proc phreg data = finaldata;  
  model time_adldepdth*status_adldepdth(0) = &VARNAME /eventcode=1 ;
  baseline out=CumIncPred(keep=time_adldepdth cif) covariates=covdata_means CIF=cif timelist=0 to 14 by 1 ;
run;

options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

%macro CumInc;

*Fit models in original data;
proc phreg data = finaldata;  
  model time_adldepdth*status_adldepdth(0) = &VARNAME /eventcode=1 ;
  baseline out=CumIncPred_adl(keep=time_adldepdth cif rename=(time_adldepdth=time) ) covariates=covdata_means CIF=cif timelist=0 to 14 by 1 ;
run;
proc phreg data = finaldata;  
  model time_iadldifdth*status_iadldifdth(0) = &VARNAME /eventcode=1 ;
  baseline out=CumIncPred_iadl(keep=time_iadldifdth cif rename=(time_iadldifdth=time) ) covariates=covdata_means CIF=cif timelist=0 to 14 by 1 ;
run;
proc phreg data = finaldata;  
  model time_walkdepdth*status_walkdepdth(0) = &VARNAME /eventcode=1 ;
  baseline out=CumIncPred_walk(keep=time_walkdepdth cif rename=(time_walkdepdth=time) ) covariates=covdata_means CIF=cif timelist=0 to 14 by 1 ;
run;
proc phreg data = finaldata;  
  model time2death*death(0) = &VARNAME ;
  baseline out=CumIncPred_death(keep=time2death s rename=(time2death=time) ) covariates=covdata_means survival=s timelist=0 to 14 by 1 ;
run;

data CumIncPred_adl; set CumIncPred_adl; length outcome $8; outcome="adl"; sim=0; run;
data CumIncPred_iadl; set CumIncPred_iadl; length outcome $8; outcome="iadl"; sim=0; run;
data CumIncPred_walk; set CumIncPred_walk; length outcome $8; outcome="walk"; sim=0; run;
data CumIncPred_death (drop=s); set CumIncPred_death; length outcome $8; cif=1-s; outcome="death";  sim=0; run;


 %do i=1 %to &S;
 /*Fit the Full model on each simulated dataset to obtain the CumInc */

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

*Compute mean of cif adl;
proc means data=Cumincpred_adl stackodsoutput mean maxdec=4; 
 var cif;
 class time;
 where sim ne 0;
 ods output summary=cif_means_adl;
run;
data cif_means_adl;
 length outcome $8;
 set cif_means_adl (keep=time mean rename=(mean=cif));
 sim=501; /* this is the mean across 500 500 simulations */
 outcome='adl';
run;
*Stack CumIncPred_adl cif_means_adl;
data Cumincpred_adl;
 set Cumincpred_adl cif_means_adl;
run;
/*7530=500+1(original)+1(mean)*15*/

*Compute mean of cif iadl;
proc means data=Cumincpred_iadl stackodsoutput mean maxdec=4; 
 var cif;
 class time;
 where sim ne 0;
 ods output summary=cif_means_iadl;
run;
data cif_means_iadl;
 length outcome $8;
 set cif_means_iadl (keep=time mean rename=(mean=cif));
 sim=501; /* this is the mean across 500 500 simulations */
 outcome='iadl';
run;
*Stack CumIncPred_iadl cif_means_iadl;
data Cumincpred_iadl;
 set Cumincpred_iadl cif_means_iadl;
run;

*Compute mean of cif walk;
proc means data=Cumincpred_walk stackodsoutput mean maxdec=4; 
 var cif;
 class time;
 where sim ne 0;
 ods output summary=cif_means_walk;
run;
data cif_means_walk;
 length outcome $8;
 set cif_means_walk (keep=time mean rename=(mean=cif));
 sim=501; /* this is the mean across 500 500 simulations */
 outcome='walk';
run;
*Stack CumIncPred_walk cif_means_walk;
data Cumincpred_walk;
 set Cumincpred_walk cif_means_walk;
run;

*Compute mean of cif death;
proc means data=Cumincpred_death stackodsoutput mean maxdec=4; 
 var cif;
 class time;
 where sim ne 0;
 ods output summary=cif_means_death;
run;
data cif_means_death;
 length outcome $8;
 set cif_means_death (keep=time mean rename=(mean=cif));
 sim=501; /* this is the mean across 500 500 simulations */
 outcome='death';
run;
*Stack CumIncPred_death cif_means_death;
data Cumincpred_death;
 set Cumincpred_death cif_means_death;
run;

*QC;
ods select all;
proc freq data=CumIncPred_adl; tables sim time; run;
proc means data=CumIncPred_adl ; var cif; run; /*7530 maximum possible of data points=500+1(original)+2(mean)*15*/
proc means data=CumIncPred_adl ; var cif; class sim; run;

proc freq data=CumIncPred_iadl; tables sim time; run;
proc means data=CumIncPred_iadl ; var cif; run; 
proc means data=CumIncPred_iadl ; var cif; class sim; run;

proc freq data=CumIncPred_walk; tables sim time; run;
proc means data=CumIncPred_walk ; var cif; run; 
proc means data=CumIncPred_walk ; var cif; class sim; run;

proc freq data=CumIncPred_death; tables sim time; run;
proc means data=CumIncPred_death ; var cif; run;
proc means data=CumIncPred_death ; var cif; class sim; run;

*Create permanent datasets;
data outdata.CumIncsce3corig_adl; set CumIncPred_adl; run;
data outdata.CumIncsce3corig_iadl; set CumIncPred_iadl; run;
data outdata.CumIncsce3corig_walk; set CumIncPred_walk; run;
data outdata.CumIncsce3corig_death; set CumIncPred_death; run;
*Stack all 4 datasets;
data outdata.CumIncsce3corig_all; set CumIncPred_adl CumIncPred_iadl CumIncPred_walk CumIncPred_death; run;

PROC EXPORT DATA=outdata.CumIncsce3corig_adl
            OUTFILE= "path\CumIncsce3corig_adl.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA=outdata.CumIncsce3corig_iadl
            OUTFILE= "path\CumIncsce3corig_iadl.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA=outdata.CumIncsce3corig_walk
            OUTFILE= "path\CumIncsce3corig_walk.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA=outdata.CumIncsce3corig_death
            OUTFILE= "path\CumIncsce3corig_death.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA=outdata.CumIncsce3corig_all
            OUTFILE= "path\CumIncsce3corig_all.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




