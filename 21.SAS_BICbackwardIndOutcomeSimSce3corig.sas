*************************************************************************************************************************************************************************************************;
*Program: 21.SAS_BICbackwardIndOutcomeSimSce3corig                                                                                                                                               ;                                                               
*Purpose: BIC backward elimination for individual outcomes for simulations of Scenario 3 with case-study censoring and training data. Obtain Union and Intersection model for each simulation    ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                                             ;
*Finished: 2021.01.28																				                                                                                             ;
*************************************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/


*****************************************************PREPARE DATA FOR MACROs ********************************************************;

proc sort data=savedata.sim500trainfullcorig out=simdata; by newid; run;

*Merge with original dataset to get the covariates;
data simdata2; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) 
        simdata;
  by newid;
proc sort; by sim outcome newid; run;
/*5531*SIM=500->2,765,500*4outcomes=11,062,000 observations and 56+6-1=61 variables.*/

*ADL;
data simdata3;
 set simdata2 (rename=(time=time_adldepdth status=status_adldepdth));
 where outcome="adl";
proc sort; by sim newid; run; /*2,765,500 observations and 60 variables*/

proc delete data=simdata simdata2; run; quit;


**************************************************** DEFINE ARGUMENTS FOR MACROS: outcome, best_bic, DeleteOneVar, cstat ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=simdata4;
%let NUMOUTCOMES=1; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth;
%let ALLTIME=time_adldepdth;
%let ALLLABEL= adl; /*labels for outcomes*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/
proc sql noprint; select max(sim) format 3. into :S from simdata3; quit; /*create macro variable with total number of simulated datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES: ADL **************************************************************;
/*Macro outcome, for each simulated dataset and each outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Create a dataset with one line corigesponding to the best model for each individual outcome and each simulation
*/

%macro outcome;
 %do i=1 %to &S; /*do this for each simulated dataset up to the last simulated dataset*/
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the first id of ith simulation*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the last id of ith simulation*/

  data simdata4;
   set simdata3 (FIRSTOBS=&fobs OBS=&lobs);
  run;

  sasfile WORK.simdata4 load;

  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);
  
	%best_bic;

    sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label)); 
         set BIC_&label (drop=DELELIST DELEVAR);
         sim=&i;
        run;

	/*For each outcome and each simulation: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
       data BIC_&label;
        set BIC_&label point=nobs nobs=nobs; 
	 output;
	 stop;
       run;

  %end; /*j loop*/

  sasfile WORK.simdata4 close;
  proc delete data=simdata4; run; quit;

  proc append base=BICsim_&label data=BIC_&label force; run; /*BICsim will have the information from all best individual models for each outcome and for each simulated dataset*/
  proc delete data=BIC_&label; run; quit;
  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

 %end; /*i loop*/
%mend outcome;

%macro best_bic;
	%let BASE=%sysfunc(compbl(dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 initial variables*/
	%let DELE=%sysfunc(compbl(ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 variables-2(AGECAT SEX)=37*/

    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &BASE;
      model &time*&outcome(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
    run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data base_&label (keep=VARINMODEL DELEVAR DELELIST AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR C_&label iauc_&label AIC_&label BIC_&label; 
      format AIC_&label BIC_&label 10.4; 
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;
	
    proc delete data=FITS1 concord iauc; run; quit;

   proc append base=BIC_&label data=base_&label force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   sasfile WORK.BIC_&label load;
   proc delete data=base_&label; run; quit;

	%do k=1 %to &NUMPRED;
	  %DeleteOneVar;
      data _null_;
       set CTABLE_&label (keep=VARINMODEL DELELIST);
       call symputx ('BASE' ,VARINMODEL);
	   call symputx ('DELE' ,DELELIST);
      run;

      proc append base=BIC_&label data=CTABLE_&label force; run;
	  proc delete data=CTABLE_&label; run; quit;
	%end;
%mend best_bic;

%macro DeleteOneVar;
 	%do l=1 %to %eval(&NUMPREDPLUSONE-&k);
     %let DELEVAR=%scan(&DELE,&l); /*select the lth word to delete. &DELE is defined in 'best_bic' macro*/
	 %let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/

     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
     run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data FITS2 (keep=VARINMODEL DELEVAR DELELIST AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR C_&label iauc_&label AIC_&label BIC_&label; 
      format AIC_&label BIC_&label 10.4; 
	  VARINMODEL="&VARNAME"; /*variables in the model*/
	  DELEVAR="&DELEVAR"; /*deleted variable*/
	  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); /*'DELELIST' contain the list of variables that we need to start with in subsequent run*/
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;

    %if &l=1 %then %do; /*in the 1st line of CTABLE_&label create CTABLE_&label dataset*/
     proc append base=CTABLE_&label data=FITS2 force; run;
     sasfile WORK.CTABLE_&label load;
    %end;
    %else %do; /*for the rest of lines of CTABLE_&label: keep updating CTABLE_&label dataset in memory*/
     proc append base=CTABLE_&label data=FITS2 force; run;
    %end; 
    proc delete data=FITS1 FITS2 concord iauc; run; quit;
	/*proc delete data=FITS1 FITS2 CensUncens DF concord iauc; run; quit;*/
  %end;
  sasfile WORK.CTABLE_&label close;

  proc sort data=CTABLE_&label; by descending BIC_&label; run;
  data CTABLE_&label;
   set CTABLE_&label point=nobs nobs=nobs;
   output;
   stop;
  run; /*choose the last observation with minimum BIC*/
%mend DeleteOneVar;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%outcome;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

*Create permanent dataset;
%let LABEL= adl; /*labels for outcomes*/
data savedata.BICsim_corig_&label; set BICsim_&label; proc sort; by sim; run;
proc delete data=BICsim_&label; run; quit;
***************************************************** END BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES: ADL ******************************************************************;


*****************************************************PREPARE DATA FOR MACROs ********************************************************;
proc sort data=savedata.sim500trainfullcorig out=simdata; by newid; run; 

*Merge with original dataset to get the covariates;
data simdata2; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) 
        simdata;
  by newid;
proc sort; by sim outcome newid; run;
/*5531*SIM=500->2,765,500*4outcomes=11,062,000 observations and 56+6-1=61 variables.*/

*IADL;
data simdata3;
 set simdata2 (rename=(time=time_iadldifdth status=status_iadldifdth));
 where outcome="iadl";
proc sort; by sim newid; run; /*2,765,500 observations and 60 variables*/

proc delete data=simdata simdata2; run; quit;


**************************************************** DEFINE ARGUMENTS FOR MACROS: outcome, best_bic, DeleteOneVar, cstat ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=simdata4;
%let NUMOUTCOMES=1; /*number of outcomes*/
%let ALLOUTCOME=status_iadldifdth;
%let ALLTIME=time_iadldifdth;
%let ALLLABEL= iadl; /*labels for outcomes*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/
proc sql noprint; select max(sim) format 3. into :S from simdata3; quit; /*create macro variable with total number of simulated datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES: IADL **************************************************************;
/*Macro outcome, for each simulated dataset and each outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Create a dataset with one line corigesponding to the best model for each individual outcome and each simulation
*/

%macro outcome;
 %do i=1 %to &S; /*do this for each simulated dataset up to the last simulated dataset*/
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the first id of ith simulation*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the last id of ith simulation*/

  data simdata4;
   set simdata3 (FIRSTOBS=&fobs OBS=&lobs);
  run;

  sasfile WORK.simdata4 load;

  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);
  
	%best_bic;

    sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label)); 
     set BIC_&label (drop=DELELIST DELEVAR);
     sim=&i;
    run;

	/*For each outcome and each simulation: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
    data BIC_&label;
     set BIC_&label point=nobs nobs=nobs; 
	 output;
	 stop;
    run;

  %end; /*j loop*/

  sasfile WORK.simdata4 close;
  proc delete data=simdata4; run; quit;

  proc append base=BICsim_&label data=BIC_&label force; run; /*BICsim will have the information from all best individual models for each outcome and for each simulated dataset*/
  proc delete data=BIC_&label; run; quit;
  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

 %end; /*i loop*/
%mend outcome;

%macro best_bic;
	%let BASE=%sysfunc(compbl(dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 initial variables*/
	%let DELE=%sysfunc(compbl(ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 variables-2(AGECAT SEX)=37*/

    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &BASE;
      model &time*&outcome(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
    run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data base_&label (keep=VARINMODEL DELEVAR DELELIST AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR C_&label iauc_&label AIC_&label BIC_&label; 
      format AIC_&label BIC_&label 10.4; 
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;
	
    proc delete data=FITS1 concord iauc; run; quit;

   proc append base=BIC_&label data=base_&label force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   sasfile WORK.BIC_&label load;
   proc delete data=base_&label; run; quit;

	%do k=1 %to &NUMPRED;
	  %DeleteOneVar;
      data _null_;
       set CTABLE_&label (keep=VARINMODEL DELELIST);
       call symputx ('BASE' ,VARINMODEL);
	   call symputx ('DELE' ,DELELIST);
      run;

      proc append base=BIC_&label data=CTABLE_&label force; run;
	  proc delete data=CTABLE_&label; run; quit;
	%end;
%mend best_bic;

%macro DeleteOneVar;
 	%do l=1 %to %eval(&NUMPREDPLUSONE-&k);
     %let DELEVAR=%scan(&DELE,&l); /*select the lth word to delete. &DELE is defined in 'best_bic' macro*/
	 %let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/

     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
     run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data FITS2 (keep=VARINMODEL DELEVAR DELELIST AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR C_&label iauc_&label AIC_&label BIC_&label; 
      format AIC_&label BIC_&label 10.4; 
	  VARINMODEL="&VARNAME"; /*variables in the model*/
	  DELEVAR="&DELEVAR"; /*deleted variable*/
	  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); /*'DELELIST' contain the list of variables that we need to start with in subsequent run.*/
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;

    %if &l=1 %then %do; /*in the 1st line of CTABLE_&label create CTABLE_&label dataset*/
     proc append base=CTABLE_&label data=FITS2 force; run;
     sasfile WORK.CTABLE_&label load;
    %end;
    %else %do; /*for the rest of lines of CTABLE_&label: keep updating CTABLE_&label dataset in memory*/
     proc append base=CTABLE_&label data=FITS2 force; run;
    %end; 
    proc delete data=FITS1 FITS2 concord iauc; run; quit;
	/*proc delete data=FITS1 FITS2 CensUncens DF concord iauc; run; quit;*/
  %end;
  sasfile WORK.CTABLE_&label close;

  proc sort data=CTABLE_&label; by descending BIC_&label; run;
  data CTABLE_&label;
   set CTABLE_&label point=nobs nobs=nobs;
   output;
   stop;
  run; /*choose the last observation with minimum BIC*/
%mend DeleteOneVar;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%outcome;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

*Create permanent dataset;
%let LABEL= iadl; /*labels for outcomes*/
data savedata.BICsim_corig_&label; set BICsim_&label; proc sort; by sim; run;
proc delete data=BICsim_&label; run; quit;
***************************************************** END BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES: IADL ******************************************************************;


*****************************************************PREPARE DATA FOR MACROs ********************************************************;
proc sort data=savedata.sim500trainfullcorig out=simdata; by newid; run; 

*Merge with original dataset to get the covariates;
data simdata2; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) 
        simdata;
  by newid;
proc sort; by sim outcome newid; run;
/*5531*SIM=500->2,765,500*4outcomes=11,062,000 observations and 56+6-1=61 variables.*/

*Walk;
data simdata3;
 set simdata2 (rename=(time=time_walkdepdth status=status_walkdepdth));
 where outcome="walk";
proc sort; by sim newid; run; /*2,765,500 observations and 60 variables*/

proc delete data=simdata simdata2; run; quit;


**************************************************** DEFINE ARGUMENTS FOR MACROS: outcome, best_bic, DeleteOneVar, cstat ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=simdata4;
%let NUMOUTCOMES=1; /*number of outcomes*/
%let ALLOUTCOME=status_walkdepdth;
%let ALLTIME=time_walkdepdth;
%let ALLLABEL= walk; /*labels for outcomes*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/
proc sql noprint; select max(sim) format 3. into :S from simdata3; quit; /*create macro variable with total number of simulated datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES: WALK **************************************************************;
/*Macro outcome, for each simulated dataset and each outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Create a dataset with one line corigesponding to the best model for each individual outcome and each simulation
*/

%macro outcome;
 %do i=1 %to &S; /*do this for each simulated dataset up to the last simulated dataset*/
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the first id of ith simulation*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the last id of ith simulation*/

  data simdata4;
   set simdata3 (FIRSTOBS=&fobs OBS=&lobs);
  run;

  sasfile WORK.simdata4 load;

  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);
  
	%best_bic;

    sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label ); 
     set BIC_&label (drop=DELELIST DELEVAR);
     sim=&i;
    run;

	/*For each outcome and each simulation: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
    data BIC_&label;
     set BIC_&label point=nobs nobs=nobs; 
	 output;
	 stop;
    run;

  %end; /*j loop*/

  sasfile WORK.simdata4 close;
  proc delete data=simdata4; run; quit;

  proc append base=BICsim_&label data=BIC_&label force; run; /*BICsim will have the information from all best individual models for each outcome and for each simulated dataset*/
  proc delete data=BIC_&label; run; quit;
  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

 %end; /*i loop*/
%mend outcome;

%macro best_bic;
	%let BASE=%sysfunc(compbl(dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 initial variables*/
	%let DELE=%sysfunc(compbl(ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 variables-2(AGECAT SEX)=37*/

    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &BASE;
      model &time*&outcome(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
    run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data base_&label (keep=VARINMODEL DELEVAR DELELIST AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR C_&label iauc_&label AIC_&label BIC_&label; 
      format AIC_&label BIC_&label 10.4; 
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;
	
    proc delete data=FITS1 concord iauc; run; quit;

   proc append base=BIC_&label data=base_&label force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   sasfile WORK.BIC_&label load;
   proc delete data=base_&label; run; quit;

	%do k=1 %to &NUMPRED;
	  %DeleteOneVar;
      data _null_;
       set CTABLE_&label (keep=VARINMODEL DELELIST );
       call symputx ('BASE' ,VARINMODEL);
	   call symputx ('DELE' ,DELELIST);
      run;

      proc append base=BIC_&label data=CTABLE_&label force; run;
	  proc delete data=CTABLE_&label; run; quit;
	%end;
%mend best_bic;

%macro DeleteOneVar;
 	%do l=1 %to %eval(&NUMPREDPLUSONE-&k);
     %let DELEVAR=%scan(&DELE,&l); /*select the lth word to delete. &DELE is defined in 'best_bic' macro*/
	 %let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/

     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
     run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data FITS2 (keep=VARINMODEL DELEVAR DELELIST  AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR C_&label iauc_&label AIC_&label BIC_&label; 
      format AIC_&label BIC_&label 10.4; 
	  VARINMODEL="&VARNAME"; /*variables in the model*/
	  DELEVAR="&DELEVAR"; /*deleted variable*/
	  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); /*'DELELIST' contain the list of variables that we need to start with in subsequent run.*/
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;

    %if &l=1 %then %do; /*in the 1st line of CTABLE_&label create CTABLE_&label dataset*/
     proc append base=CTABLE_&label data=FITS2 force; run;
     sasfile WORK.CTABLE_&label load;
    %end;
    %else %do; /*for the rest of lines of CTABLE_&label: keep updating CTABLE_&label dataset in memory*/
     proc append base=CTABLE_&label data=FITS2 force; run;
    %end; 
    proc delete data=FITS1 FITS2 concord iauc; run; quit;
	/*proc delete data=FITS1 FITS2 CensUncens DF concord iauc; run; quit;*/
  %end;
  sasfile WORK.CTABLE_&label close;

  proc sort data=CTABLE_&label; by descending BIC_&label; run;
  data CTABLE_&label;
   set CTABLE_&label point=nobs nobs=nobs;
   output;
   stop;
  run; /*choose the last observation with minimum BIC*/
%mend DeleteOneVar;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%outcome;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

*Create permanent dataset;
%let LABEL= walk; /*labels for outcomes*/
data savedata.BICsim_corig_&label; set BICsim_&label; proc sort; by sim; run;
proc delete data=BICsim_&label; run; quit;
***************************************************** END BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES: WALK ******************************************************************;


*****************************************************PREPARE DATA FOR MACROs ********************************************************;
proc sort data=savedata.sim500trainfullcorig out=simdata; by newid; run; 

*Merge with original dataset to get the covariates;
data simdata2; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) 
        simdata;
  by newid;
proc sort; by sim outcome newid; run;
/*5531*SIM=500->2,765,500*4outcomes=11,062,000 observations and 56+6-1=61 variables.*/

*Death;
data simdata3;
 set simdata2 (rename=(time=time2death status=death));
 where outcome="death";
proc sort; by sim newid; run; /*2,765,500 observations and 60 variables*/

proc delete data=simdata simdata2; run; quit;


**************************************************** DEFINE ARGUMENTS FOR MACROS: outcome, best_bic, DeleteOneVar, cstat ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=simdata4;
%let NUMOUTCOMES=1; /*number of outcomes*/
%let ALLOUTCOME=death;
%let ALLTIME=time2death;
%let ALLLABEL= death; /*labels for outcomes*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/
proc sql noprint; select max(sim) format 3. into :S from simdata3; quit; /*create macro variable with total number of simulated datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES: DEATH **************************************************************;
/*Macro outcome, for each simulated dataset and each outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Create a dataset with one line corigesponding to the best model for each individual outcome and each simulation
*/

%macro outcome;
 %do i=1 %to &S; /*do this for each simulated dataset up to the last simulated dataset*/
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the first id of ith simulation*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='SIMDATA3'; quit; /*create macro variable with the last id of ith simulation*/

  data simdata4;
   set simdata3 (FIRSTOBS=&fobs OBS=&lobs);
  run;

  sasfile WORK.simdata4 load;

  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);
  
	%best_bic;

    sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label )); 
     set BIC_&label (drop=DELELIST DELEVAR);
     sim=&i;
    run;

	/*For each outcome and each simulation: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
    data BIC_&label;
     set BIC_&label point=nobs nobs=nobs; 
	 output;
	 stop;
    run;

  %end; /*j loop*/

  sasfile WORK.simdata4 close;
  proc delete data=simdata4; run; quit;

  proc append base=BICsim_&label data=BIC_&label force; run; /*BICsim will have the information from all best individual models for each outcome and for each simulated dataset*/
  proc delete data=BIC_&label; run; quit;
  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

 %end; /*i loop*/
%mend outcome;

%macro best_bic;
	%let BASE=%sysfunc(compbl(dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 initial variables*/
	%let DELE=%sysfunc(compbl(ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 variables-2(AGECAT SEX)=37*/

    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &BASE;
      model &time*&outcome(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
    run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data base_&label (keep=VARINMODEL DELEVAR DELELIST  AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR  C_&label iauc_&label AIC_&label BIC_&label; 
      format AIC_&label BIC_&label 10.4; 
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;
	
    proc delete data=FITS1 concord iauc; run; quit;

   proc append base=BIC_&label data=base_&label force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   sasfile WORK.BIC_&label load;
   proc delete data=base_&label; run; quit;

	%do k=1 %to &NUMPRED;
	  %DeleteOneVar;
      data _null_;
       set CTABLE_&label (keep=VARINMODEL DELELIST );
       call symputx ('BASE' ,VARINMODEL);
	   call symputx ('DELE' ,DELELIST);
      run;

      proc append base=BIC_&label data=CTABLE_&label force; run;
	  proc delete data=CTABLE_&label; run; quit;
	%end;
%mend best_bic;

%macro DeleteOneVar;
 	%do l=1 %to %eval(&NUMPREDPLUSONE-&k);
     %let DELEVAR=%scan(&DELE,&l); /*select the lth word to delete. &DELE is defined in 'best_bic' macro*/
	 %let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/

     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
     run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data FITS2 (keep=VARINMODEL DELEVAR DELELIST   AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR  C_&label iauc_&label AIC_&label BIC_&label; 
      format AIC_&label BIC_&label 10.4; 
	  VARINMODEL="&VARNAME"; /*variables in the model*/
	  DELEVAR="&DELEVAR"; /*deleted variable*/
	  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); /*'DELELIST' contain the list of variables that we need to start with in subsequent run.*/
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;

    %if &l=1 %then %do; /*in the 1st line of CTABLE_&label create CTABLE_&label dataset*/
     proc append base=CTABLE_&label data=FITS2 force; run;
     sasfile WORK.CTABLE_&label load;
    %end;
    %else %do; /*for the rest of lines of CTABLE_&label: keep updating CTABLE_&label dataset in memory*/
     proc append base=CTABLE_&label data=FITS2 force; run;
    %end; 
    proc delete data=FITS1 FITS2 concord iauc; run; quit;
  %end;
  sasfile WORK.CTABLE_&label close;

  proc sort data=CTABLE_&label; by descending BIC_&label; run;
  data CTABLE_&label;
   set CTABLE_&label point=nobs nobs=nobs;
   output;
   stop;
  run; /*choose the last observation with minimum BIC*/
%mend DeleteOneVar;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%outcome;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

*Create permanent dataset;
%let LABEL= death; /*labels for outcomes*/
data savedata.BICsim_corig_&label; set BICsim_&label; proc sort; by sim; run;
proc delete data=BICsim_&label; run; quit;
***************************************************** END BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES: DEATH ******************************************************************;


/***********************************************************************************************************************************************************************************/
/***********************************************************************************************************************************************************************************/
/*Get Union and Intersection Model and compute summary statistics*/


/*Merge all 4 datasets with the best individual model within each outcome for each simulated dataset*/
data BICsim;
   merge savedata.BICsim_corig_adl savedata.BICsim_corig_iadl savedata.BICsim_corig_walk savedata.BICsim_corig_death;
   by sim;
run;

data bicsimind_Sce3corig  (drop=delims i x: n i temp);
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
proc means data=bicsimind_Sce3corig stackodsoutput n mean stderr clm median p25 p75 maxdec=4; 
 var numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath numVarsfinsim_union numVarsfinsim_inters C_adl C_iadl C_walk C_death iauc_adl iauc_iadl iauc_walk iauc_death VARSEC_adl VARSEC_iadl VARSEC_walk VARSEC_death ;
 ods output summary=bicsimind_Sce3corig_stats(rename=(Mean=Mean_sim_Ind));
proc sort; by variable; run;

PROC EXPORT DATA= bicsimind_Sce3corig_stats
            OUTFILE= "path\bicsimind_Sce3corig_stats.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
