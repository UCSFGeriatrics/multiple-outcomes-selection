***********************************************************************************************************************************************************************************;
*Program: 38.SAS_BICbackwardIndOutcome500bs                                                                                                                                        ;                                                               
*Purpose: Perform BIC backward elimination for individual outcomes in 500 bootstrap samples                                                                                        ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2021.01.28																				                                                                               ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";

options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/
options errorabend; /*so the process stops as soon as an error is encountered*/


/************************************************************************************************************************************************************************************/
/**** ADL ****/

*Merge with original dataset to get the covariates;
data bsample; 
  merge savedata.originaldata (drop=time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) savedata.bs500 (in=A);
  by newid;
  if A;
run;
/*5531*BS=500->2,765,500 observations and 64+1-6=59 variables*/
/*All outcome variables: time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death */

*Modify dataset to apply Wolber modification to Competing-risk regression;
data bsample;
 set bsample; 
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
/* if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;*/
/* if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;*/
proc sort; by replicate newid; run;
/*5531*BS=500->2,765,500 observations and 59 variables*/

/*QC*/
proc freq data=bsample; tables replicate; run;

**************************************************** DEFINE ARGUMENTS FOR MACROS: outcome, best_bic, DeleteOneVar, cstat ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=bsample2;
%let NUMOUTCOMES=1; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth;
%let ALLTIME=time_adldepdth;
%let ALLLABEL= adl; /*labels for outcomes*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/
*Times that takes to collect each of the predictors in the model;
%let ALCOHOLSEC=8.32; %let ARTHRITISSEC=3.18;  ;%let CANCERSEC=13.1; %let COGDLRC3GSEC=78.495; %let COGIMRC3GSEC=78.155;
%let dAGESEC=2.98; %let DIABETESSEC=8.68; %let DRIVESEC=11.55; %let EDUSEC=5.07; %let EXERCISESEC=4.73;
%let EYE2GSEC=27.08; %let FALLSEC=4.04; %let HEARAIDSEC=3.44; %let HEARINGSEC=6.24;  %let HEARTFAILURESEC=3.21;
%let HYPERTENSIONSEC=3.53; %let INCONTINENCESEC=3.63; %let LALONESEC=3.08; %let LUNGSEC=5.71; %let MSTATSEC=2.81;
%let OTHERARMSEC=6.81; %let OTHERCHAIRSEC=4.24; %let OTHERCLIM3GSEC=13.79; %let OTHERDIMESEC=4.5; %let OTHERLIFTSEC=4.69;
%let OTHERPUSHSEC=5.98; %let OTHERSITSEC=4.68; %let OTHERSTOOPSEC=5; %let OTHERWALKSEC=12.4; %let PAINSEC=4.83;
%let CESDALLSEC=48.72; %let qBMISEC=3.21; %let qFAGESEC=4.18; %let qMAGESEC=4.11; %let SEXSEC=3.67;
%let SHLTSEC= 6.63; %let SMOKINGSEC=8.13; %let STROKESEC=7.45; %let VOLUNTEERSEC=3.26;
proc sql noprint; select max(replicate) format 3. into :S from bsample; quit; /*create macro variable with total number of bs datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES **************************************************************;
/*Macro outcome, for each bs dataset and each outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Create a dataset with one line corresponding to the best model for each individual outcome and each bs data
*/

%macro outcome;
 %do i=1 %to &S; /*do this for each bs dataset up to the last bs dataset*/
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE'; quit; /*create macro variable with the first id of ith bs*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE'; quit; /*create macro variable with the last id of ith bs*/

  data bsample2;
   set bsample (FIRSTOBS=&fobs OBS=&lobs);
  run;

  *sasfile WORK.bsample2 load;

  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);
  
	%best_bic;

    *sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label VARSEC=VARSEC_&label)); 
     set BIC_&label (drop=DELELIST DELEVAR);
     replicate=&i;
    run;

	/*For each outcome and each bs data: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
    data BIC_&label;
     set BIC_&label point=nobs nobs=nobs; 
	 output;
	 stop;
    run;

  %end; /*j loop*/

  *sasfile WORK.bsample2 close;
  proc delete data=bsample2; run; quit;

  proc append base=BICrep_&label data=BIC_&label force; run; /*BICrep will have the information from all best individual models for each outcome and for each bs dataset*/
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
	%let BASESEC=433.31; 
	/*433.31 is the initial total time cost when all 39 variables are in the model. As we go from one model to a smaller model the basesec variable will be replaced for a new basesec based on the variables left in the model*/

    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &BASE;
      model &time*&outcome(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
    run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data base_&label (keep=VARINMODEL DELEVAR DELELIST VARSEC AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR VARSEC C_&label iauc_&label AIC_&label BIC_&label; /*add LOGL_&label for time cost selection*/
      format AIC_&label BIC_&label 10.4; /*add LOGL_&label for time cost selection*/
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  VARSEC=&BASESEC;
	  *Uncens=&Uncens;
	  *DF=&DF;
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;
	
    proc delete data=FITS1 concord iauc; run; quit;
    /*proc delete data=FITS1 CensUncens DF concord iauc; run; quit;*/

   proc append base=BIC_&label data=base_&label force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   *sasfile WORK.BIC_&label load;
   proc delete data=base_&label; run; quit;

	%do k=1 %to &NUMPRED;
	  %DeleteOneVar;
      data _null_;
       set CTABLE_&label (keep=VARINMODEL DELELIST VARSEC);
       call symputx ('BASE' ,VARINMODEL);
	   call symputx ('DELE' ,DELELIST);
	   call symputx ('BASESEC', VARSEC);
      run;

      proc append base=BIC_&label data=CTABLE_&label force; run;
	  proc delete data=CTABLE_&label; run; quit;
	%end;
%mend best_bic;

%macro DeleteOneVar;
 	%do l=1 %to %eval(&NUMPREDPLUSONE-&k);
     %let DELEVAR=%scan(&DELE,&l); /*select the lth word to delete. &DELE is defined in 'best_bic' macro*/
	 %let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/
	 %let VARSEC=%sysevalf(&BASESEC-&&&DELEVAR.SEC); /*&&&DELEVAR.SEC: this is to tell sas that the variable delevar.sec keep changing as we delete a different variable each time*/

     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
     run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data FITS2 (keep=VARINMODEL DELEVAR DELELIST VARSEC  AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR VARSEC C_&label iauc_&label AIC_&label BIC_&label; /*add LOGL_&label for time cost selection*/
      format AIC_&label BIC_&label 10.4; /*add LOGL_&label for time cost selection*/
	  VARINMODEL="&VARNAME"; /*variables in the model*/
	  DELEVAR="&DELEVAR"; /*deleted variable*/
	  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); 
	  /*'DELELIST' contain the list of variables that we need to start with in subsequent run. That is, the 2nd time we run macro 'DeleteOneVar' we have 36 variables in macro variable 'DELE',
	  at this step we call it 'DELELIST' and it contains the 'DELE' list minos the variable deleted in the previous run. Later, 'DELELIST' is redefined as 'DELE'*/
	  VARSEC=&VARSEC; /*time cost of reduced model*/
	  *Uncens=&Uncens;
	  *DF=&DF;
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;

     proc append base=CTABLE_&label data=FITS2 force; run;
     *sasfile WORK.CTABLE_&label load;
     proc delete data=FITS1 FITS2 concord iauc; run; quit;
	/*proc delete data=FITS1 FITS2 CensUncens DF concord iauc; run; quit;*/
  %end;
  *sasfile WORK.CTABLE_&label close;

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
data savedata.BICbs_&label; set BICrep_&label; proc sort; by replicate; run;


/************************************************************************************************************************************************************************************/
/**** IADL****/

*Merge with original dataset to get the covariates;
data bsample; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_walkdepdth status_walkdepdth time2death death) savedata.bs500 (in=A);
  by newid;
  if A;
run;
/*5531*BS=500->2,765,500 observations and 64+1-6=59 variables*/
/*All outcome variables: time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death */

*Modify dataset to apply Wolber modification to Competing-risk regression;
data bsample;
 set bsample; 
/* if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;*/
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
/* if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;*/
proc sort; by replicate newid; run;
/*5531*BS=500->2,765,500 observations and 59 variables*/

/*QC*/
proc freq data=bsample; tables replicate; run;

**************************************************** DEFINE ARGUMENTS FOR MACROS: outcome, best_bic, DeleteOneVar, cstat ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=bsample2;
%let NUMOUTCOMES=1; /*number of outcomes*/
%let ALLOUTCOME=status_iadldifdth;
%let ALLTIME=time_iadldifdth;
%let ALLLABEL= iadl; /*labels for outcomes*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/
*Times that takes to collect each of the predictors in the model;
%let ALCOHOLSEC=8.32; %let ARTHRITISSEC=3.18;  ;%let CANCERSEC=13.1; %let COGDLRC3GSEC=78.495; %let COGIMRC3GSEC=78.155;
%let dAGESEC=2.98; %let DIABETESSEC=8.68; %let DRIVESEC=11.55; %let EDUSEC=5.07; %let EXERCISESEC=4.73;
%let EYE2GSEC=27.08; %let FALLSEC=4.04; %let HEARAIDSEC=3.44; %let HEARINGSEC=6.24;  %let HEARTFAILURESEC=3.21;
%let HYPERTENSIONSEC=3.53; %let INCONTINENCESEC=3.63; %let LALONESEC=3.08; %let LUNGSEC=5.71; %let MSTATSEC=2.81;
%let OTHERARMSEC=6.81; %let OTHERCHAIRSEC=4.24; %let OTHERCLIM3GSEC=13.79; %let OTHERDIMESEC=4.5; %let OTHERLIFTSEC=4.69;
%let OTHERPUSHSEC=5.98; %let OTHERSITSEC=4.68; %let OTHERSTOOPSEC=5; %let OTHERWALKSEC=12.4; %let PAINSEC=4.83;
%let CESDALLSEC=48.72; %let qBMISEC=3.21; %let qFAGESEC=4.18; %let qMAGESEC=4.11; %let SEXSEC=3.67;
%let SHLTSEC= 6.63; %let SMOKINGSEC=8.13; %let STROKESEC=7.45; %let VOLUNTEERSEC=3.26;
proc sql noprint; select max(replicate) format 3. into :S from bsample; quit; /*create macro variable with total number of bs datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES **************************************************************;
/*Macro outcome, for each bs dataset and each outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Create a dataset with one line corresponding to the best model for each individual outcome and each bs data
*/

%macro outcome;
 %do i=1 %to &S; /*do this for each bs dataset up to the last bs dataset*/
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE'; quit; /*create macro variable with the first id of ith bs*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE'; quit; /*create macro variable with the last id of ith bs*/

  data bsample2;
   set bsample (FIRSTOBS=&fobs OBS=&lobs);
  run;

  *sasfile WORK.bsample2 load;

  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);
  
	%best_bic;

    *sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label VARSEC=VARSEC_&label)); 
     set BIC_&label (drop=DELELIST DELEVAR);
     replicate=&i;
    run;

	/*For each outcome and each bs data: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
    data BIC_&label;
     set BIC_&label point=nobs nobs=nobs; 
	 output;
	 stop;
    run;

  %end; /*j loop*/

  *sasfile WORK.bsample2 close;
  proc delete data=bsample2; run; quit;

  proc append base=BICrep_&label data=BIC_&label force; run; /*BICrep will have the information from all best individual models for each outcome and for each bs dataset*/
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
	%let BASESEC=433.31; 
	/*433.31 is the initial total time cost when all 39 variables are in the model. As we go from one model to a smaller model the basesec variable will be replaced for a new basesec based on the variables left in the model*/

    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &BASE;
      model &time*&outcome(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
    run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data base_&label (keep=VARINMODEL DELEVAR DELELIST VARSEC AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR VARSEC C_&label iauc_&label AIC_&label BIC_&label; /*add LOGL_&label for time cost selection*/
      format AIC_&label BIC_&label 10.4; /*add LOGL_&label for time cost selection*/
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  VARSEC=&BASESEC;
	  *Uncens=&Uncens;
	  *DF=&DF;
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;
	
    proc delete data=FITS1 concord iauc; run; quit;
    /*proc delete data=FITS1 CensUncens DF concord iauc; run; quit;*/

   proc append base=BIC_&label data=base_&label force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   *sasfile WORK.BIC_&label load;
   proc delete data=base_&label; run; quit;

	%do k=1 %to &NUMPRED;
	  %DeleteOneVar;
      data _null_;
       set CTABLE_&label (keep=VARINMODEL DELELIST VARSEC);
       call symputx ('BASE' ,VARINMODEL);
	   call symputx ('DELE' ,DELELIST);
	   call symputx ('BASESEC', VARSEC);
      run;

      proc append base=BIC_&label data=CTABLE_&label force; run;
	  proc delete data=CTABLE_&label; run; quit;
	%end;
%mend best_bic;

%macro DeleteOneVar;
 	%do l=1 %to %eval(&NUMPREDPLUSONE-&k);
     %let DELEVAR=%scan(&DELE,&l); /*select the lth word to delete. &DELE is defined in 'best_bic' macro*/
	 %let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/
	 %let VARSEC=%sysevalf(&BASESEC-&&&DELEVAR.SEC); /*&&&DELEVAR.SEC: this is to tell sas that the variable delevar.sec keep changing as we delete a different variable each time*/

     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
     run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data FITS2 (keep=VARINMODEL DELEVAR DELELIST VARSEC  AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR VARSEC C_&label iauc_&label AIC_&label BIC_&label; /*add LOGL_&label for time cost selection*/
      format AIC_&label BIC_&label 10.4; /*add LOGL_&label for time cost selection*/
	  VARINMODEL="&VARNAME"; /*variables in the model*/
	  DELEVAR="&DELEVAR"; /*deleted variable*/
	  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); 
	  /*'DELELIST' contain the list of variables that we need to start with in subsequent run. That is, the 2nd time we run macro 'DeleteOneVar' we have 36 variables in macro variable 'DELE',
	  at this step we call it 'DELELIST' and it contains the 'DELE' list minos the variable deleted in the previous run. Later, 'DELELIST' is redefined as 'DELE'*/
	  VARSEC=&VARSEC; /*time cost of reduced model*/
	  *Uncens=&Uncens;
	  *DF=&DF;
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;

     proc append base=CTABLE_&label data=FITS2 force; run;
     *sasfile WORK.CTABLE_&label load;
     proc delete data=FITS1 FITS2 concord iauc; run; quit;
	/*proc delete data=FITS1 FITS2 CensUncens DF concord iauc; run; quit;*/
  %end;
  *sasfile WORK.CTABLE_&label close;

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
data savedata.BICbs_&label; set BICrep_&label; proc sort; by replicate; run;

/************************************************************************************************************************************************************************************/
/**** WALK ****/

*Merge with original dataset to get the covariates;
data bsample; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time2death death) savedata.bs500 (in=A);
  by newid;
  if A;
run;
/*5531*BS=500->2,765,500 observations and 64+1-6=59 variables*/
/*All outcome variables: time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death */

*Modify dataset to apply Wolber modification to Competing-risk regression;
data bsample;
 set bsample; 
/* if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;*/
/* if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;*/
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
proc sort; by replicate newid; run;
/*5531*BS=500->2,765,500 observations and 59 variables*/

/*QC*/
proc freq data=bsample; tables replicate; run;

**************************************************** DEFINE ARGUMENTS FOR MACROS: outcome, best_bic, DeleteOneVar, cstat ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=bsample2;
%let NUMOUTCOMES=1; /*number of outcomes*/
%let ALLOUTCOME=status_walkdepdth;
%let ALLTIME=time_walkdepdth;
%let ALLLABEL= walk; /*labels for outcomes*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/
*Times that takes to collect each of the predictors in the model;
%let ALCOHOLSEC=8.32; %let ARTHRITISSEC=3.18;  ;%let CANCERSEC=13.1; %let COGDLRC3GSEC=78.495; %let COGIMRC3GSEC=78.155;
%let dAGESEC=2.98; %let DIABETESSEC=8.68; %let DRIVESEC=11.55; %let EDUSEC=5.07; %let EXERCISESEC=4.73;
%let EYE2GSEC=27.08; %let FALLSEC=4.04; %let HEARAIDSEC=3.44; %let HEARINGSEC=6.24;  %let HEARTFAILURESEC=3.21;
%let HYPERTENSIONSEC=3.53; %let INCONTINENCESEC=3.63; %let LALONESEC=3.08; %let LUNGSEC=5.71; %let MSTATSEC=2.81;
%let OTHERARMSEC=6.81; %let OTHERCHAIRSEC=4.24; %let OTHERCLIM3GSEC=13.79; %let OTHERDIMESEC=4.5; %let OTHERLIFTSEC=4.69;
%let OTHERPUSHSEC=5.98; %let OTHERSITSEC=4.68; %let OTHERSTOOPSEC=5; %let OTHERWALKSEC=12.4; %let PAINSEC=4.83;
%let CESDALLSEC=48.72; %let qBMISEC=3.21; %let qFAGESEC=4.18; %let qMAGESEC=4.11; %let SEXSEC=3.67;
%let SHLTSEC= 6.63; %let SMOKINGSEC=8.13; %let STROKESEC=7.45; %let VOLUNTEERSEC=3.26;
proc sql noprint; select max(replicate) format 3. into :S from bsample; quit; /*create macro variable with total number of bs datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES **************************************************************;
/*Macro outcome, for each bs dataset and each outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Create a dataset with one line corresponding to the best model for each individual outcome and each bs data
*/

%macro outcome;
 %do i=1 %to &S; /*do this for each bs dataset up to the last bs dataset*/
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE'; quit; /*create macro variable with the first id of ith bs*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE'; quit; /*create macro variable with the last id of ith bs*/

  data bsample2;
   set bsample (FIRSTOBS=&fobs OBS=&lobs);
  run;

  *sasfile WORK.bsample2 load;

  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);
  
	%best_bic;

    *sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label VARSEC=VARSEC_&label)); 
     set BIC_&label (drop=DELELIST DELEVAR);
     replicate=&i;
    run;

	/*For each outcome and each bs data: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
    data BIC_&label;
     set BIC_&label point=nobs nobs=nobs; 
	 output;
	 stop;
    run;

  %end; /*j loop*/

  *sasfile WORK.bsample2 close;
  proc delete data=bsample2; run; quit;

  proc append base=BICrep_&label data=BIC_&label force; run; /*BICrep will have the information from all best individual models for each outcome and for each bs dataset*/
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
	%let BASESEC=433.31; 
	/*433.31 is the initial total time cost when all 39 variables are in the model. As we go from one model to a smaller model the basesec variable will be replaced for a new basesec based on the variables left in the model*/

    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &BASE;
      model &time*&outcome(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
    run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data base_&label (keep=VARINMODEL DELEVAR DELELIST VARSEC AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR VARSEC C_&label iauc_&label AIC_&label BIC_&label; /*add LOGL_&label for time cost selection*/
      format AIC_&label BIC_&label 10.4; /*add LOGL_&label for time cost selection*/
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  VARSEC=&BASESEC;
	  *Uncens=&Uncens;
	  *DF=&DF;
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;
	
    proc delete data=FITS1 concord iauc; run; quit;
    /*proc delete data=FITS1 CensUncens DF concord iauc; run; quit;*/

   proc append base=BIC_&label data=base_&label force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   *sasfile WORK.BIC_&label load;
   proc delete data=base_&label; run; quit;

	%do k=1 %to &NUMPRED;
	  %DeleteOneVar;
      data _null_;
       set CTABLE_&label (keep=VARINMODEL DELELIST VARSEC);
       call symputx ('BASE' ,VARINMODEL);
	   call symputx ('DELE' ,DELELIST);
	   call symputx ('BASESEC', VARSEC);
      run;

      proc append base=BIC_&label data=CTABLE_&label force; run;
	  proc delete data=CTABLE_&label; run; quit;
	%end;
%mend best_bic;

%macro DeleteOneVar;
 	%do l=1 %to %eval(&NUMPREDPLUSONE-&k);
     %let DELEVAR=%scan(&DELE,&l); /*select the lth word to delete. &DELE is defined in 'best_bic' macro*/
	 %let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/
	 %let VARSEC=%sysevalf(&BASESEC-&&&DELEVAR.SEC); /*&&&DELEVAR.SEC: this is to tell sas that the variable delevar.sec keep changing as we delete a different variable each time*/

     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
     run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data FITS2 (keep=VARINMODEL DELEVAR DELELIST VARSEC  AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR VARSEC C_&label iauc_&label AIC_&label BIC_&label; /*add LOGL_&label for time cost selection*/
      format AIC_&label BIC_&label 10.4; /*add LOGL_&label for time cost selection*/
	  VARINMODEL="&VARNAME"; /*variables in the model*/
	  DELEVAR="&DELEVAR"; /*deleted variable*/
	  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); 
	  /*'DELELIST' contain the list of variables that we need to start with in subsequent run. That is, the 2nd time we run macro 'DeleteOneVar' we have 36 variables in macro variable 'DELE',
	  at this step we call it 'DELELIST' and it contains the 'DELE' list minos the variable deleted in the previous run. Later, 'DELELIST' is redefined as 'DELE'*/
	  VARSEC=&VARSEC; /*time cost of reduced model*/
	  *Uncens=&Uncens;
	  *DF=&DF;
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;

     proc append base=CTABLE_&label data=FITS2 force; run;
     *sasfile WORK.CTABLE_&label load;
     proc delete data=FITS1 FITS2 concord iauc; run; quit;
	/*proc delete data=FITS1 FITS2 CensUncens DF concord iauc; run; quit;*/
  %end;
  *sasfile WORK.CTABLE_&label close;

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
data savedata.BICbs_&label; set BICrep_&label; proc sort; by replicate; run;


***************************************************** END BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES ******************************************************************;

/************************************************************************************************************************************************************************************/
/**** DEATH ****/


*Merge with original dataset to get the covariates;
data bsample; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth) savedata.bs500 (in=A);
  by newid;
  if A;
proc sort; by replicate newid; run;
/*5531*BS=500->2,765,500 observations and 64+1-6=59 variables*/
/*All outcome variables: time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death */

/*QC*/
/*proc freq data=bsample; tables replicate; run;*/

**************************************************** DEFINE ARGUMENTS FOR MACROS: outcome, best_bic, DeleteOneVar, cstat ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=bsample2;
%let NUMOUTCOMES=1; /*number of outcomes*/
%let ALLOUTCOME=death;
%let ALLTIME=time2death;
%let ALLLABEL= death; /*labels for outcomes*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/
*Times that takes to collect each of the predictors in the model;
%let ALCOHOLSEC=8.32; %let ARTHRITISSEC=3.18;  ;%let CANCERSEC=13.1; %let COGDLRC3GSEC=78.495; %let COGIMRC3GSEC=78.155;
%let dAGESEC=2.98; %let DIABETESSEC=8.68; %let DRIVESEC=11.55; %let EDUSEC=5.07; %let EXERCISESEC=4.73;
%let EYE2GSEC=27.08; %let FALLSEC=4.04; %let HEARAIDSEC=3.44; %let HEARINGSEC=6.24;  %let HEARTFAILURESEC=3.21;
%let HYPERTENSIONSEC=3.53; %let INCONTINENCESEC=3.63; %let LALONESEC=3.08; %let LUNGSEC=5.71; %let MSTATSEC=2.81;
%let OTHERARMSEC=6.81; %let OTHERCHAIRSEC=4.24; %let OTHERCLIM3GSEC=13.79; %let OTHERDIMESEC=4.5; %let OTHERLIFTSEC=4.69;
%let OTHERPUSHSEC=5.98; %let OTHERSITSEC=4.68; %let OTHERSTOOPSEC=5; %let OTHERWALKSEC=12.4; %let PAINSEC=4.83;
%let CESDALLSEC=48.72; %let qBMISEC=3.21; %let qFAGESEC=4.18; %let qMAGESEC=4.11; %let SEXSEC=3.67;
%let SHLTSEC= 6.63; %let SMOKINGSEC=8.13; %let STROKESEC=7.45; %let VOLUNTEERSEC=3.26;
proc sql noprint; select max(replicate) format 3. into :S from bsample; quit; /*create macro variable with total number of bs datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES **************************************************************;
/*Macro outcome, for each bs dataset and each outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Create a dataset with one line corresponding to the best model for each individual outcome and each bs data
*/

%macro outcome;
 %do i=1 %to &S; /*do this for each bs dataset up to the last bs dataset*/
  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE'; quit; /*create macro variable with the first id of ith bs*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE'; quit; /*create macro variable with the last id of ith bs*/

  data bsample2;
   set bsample (FIRSTOBS=&fobs OBS=&lobs);
  run;

  *sasfile WORK.bsample2 load;

  %PUT "&i" ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
  %do j=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&j);
	%let LABEL=%scan(&ALLLABEL,&j);
  
	%best_bic;

    *sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label VARSEC=VARSEC_&label)); 
     set BIC_&label (drop=DELELIST DELEVAR);
     replicate=&i;
    run;

	/*For each outcome and each bs data: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
    data BIC_&label;
     set BIC_&label point=nobs nobs=nobs; 
	 output;
	 stop;
    run;

  %end; /*j loop*/

  *sasfile WORK.bsample2 close;
  proc delete data=bsample2; run; quit;

  proc append base=BICrep_&label data=BIC_&label force; run; /*BICrep will have the information from all best individual models for each outcome and for each bs dataset*/
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
	%let BASESEC=433.31; 
	/*433.31 is the initial total time cost when all 39 variables are in the model. As we go from one model to a smaller model the basesec variable will be replaced for a new basesec based on the variables left in the model*/

    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &BASE;
      model &time*&outcome(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
    run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data base_&label (keep=VARINMODEL DELEVAR DELELIST VARSEC AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR VARSEC C_&label iauc_&label AIC_&label BIC_&label; /*add LOGL_&label for time cost selection*/
      format AIC_&label BIC_&label 10.4; /*add LOGL_&label for time cost selection*/
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  VARSEC=&BASESEC;
	  *Uncens=&Uncens;
	  *DF=&DF;
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;
	
    proc delete data=FITS1 concord iauc; run; quit;
    /*proc delete data=FITS1 CensUncens DF concord iauc; run; quit;*/

   proc append base=BIC_&label data=base_&label force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   *sasfile WORK.BIC_&label load;
   proc delete data=base_&label; run; quit;

	%do k=1 %to &NUMPRED;
	  %DeleteOneVar;
      data _null_;
       set CTABLE_&label (keep=VARINMODEL DELELIST VARSEC);
       call symputx ('BASE' ,VARINMODEL);
	   call symputx ('DELE' ,DELELIST);
	   call symputx ('BASESEC', VARSEC);
      run;

      proc append base=BIC_&label data=CTABLE_&label force; run;
	  proc delete data=CTABLE_&label; run; quit;
	%end;
%mend best_bic;

%macro DeleteOneVar;
 	%do l=1 %to %eval(&NUMPREDPLUSONE-&k);
     %let DELEVAR=%scan(&DELE,&l); /*select the lth word to delete. &DELE is defined in 'best_bic' macro*/
	 %let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/
	 %let VARSEC=%sysevalf(&BASESEC-&&&DELEVAR.SEC); /*&&&DELEVAR.SEC: this is to tell sas that the variable delevar.sec keep changing as we delete a different variable each time*/

     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
      class &VARNAME;
      model &time*&outcome(0) = &VARNAME;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
     run;

	data _null_; set concord; call symputx ('c' ,Estimate); run;
	data _null_; set iauc; call symputx ('iauc' ,Estimate); run;

	data FITS2 (keep=VARINMODEL DELEVAR DELELIST VARSEC  AIC_&label BIC_&label C_&label iauc_&label); 
      length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
      set FITS1 end=last;
      retain VARINMODEL DELEVAR VARSEC C_&label iauc_&label AIC_&label BIC_&label; /*add LOGL_&label for time cost selection*/
      format AIC_&label BIC_&label 10.4; /*add LOGL_&label for time cost selection*/
	  VARINMODEL="&VARNAME"; /*variables in the model*/
	  DELEVAR="&DELEVAR"; /*deleted variable*/
	  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); 
	  /*'DELELIST' contain the list of variables that we need to start with in subsequent run. That is, the 2nd time we run macro 'DeleteOneVar' we have 36 variables in macro variable 'DELE',
	  at this step we call it 'DELELIST' and it contains the 'DELE' list minos the variable deleted in the previous run. Later, 'DELELIST' is redefined as 'DELE'*/
	  VARSEC=&VARSEC; /*time cost of reduced model*/
	  *Uncens=&Uncens;
	  *DF=&DF;
	  C_&label=&c;
	  iauc_&label=&iauc;
	  if CRITERION='AIC' then AIC_&label=WITHCOVARIATES;
	  if CRITERION='SBC' then BIC_&label=WITHCOVARIATES;
      if last;
     run;

     proc append base=CTABLE_&label data=FITS2 force; run;
     *sasfile WORK.CTABLE_&label load;
     proc delete data=FITS1 FITS2 concord iauc; run; quit;
	/*proc delete data=FITS1 FITS2 CensUncens DF concord iauc; run; quit;*/
  %end;
  *sasfile WORK.CTABLE_&label close;

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
data savedata.BICbs_&label; set BICrep_&label; proc sort; by replicate; run;


***************************************************** END BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES ******************************************************************;


