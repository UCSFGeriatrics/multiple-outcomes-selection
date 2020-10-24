***********************************************************************************************************************************************************************************;
*Program: 39.SAS_baBICbackward500bs                                                                                                                                                ;                                                               
*Purpose: baBIC backward elimination in 500 bootstrap samples                                                                                                                      ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2020.10.21																				                                                                               ;
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
options nothreads; /*to avoid Insufficient space error in Utility files*/
options errorabend; /*so the process stops as soon as an error is encountered*/

*****************************************************PREPARE DATA FOR MACROs ********************************************************;

*Merge with original dataset to get the covariates;
data bsample; 
  merge savedata.originaldata savedata.bs500 (in=A);
  by newid;
  if A;
proc sort; by replicate newid; run;
/*5531*BS=500->2,765,500 observations and 64+1=65 variables*/

*Modify dataset to apply Wolber modification to Competing-risk regression;
data bsample;
 set bsample; 
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
proc sort; by replicate newid; run;
/*5531*BS=500->2,765,500 observations and 65 variables*/

*Separate 4 outcomes in 4 datasets;
data adl (rename=(status_adldepdth=status time_adldepdth=time)) ;
 set bsample(drop=status_iadldifdth status_walkdepdth death time_iadldifdth time_walkdepdth time2death);
 length outcome $16;
 outcome="adl";
run; /*2,765,500 observations and 65-6+1=60 variables.*/
data iadl (rename=(status_iadldifdth=status time_iadldifdth=time)) ;
 set bsample(drop=status_adldepdth status_walkdepdth death time_adldepdth time_walkdepdth time2death);
 length outcome $16;
 outcome="iadl";
run; /*2,765,500 observations and 60 variables.*/
data walk (rename=(status_walkdepdth=status time_walkdepdth=time)) ;
 set bsample(drop=status_adldepdth status_iadldifdth death time_adldepdth time_iadldifdth time2death);
 length outcome $16;
 outcome="walk";
run; /*2,765,500 observations and 60 variables.*/
data death (rename=(death=status time2death=time)) ;
 set bsample(drop=status_adldepdth status_iadldifdth status_walkdepdth  time_adldepdth time_iadldifdth time_walkdepdth );
 length outcome $16;
 outcome="death";
run; /*2,765,500 observations and 60 variables.*/

*Stack all 4 datasets;
data bsample2;
 set adl iadl walk death;
proc sort; by replicate outcome newid; run;
/*2,765,500*4=11,062,000 and 60 variables*/

/*QC*/
/*proc freq data=bsample2; tables outcome replicate; run;*/

proc delete data=adl iadl walk death bsample; run; quit;

***************************************************** DEFINE ARGUMENTS FOR MACROS ********************************************************;
*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=bsample3;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLLABELS= adl iadl walk death; /*labels for outcomes*/
%let Cvars=C_adl C_iadl C_walk C_death; /*name of variables for the C-stat*/
%let IAUCvars=iauc_adl iauc_iadl iauc_walk iauc_death; /*name of variables for the IAUC stat*/
%let AICvars=AIC_adl AIC_iadl AIC_walk AIC_death; /*name of variables for the AIC stat*/
%let BICvars=BIC_adl BIC_iadl BIC_walk BIC_death; /*name of variables for the BIC stat*/
%let SBCfullvars=SBCfull_adl SBCfull_iadl SBCfull_walk SBCfull_death; /*name of variables for the SBC full stat*/
%let SBCbestvars=SBCbest_adl SBCbest_iadl SBCbest_walk SBCbest_death; /*name of variables for the SBC best stat*/
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
proc sql noprint; select max(replicate) format 3. into :S from bsample2; quit; /*create macro variable with total number of bs datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 OUTCOMES **************************************************************;
/*Macro sim, for each bootstrap dataset:
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Selects the 'best model' and stack results in one common dataset
*/

%macro sim;
 %do l=1 %to &S; /*do this for each bs dataset up to the last bs dataset*/
     proc sql noprint; select (&l-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the first id of lth bs data*/
     proc sql noprint; select &l*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the last id of lth bs data*/

     data bsample3;
	 	set bsample2 (FIRSTOBS=&fobs OBS=&lobs);
	 proc sort; by outcome; run;

	 sasfile WORK.bsample3 load;

	%PUT "&l" ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
	%best_bic;
	%PUT "&l" ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

	data BIC_&l; set BIC (drop=DELELIST); replicate=&l; run;
	sasfile WORK.BIC close;
    sasfile WORK.bsample3 close; 
	proc delete data=bsample3 BIC; run; quit;

	proc sort data=BIC_&l; by descending BIC_avg; run;
	data BIC_&l;
	 set BIC_&l point=nobs nobs=nobs;
	 output;
	 stop;
	run; 

    %if &l=1 %then %do; /*in the 1st bs data(l=1) create BICrep dataset*/
      proc append base=BICrep data=BIC_&l force; run; /*BICrep will have the information from all best individual models for each outcome and for each bs dataset*/
      sasfile WORK.BICrep load;
    %end;
    %else %do; /*for the rest of the bs data: keep updating BICrep dataset in memory*/
      proc append base=BICrep data=BIC_&l force; run;
    %end; 
	proc delete data=BIC_&l; run; quit;

 %end; /*l loop*/

%mend sim;

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

	/*Run PHREG for the 1st time with all the predictors in the model*/
	proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc);
	  by outcome; /*fit 4 regression models, one/outcome*/
      class &BASE;
      model time*status(0) = &BASE;
	  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
	  /*ods output FITSTATISTICS=FITS1 CensoredSummary=CensUncens GlobalTests=DF CONCORDANCE=concord IAUC=iauc;*/
	  /*CensoredSummary if Cox, FailureSummary if Competing-risks*/
    run;

	data estimates;
 	 set FITS1 concord(keep=outcome estimate rename=(estimate=c)) iauc(keep=outcome estimate rename=(estimate=iauc));
	run;

	data _null_;
	   set savedata.bicbs500by2 (keep= replicate &BICvars);
	   where replicate=&l;
	   call symputx (' best_adl'  ,BIC_adl);
	   call symputx (' best_iadl' ,BIC_iadl);
	   call symputx (' best_walk'  ,BIC_walk);
	   call symputx (' best_death'  ,BIC_death);
	run;

	data BASE_BIC (keep= VARINMODEL DELEVAR DELELIST VARSEC &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars C_avg iauc_avg BIC_avg); 
	  length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
	  set estimates end=last;
	  retain VARINMODEL DELEVAR VARSEC &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars ; /*add &LOGLvars for time cost selection*/
	  format &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars 10.4; /*add &LOGLvars for time cost selection*/
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
	  VARSEC=&BASESEC;
	  array label{&NUMOUTCOMES} &ALLLABELS;
	  array Cs{&NUMOUTCOMES} &Cvars;
	  array IAUCs{&NUMOUTCOMES} &IAUCvars;
	  array AICs{&NUMOUTCOMES} &AICvars;
	  array BICs{&NUMOUTCOMES} &BICvars;
	  array SBCfull{&NUMOUTCOMES} &SBCfullvars;
	  array SBCbest{&NUMOUTCOMES} &SBCbestvars;
	  array best{&NUMOUTCOMES} (&best_adl &best_iadl &best_walk &best_death);
	  do k=1 to &NUMOUTCOMES;
	   SBCbest{k}=best{k};
	   if outcome=vname(label{k}) then do;
	    if c ne . then Cs{k}=c;
	    else if iauc ne . then IAUCs{k}=iauc;
		else if CRITERION='AIC' then AICs{k}=WITHCOVARIATES;
		else if CRITERION='SBC' then do;
		 SBCfull{k}=WITHCOVARIATES;
	     BICs{k}=abs(WITHCOVARIATES-SBCbest{k})/abs(SBCfull{k}-SBCbest{k});
		end; 
	   end;
	  end; /*k loop*/
	 if last;
	 C_avg=mean(of &Cvars);
	 iauc_avg=mean(of &IAUCvars);
	 BIC_avg=mean(of &BICvars);
	run;
	proc delete data=FITS1 concord iauc estimates; run; quit;
    /*proc delete data=FITS1 CensUncens DF concord iauc estimates; run; quit;*/

	data _null_;
	   set BASE_BIC (keep= &SBCfullvars);
	   call symputx ('full_adl'  ,SBCfull_adl);
	   call symputx ('full_iadl' ,SBCfull_iadl);
	   call symputx ('full_walk' ,SBCfull_walk);
	   call symputx ('full_death' ,SBCfull_death);
	run;

   proc append base=BIC data=BASE_BIC(drop=&SBCfullvars) force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   sasfile WORK.BIC load;
   proc delete data=BASE_BIC; run; quit;

	%do i=1 %to &NUMPRED;
	  %DeleteOneVar;
	     data _null_;
	       set AVGCTABLE2 (keep=VARINMODEL DELELIST VARSEC);
	       call symputx (' BASE ' ,VARINMODEL);
		   call symputx (' DELE ' ,DELELIST);
		   call symputx (' BASESEC', VARSEC);
	     run;

        proc append base=BIC data=AVGCTABLE2; run;
	    proc delete data=AVGCTABLE2; run; quit;
	%end; /*i loop*/
%mend best_bic;

%macro DeleteOneVar;
    %do j=1 %to %eval(&NUMPREDPLUSONE-&i);
        %let DELEVAR=%scan(&DELE,&j); /*select the jth word to delete. &DELE is defined in 'best_bic' macro*/
		%let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/
		%let VARSEC=%sysevalf(&BASESEC-&&&DELEVAR.SEC); /*&&&DELEVAR.SEC: this is to tell sas that the variable delevar.sec keep changing as we delete a different variable each time*/

		proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc);
		  by outcome; /*fit 4 regression models, one/outcome*/
	      class &VARNAME;
	      model time*status(0) = &VARNAME;
		  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
		  /*ods output FITSTATISTICS=FITS1 CensoredSummary=CensUncens GlobalTests=DF CONCORDANCE=concord IAUC=iauc;*/
		  /*CensoredSummary if Cox, FailureSummary if Competing-risks*/
	    run;

		data estimates;
	 	 set FITS1 concord(keep=outcome estimate rename=(estimate=c)) iauc(keep=outcome estimate rename=(estimate=iauc));
		run;

		data FITS2 (keep= VARINMODEL DELEVAR DELELIST VARSEC &Cvars &IAUCvars &AICvars &BICvars C_avg iauc_avg BIC_avg); 
		  length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
		  set estimates end=last;
		  retain VARINMODEL DELEVAR VARSEC &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars ; /*add &LOGLvars for time cost selection*/
		  format &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars 10.4; /*add &LOGLvars for time cost selection*/
		  VARINMODEL="&VARNAME";
		  DELEVAR="&DELEVAR"; /*deleted variable*/
		  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); 
		  /*'DELELIST' contain the list of variables that we need to start with in subsequent run. That is, the 2nd time we run macro 'DeleteOneVar' we have 36 variables in macro variable 'DELE',
		  at this step we call it 'DELELIST' and it contains the 'DELE' list minos the variable deleted in the previous run. Later, 'DELELIST' is redefined as 'DELE'*/
		  VARSEC=&VARSEC;/*time cost of reduced model*/
		  array label{&NUMOUTCOMES} &ALLLABELS;
		  array Cs{&NUMOUTCOMES} &Cvars;
		  array IAUCs{&NUMOUTCOMES} &IAUCvars;
		  array AICs{&NUMOUTCOMES} &AICvars;
		  array BICs{&NUMOUTCOMES} &BICvars;
		  array SBCfull{&NUMOUTCOMES} &SBCfullvars;
		  array full{&NUMOUTCOMES} (&full_adl &full_iadl &full_walk &full_death);
		  array SBCbest{&NUMOUTCOMES} &SBCbestvars;
		  array best{&NUMOUTCOMES} (&best_adl &best_iadl &best_walk &best_death);
		  do k=1 to &NUMOUTCOMES;
		   SBCbest{k}=best{k};
		   SBCfull{k}=full{k};
		   if outcome=vname(label{k}) then do;
		    if c ne . then Cs{k}=c;
		    else if iauc ne . then IAUCs{k}=iauc;
			else if CRITERION='AIC' then AICs{k}=WITHCOVARIATES;
		    else if CRITERION='SBC' then BICs{k}=abs(WITHCOVARIATES-SBCbest{k})/abs(SBCfull{k}-SBCbest{k});
		   end;
		  end; /*k loop*/
		 if last;
		 C_avg=mean(of &Cvars);
		 iauc_avg=mean(of &IAUCvars);
		 BIC_avg=mean(of &BICvars);     
		run;

	    %if &j=1 %then %do; /*in the 1st line of AVGCTABLE create AVGCTABLE dataset*/
	     proc append base=AVGCTABLE data=FITS2 force; run;
	     sasfile WORK.AVGCTABLE load;
	    %end;
	    %else %do; /*for the rest of lines of AVGCTABLE: keep updating AVGCTABLE dataset in memory*/
	     proc append base=AVGCTABLE data=FITS2 force; run;
	    %end; 
	    proc delete data=concord iauc FITS1 estimates FITS2; run; quit;
		/*proc delete data=FITS1 estimates FITS2 CensUncens DF concord iauc; run; quit;*/
	  %end; /*j loop*/
	sasfile WORK.AVGCTABLE close;

   proc sort data= AVGCTABLE; by descending BIC_avg; run;

   data AVGCTABLE2;
    set AVGCTABLE point=nobs nobs=nobs;
    output;
    stop;
   run; /*choose the last observation that has minimum BIC*/

   proc delete data= AVGCTABLE; run; quit;
%mend DeleteOneVar;

%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%sim;
%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
***************************************************** END BACKWARD 4 OUTCOMES ******************************************************************;

/*Create permanent dataset with the best (i.e. minimum BIC) combined model for each bs data*/
data savedata.BICrep&S; set BICrep; run;
sasfile WORK.BICrep close; 
proc delete data=BICrep; run; quit;


