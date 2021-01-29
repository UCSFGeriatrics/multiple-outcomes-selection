***********************************************************************************************************************************************************************************;
*Program: 3.SAS_baBICbackwardCR                                                                                                                                                    ;                                                               
*Purpose: best average BIC (baBIC) backward elimination using HRS original data set. It uses Cox regression for Death, and Competing-risk regression for rest of outcomes          ;  
*         baBIC=abs(BICk-BICbest)/abs(BICfull-BICbest)  															                                                               ;
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2021.01.28																				                                                                               ;
***********************************************************************************************************************************************************************************;

proc options option=work; run;
%put %sysfunc(pathname(work));
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
options errorabend; /*so the process stops as soon as an error is encountered*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

*****************************************************PREPARE DATA FOR MACROs ********************************************************;

data finaldata; set savedata.originaldata; run; 

*Separate 4 outcomes in 4 datasets;
data adl (rename=(status_adldepdth=status time_adldepdth=time)) ;
 set finaldata(drop=status_iadldifdth status_walkdepdth death time_iadldifdth time_walkdepdth time2death);
 length outcome $16;
 outcome="adl";
run;
data iadl (rename=(status_iadldifdth=status time_iadldifdth=time)) ;
 set finaldata(drop=status_adldepdth status_walkdepdth death time_adldepdth time_walkdepdth time2death);
 length outcome $16;
 outcome="iadl";
run; 
data walk (rename=(status_walkdepdth=status time_walkdepdth=time)) ;
 set finaldata(drop=status_adldepdth status_iadldifdth death time_adldepdth time_iadldifdth time2death);
 length outcome $16;
 outcome="walk";
run; 
data death (rename=(death=status time2death=time)) ;
 set finaldata(drop=status_adldepdth status_iadldifdth status_walkdepdth  time_adldepdth time_iadldifdth time_walkdepdth );
 length outcome $16;
 outcome="death";
run; 

*Stack all 3 datasets for competing-risk;
data compdata;
 set adl iadl walk;
proc sort; by outcome newid; run; /*sort by outcome because this is important to use in BY statement of the PHREG procedure*/
/*5,531*3=16,593 and 59 variables*/
/*QC*/
proc freq data=compdata; tables outcome; run;

proc delete data=finaldata adl iadl walk; run; quit;

sasfile WORK.compdata load; /*load data into memory*/
sasfile WORK.death load;

***************************************************** DEFINE ARGUMENTS FOR MACROS ********************************************************;

*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATACOMP=compdata; /*data for Competing-risk regression*/
%let DATACOX=death;  /*data for Cox regression*/
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

***************************************************** BACKWARD ELIMINATION 4 OUTCOMES **************************************************************;
%macro best_bic;
	%let BASE=%sysfunc(compbl(dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 initial variables*/
	%let DELE=%sysfunc(compbl(ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
			                                   FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	                                           OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
			                                   qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER)); /*all 39 variables-2(AGECAT SEX)=37*/

	/*Run PHREG for the 1st time with all the predictors in the model*/

	/*Competing-risk regression models*/
	proc phreg data = &DATACOMP;
	  by outcome; /*fit 3 regression models, eg.: adl, iadl, walk, one/outcome*/
      class &BASE;
      model time*status(0) = &BASE /eventcode=1;
	  ods output FITSTATISTICS=FITS1;
	  output out=BSOUT xbeta=xb;
    run;
	data BSOUT; 
 	 set BSOUT; 
 	 if status=2 then do; /*the status and time need to be changed here since CONCORDANCE option below doesnt understand status=2*/
  	  status=0;
  	  time=15.0278689;
     end;
    run;
	proc phreg data = BSOUT CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
	  by outcome; /*fit 3 regression models, one/outcome*/
      class &BASE;
      model time*status(0) = &BASE / nofit;
	  roc 'CompRiskC' pred=xb;
	  ods output CONCORDANCE=concord IAUC=iauc;
    run;
	proc delete data=BSOUT; run; quit; 

	/*Cox regression model*/
	proc phreg data = &DATACOX CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc);
	  by outcome; /*fit 1 regression model*/
      class &BASE;
      model time*status(0) = &BASE;
	  ods output FITSTATISTICS=FITS1_cox CONCORDANCE=concord_cox IAUC=iauc_cox;
    run;

	data estimates;
 	 set FITS1 FITS1_cox 
     concord(keep=outcome estimate rename=(estimate=c)) concord_cox(keep=outcome estimate rename=(estimate=c)) 
     iauc(keep=outcome estimate rename=(estimate=iauc)) iauc_cox(keep=outcome estimate rename=(estimate=iauc));
	run;

	data _null_;
	   set savedata.BIC_BestModelsUnion (keep= &BICvars);
	   call symputx (' best_adl'  ,BIC_adl);
	   call symputx (' best_iadl' ,BIC_iadl);
	   call symputx (' best_walk'  ,BIC_walk);
	   call symputx (' best_death'  ,BIC_death);
	run;

	data BASE_BIC (keep= VARINMODEL DELEVAR DELELIST &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars C_avg iauc_avg BIC_avg); 
	  length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
	  set estimates end=last;
	  retain VARINMODEL DELEVAR &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars ; 
	  format &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars 10.4; 
	  VARINMODEL="&BASE";
	  DELEVAR=" ";
	  DELELIST=" "; 
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
    proc delete data=FITS1 FITS1_cox concord concord_cox iauc iauc_cox estimates; run; quit;

	data _null_;
	   set BASE_BIC (keep= &SBCfullvars);
	   call symputx ('full_adl'  ,SBCfull_adl);
	   call symputx ('full_iadl' ,SBCfull_iadl);
	   call symputx ('full_walk'  ,SBCfull_walk);
	   call symputx ('full_death'  ,SBCfull_death);
	run;

   proc append base=BIC data=BASE_BIC(drop=&SBCfullvars) force; run; /*BIC is the final dataset with backward elimination steps from 39 variables to 2*/
   sasfile WORK.BIC load;
   proc delete data=BASE_BIC; run; quit;

	%do i=1 %to &NUMPRED;
	  %DeleteOneVar;
	     data _null_;
	       set AVGCTABLE2 (keep=VARINMODEL DELELIST);
	       call symputx (' BASE ' ,VARINMODEL);
		   call symputx (' DELE ' ,DELELIST);
	     run;

        proc append base=BIC data=AVGCTABLE2; run;
	    proc delete data=AVGCTABLE2; run; quit;
	%end; /*i loop*/
%mend best_bic;

%macro DeleteOneVar;
    %do j=1 %to %eval(&NUMPREDPLUSONE-&i);
        %let DELEVAR=%scan(&DELE,&j); /*select the jth word to delete. &DELE is defined in 'best_bic' macro*/
		%let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/

	/*Competing-risk regression models*/
		proc phreg data = &DATACOMP;
		  by outcome; /*fit 3 regression models, one/outcome*/
	      class &VARNAME;
	      model time*status(0) = &VARNAME /eventcode=1;
		  ods output FITSTATISTICS=FITS1;
		  output out=BSOUT xbeta=xb;
	    run;
		data BSOUT; 
	 	 set BSOUT; 
	 	 if status=2 then do; /*the status and time need to be changed here since CONCORDANCE option below doesnt understand status=2*/
	  	  status=0;
	  	  time=15.0278689;
	     end;
	    run;
		proc phreg data = BSOUT CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
		  by outcome; /*fit 3 regression models, one/outcome*/
	      class &VARNAME;
	      model time*status(0) = &VARNAME / nofit;
		  roc 'CompRiskC' pred=xb;
		  ods output CONCORDANCE=concord IAUC=iauc;
	    run;
		proc delete data=BSOUT; run; quit; 

		/*Cox regression model*/
		proc phreg data = &DATACOX CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc);
		  by outcome; /*fit 1 regression model*/
	      class &VARNAME;
	      model time*status(0) = &VARNAME;
		  ods output FITSTATISTICS=FITS1_cox CONCORDANCE=concord_cox IAUC=iauc_cox;
	    run;

		data estimates;
	 	 set FITS1 FITS1_cox 
	     concord(keep=outcome estimate rename=(estimate=c)) concord_cox(keep=outcome estimate rename=(estimate=c)) 
	     iauc(keep=outcome estimate rename=(estimate=iauc)) iauc_cox(keep=outcome estimate rename=(estimate=iauc));
		run;

		data FITS2 (keep= VARINMODEL DELEVAR DELELIST &Cvars &IAUCvars &AICvars &BICvars C_avg iauc_avg BIC_avg); 
		  length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
		  set estimates end=last;
		  retain VARINMODEL DELEVAR &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars ; 
		  format &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars 10.4; 
		  VARINMODEL="&VARNAME";
		  DELEVAR="&DELEVAR"; /*deleted variable*/
		  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); /*'DELELIST' contain the list of variables that we need to start with in subsequent run*/
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

	    %if &j=1 %then %do; /*in the 1st line of CTABLE_&label create CTABLE_&label dataset*/
	     proc append base=AVGCTABLE data=FITS2 force; run;
	     sasfile WORK.AVGCTABLE load;
	    %end;
	    %else %do; /*for the rest of lines of CTABLE_&label: keep updating CTABLE_&label dataset in memory*/
	     proc append base=AVGCTABLE data=FITS2 force; run;
	    %end; 
	    proc delete data=concord concord_cox iauc iauc_cox FITS1 FITS1_cox estimates FITS2; run; quit;

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

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%best_bic;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
***************************************************** END BACKWARD 4 OUTCOMES ******************************************************************;

sasfile WORK.compdata close;
sasfile WORK.death close;

*Create permanent dataset;
sasfile WORK.BIC close;
data savedata.BICnew; set BIC (drop=DELELIST); run;
proc delete data=BIC; run; quit;

ods select all; /*to print results below*/
ods listing close; /*turn of the output window / "listing" output, so I don't get WARNING: Data too long for column*/
ods csv file="path\Results_baBICnew.csv";
proc print data=savedata.BICnew; run;
ods csv close;

ods listing; /* turn back on the output window / "listing" output*/

