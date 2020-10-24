***********************************************************************************************************************************************************************************;
*Program: 18.SAS_baBICbackwardSimSce2Corr                                                                                                                                          ;                                                               
*Purpose: baBIC backward elimination for Scenario 2 simulated training correlated data                                                                                             ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2020.04.24																				                                                                               ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
libname savedata "path";
options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

*****************************************************PREPARE DATA FOR MACROs ********************************************************;

proc sort data=savedata.sim500corrbyTrain out=simdata; by newid; run; 

*Merge with original dataset to get the covariates;
data simdata2; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) 
        simdata;
  by newid;
proc sort; by sim outcome newid; run;
/*5531*SIM=500->2,765,500*4outcomes=11,062,000 observations and 56+5-1=60 variables.*/
proc delete data=simdata; run; quit;
proc freq data=simdata2; tables sim outcome; run;


***************************************************** DEFINE ARGUMENTS FOR MACROS ********************************************************;
*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=simdata3;
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
proc sql noprint; select max(sim) format 3. into :S from simdata2; quit; /*create macro variable with total number of simulated datasets*/
%put "&S"; 

***************************************************** BACKWARD ELIMINATION 4 OUTCOMES **************************************************************;
/*Macro sim, for each simulated dataset:
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) Creates one dataset with results from backward elimination
3-) Selects the 'best model' and stack results in one common dataset
*/

%macro sim;
 %do l=1 %to &S; /*do this for each simulated dataset up to the last simulated dataset*/
     proc sql noprint; select (&l-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='SIMDATA2'; quit; /*create macro variable with the first id of lth simulation*/
     proc sql noprint; select &l*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='SIMDATA2'; quit; /*create macro variable with the last id of lth simulation*/

     data simdata3;
	 	set simdata2 (FIRSTOBS=&fobs OBS=&lobs);
	 proc sort; by outcome; run;

	 sasfile WORK.simdata3 load;

	%PUT "&l" ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
	%best_bic;
	%PUT "&l" ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

	data BIC_&l; set BIC (drop=DELELIST); sim=&l; run;
	sasfile WORK.BIC close;
    sasfile WORK.simdata3 close; 
	proc delete data=simdata3 BIC; run; quit;

	proc sort data=BIC_&l; by descending BIC_avg; run;
	data BIC_&l;
	 set BIC_&l point=nobs nobs=nobs;
	 output;
	 stop;
	run; 

    %if &l=1 %then %do; /*in the 1st simulation(l=1) create BICsim dataset*/
      proc append base=BICsim data=BIC_&l force; run; /*BICsim will have the information from all best individual models for each outcome and for each simulated dataset*/
      sasfile WORK.BICsim load;
    %end;
    %else %do; /*for the rest of the simulations: keep updating BICsim dataset in memory*/
      proc append base=BICsim data=BIC_&l force; run;
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
	   set savedata.bicsim500corrby (keep= sim &BICvars);
	   where sim=&l;
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

		data FITS2 (keep= VARINMODEL DELEVAR DELELIST &Cvars &IAUCvars &AICvars &BICvars C_avg iauc_avg BIC_avg); 
		  length VARINMODEL $ 600 DELELIST $600 DELEVAR $ 60;
		  set estimates end=last;
		  retain VARINMODEL DELEVAR &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars ; 
		  format &Cvars &IAUCvars &AICvars &BICvars &SBCfullvars &SBCbestvars 10.4; 
		  VARINMODEL="&VARNAME";
		  DELEVAR="&DELEVAR"; /*deleted variable*/
		  DELELIST=compbl(tranwrd("&DELE","&DELEVAR",' ')); 
		  /*'DELELIST' contain the list of variables that we need to start with in subsequent run. That is, the 2nd time we run macro 'DeleteOneVar' we have 36 variables in macro variable 'DELE',
		  at this step we call it 'DELELIST' and it contains the 'DELE' list minos the variable deleted in the previous run. Later, 'DELELIST' is redefined as 'DELE'*/
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

/*Create permanent dataset with the best (i.e. minimum BIC) combined model for each simulation*/
data savedata.baBICsim&S.corrSce2; set BICsim; run;
sasfile WORK.BICsim close; 
proc delete data=BICsim; run; quit;


/***********************************************************************************************************************************************/
*Compare variables in original best model vs variables in best model from each simulated dataset;

*Define some macro variables;

*Full model;
proc sort data=savedata.BICnew out=bic_full; by descending DELEVAR ; run; /*last obs will have the variables with full model since DELEVAR is missing*/
data bic_full; set bic_full(keep=VARINMODEL rename=(VARINMODEL=varsfull)) end=last; if last=1; run; 
data _null_;
 set bic_full;
 delims = ' ';                 /*delimiter: space*/
 numVarsfull = countw(varsfull, delims);  /*for line of text, how many words/variables?*/
call symputx ('numVarsfull' ,numVarsfull);
run; 
%put "&numVarsfull";

*baBIC model;
proc sort data=savedata.BICnew out=bic_final; by descending BIC_avg ; data bic_final; set bic_final end=last; if last=1; run; 
data _null_; set bic_final; call symputx ('varsbaBIC' ,VARINMODEL); run; 
data _null_; set bic_final; call symputx ('C_mean' ,C_avg); run; 
data _null_; set bic_final; call symputx ('IAUC_mean' ,iauc_avg); run; 
data _null_; set bic_final; call symputx ('BIC_mean' ,BIC_avg); run; 
data _null_; set bic_final; call symputx ('C_adlfinal' ,C_adl); run; 
data _null_; set bic_final; call symputx ('C_iadlfinal' ,C_iadl); run; 
data _null_; set bic_final; call symputx ('C_walkfinal' ,C_walk); run; 
data _null_; set bic_final; call symputx ('C_deathfinal' ,C_death); run; 

data _null_;
 set bic_final;
 delims = ' ';                 /*delimiter: space*/
 numVarsbaBIC = countw(VARINMODEL, delims);  /*for line of text, how many words/variables?*/
 call symputx ('numVarsbaBIC' ,numVarsbaBIC);
run; 
%put "&numVarsbaBIC";
%put "&varsbaBIC";

*Create a number of "varbaBIC" variables that have as character values the name of the variables that are in the original baBIC model;
data varfindata;
 set bic_final (keep=VARINMODEL rename=(VARINMODEL=varsbaBIC)); 
 numVarsbaBIC=&numVarsbaBIC;
 delims = ' ';                       /*delimiter: space*/
 array varbaBIC [&numVarsbaBIC] $20. ; /*e.g. if the number of variable in the baBIC model is 15, there are 15 variables: varbaBIC1-varbaBIC15 created*/
 do i=1 to &numVarsbaBIC;            
   varbaBIC[i] = scan(varsbaBIC, i, delims); /* split text into words/variables */
 end;
 drop delims i;
 run;
proc print data=varfindata; run;

*Union model;
data _null;
 set savedata.BIC_BestModelsUnion;
 delims=' ';
 numVarsUnion = countw(jointvars, delims);  /*for line of text, how many words/variables?*/
 call symputx ('numVarsUnion' ,numVarsUnion);
 call symputx ('varsUnion' ,jointvars);
run; 
%put "&numVarsUnion";
%put "&varsUnion";

*All the variables in the baBIC model are in the Union model;
%put "&varsUnion";
%put "&varsbaBIC";

%let numVarsNoUnion=%eval(&numVarsfull-&numVarsUnion);
%put "&numVarsNoUnion";

*Create a number of "varUnion" variables that have as character values the name of the variables in the Union model;
data varfindata2;
	merge varfindata savedata.BIC_BestModelsUnion (keep=jointvars rename=(jointvars=varsUnion));
	delims = ' ';                       /*delimiter: space*/
	numVarsUnion=&numVarsUnion;
	array varUnion [&numVarsUnion] $20. ; /*e.g. if the number of variable in the Union model is 23, there are 23 variables: varUnion1-varUnion23 created*/
	do i=1 to &numVarsUnion;            
	   varUnion[i] = scan(varsUnion, i, delims); /* split text into words/variables */
	end;
	drop delims i;
run;
proc print data=varfindata2; run;

*Create a number of "varNoUnion" variables that have as character values the name of the variables that are NOT in the Union model;
data varfindata3;
 merge bic_full varfindata2;
 delims = ' ';   
 varsNoUnion=varsfull; 
 array varUnion [&numVarsUnion] ;
 do i=1 to &numVarsUnion;
 	varsNoUnion=tranwrd(varsNoUnion, strip(varUnion[i]),' '); /*TRANWRD(character-value, from-string, to-string): replace variables in Union with blank: variables left are not in the Union model*/
 end;
 drop delims i;
 varsNoUnion=compbl(varsNoUnion);
 numVarsNoUnion=&numVarsNoUnion;
run;
proc print data=varfindata3; run;
data varfindata4;
 set varfindata3 (drop=varsfull);
 delims = ' ';                 
 array varNoUnion [&numVarsNoUnion] $20.; /*e.g. if the number of variable that are NOT in the Union model is 16, there are 16 variables: varNoUnion1-varNoUnion16 created*/
 do i=1 to &numVarsNoUnion;            
   varNoUnion[i] = scan(varsNoUnion, i, delims);
 end;
 drop delims i;
run;
proc print data=varfindata4; run;
data _null_; set varfindata4; call symputx ('varsNoUnion' ,varsNoUnion); run; 
%put "&varsNoUnion";
%put "&varsUnion";

*Create indicator variables that contain whether a variable in the Union/baBIC model is present in each of the simulated models and
indicator variables that record whether a variable that is NOT in the Union model is present in each simulated model;
data BICsim2;
 if _N_=1 then set varfindata4; 
 set savedata.baBICsim&S.corrSce2 (keep=sim VARINMODEL C_avg iauc_avg BIC_avg C_adl C_iadl C_walk C_death);

 delims = ' ';   
 numVarsfinsim = countw(VARINMODEL, delims);

 array varUnion [&numVarsUnion] ;
 array Uniony[&numVarsUnion] &varsUnion;
 do i=1 to &numVarsUnion;
  if find(VARINMODEL,strip(varUnion[i]),'i')>0 then Uniony[i]=1; /*i: ignore case.*/
  else Uniony[i]=0; 
 end;

 array varNoUnion[&numVarsNoUnion] ;
 array varn[&numVarsNoUnion] &varsNoUnion;
 do i=1 to &numVarsNoUnion;
  if find(VARINMODEL,strip(varNoUnion[i]),'i')>0 then varn[i]=1; 
  else varn[i]=0; 
 end;

 anyVarNoUnion=max(of &varsNoUnion);
 sumVarNoUnion=sum(of &varsNoUnion);

 array varbaBIC [&numVarsbaBIC] ;
 array baBICy [&numVarsbaBIC] ;
 do i=1 to &numVarsbaBIC;
  if find(VARINMODEL,strip(varbaBIC[i]),'i')>0 then baBICy[i]=1; /*i: ignore case.*/
  else baBICy[i]=0; 
 end;

 sumbaBIC=sum(of baBICy1-baBICy15);
 if VARINMODEL=varsbaBIC then samemodel=1; else samemodel=0;

 BIC_meanfinal=&BIC_mean;
 C_meanfinal=&C_mean;
 IAUC_meanfinal=&IAUC_mean;
 C_adlfinal=&C_adlfinal;
 C_iadlfinal=&C_iadlfinal;
 C_walkfinal=&C_walkfinal;
 C_deathfinal=&C_deathfinal;

 drop delims i;
run;

/*QC*/
proc contents data=BICsim2; run;
proc print data=BICsim2; var varsbaBIC numVarsbaBIC VARINMODEL numVarsfinsim baBICy1-baBICy15 sumbaBIC;  run;
proc freq data=BICsim2; tables sumbaBIC samemodel anyVarNoUnion sumVarNoUnion ; run;

proc print data=BICsim2; var VARINMODEL &varsNoUnion anyvarNoUnion sumVarNoUnion; where anyVarNoUnion=1;  run; /*How many variables that are not in the final-combined model are selected in the simulation models?*/

proc freq data=BICsim2; tables &varsUnion anyVarNoUnion sumbaBIC; run;

data savedata.bicnewsim&S.corrbyoutcome2plus;
 retain sim varsbaBIC numVarsbaBIC VARINMODEL numVarsfinsim &varsUnion &varsNoUnion anyVarNoUnion sumVarNoUnion baBICy1-baBICy15 sumbaBIC samemodel C_meanfinal C_adlfinal C_iadlfinal C_walkfinal C_deathfinal IAUC_meanfinal BIC_meanfinal C_avg C_adl C_iadl C_walk C_death iauc_avg BIC_avg ;
 set BICsim2 (keep=sim varsbaBIC numVarsbaBIC VARINMODEL numVarsfinsim &varsUnion &varsNoUnion anyVarNoUnion sumVarNoUnion baBICy1-baBICy15 sumbaBIC samemodel C_meanfinal C_adlfinal C_iadlfinal C_walkfinal C_deathfinal IAUC_meanfinal BIC_meanfinal C_avg C_adl C_iadl C_walk C_death iauc_avg BIC_avg);
run; /*observations=number of simulations and 79 variables.*/
proc contents data=savedata.bicnewsim&S.corrbyoutcome2plus; run;


/******************************************/
*Compute % of simulations 'best model'/'best variable'/'not best variable' were selected, and mean C-stat, mean IAUC;

/* Use the STACKODSOUTPUT option on the PROC MEANS statement   */
/* and use the ODS OUTPUT statement to create output data set. */

*Summary stat of simulated models vs original model;
proc means data=savedata.bicnewsim&S.corrbyoutcome2plus stackodsoutput n mean clm stderr maxdec=4;
 var numVarsfinsim C_avg C_adl C_iadl C_walk C_death iauc_avg BIC_avg;
 ods output summary=gralstatssim (rename=(N=Simulations Mean=MeanSimulation));
proc sort; by variable; run;

data gralstats;
 set savedata.bicnewsim&S.corrbyoutcome2plus  (keep=numVarsbaBIC C_meanfinal C_adlfinal C_iadlfinal C_walkfinal C_deathfinal IAUC_meanfinal BIC_meanfinal) end=last;
 if last=1;
run;
proc transpose data=gralstats out=gralstats2(rename=(_NAME_=Variable COL1=MeanOriginal)); run;
data gralstats3;
 set gralstats2;
 if variable='numVarsbaBIC' then variable='numVarsfinsim';
 else if variable='C_meanfinal' then variable='C_avg';
 else if variable='C_adlfinal' then variable='C_adl';
 else if variable='C_iadlfinal' then variable='C_iadl';
 else if variable='C_walkfinal' then variable='C_walk';
 else if variable='C_deathfinal' then variable='C_death';
 else if variable='IAUC_meanfinal' then variable='iauc_avg';
 else if variable='BIC_meanfinal' then variable='BIC_avg';
proc sort; by variable; run;
data savedata.BICnewsim&S.corrByOutStatsPlus;
 merge gralstats3 gralstatssim;
 by variable;
 format MeanOriginal 8.4;
proc print; run;

*Percentage of inclusion of variables in the union model using S simulated datasets;
proc means data=savedata.bicnewsim&S.corrbyoutcome2plus  stackodsoutput n mean maxdec=4;
 var &varsUnion samemodel;
 ods output summary=means (rename=(N=Simulations));
run;
data means;
 set means;
 Percentage=Mean*100;
proc print; run;

proc freq data=savedata.bicnewsim&S.corrbyoutcome2plus; tables sumbaBIC / nocum nofreq out=freq (keep=sumbaBIC percent rename=(percent=Percentage)); run;

proc print data=savedata.bicnewsim&S.corrbyoutcome2plus (obs=10); var sim varsbaBIC VARINMODEL baBICy1-baBICy15;  where sumbaBIC=11; run;

data means_freq;
 set means freq;
 if sumbaBIC=9 then do; variable="model_9vars"; simulations=&S; end;
 else if sumbaBIC=10 then do; variable="model_10vars"; simulations=&S; end;
 else if sumbaBIC=11 then do; variable="model_11vars"; simulations=&S; end;
 else if sumbaBIC=12 then do; variable="model_12vars"; simulations=&S; end;
 else if sumbaBIC=13 then do; variable="model_13vars"; simulations=&S; end;
 else if sumbaBIC=14 then do; variable="model_14vars"; simulations=&S; end;
 else if sumbaBIC=15 then do; variable="model_15vars"; simulations=&S; end;
proc print; run;

*Percentage of correct inclusion of variables in baBIC model;
proc means data=savedata.bicnewsim&S.corrbyoutcome2plus  stackodsoutput n mean maxdec=4;
 var baBICy1-baBICy15;
 ods output summary=meansbaBIC (rename=(N=Simulations));
run;
data meansbaBIC;
 set meansbaBIC;
 Percentage=Mean*100;
proc print; run;
proc means data=meansbaBIC n mean; var Mean; where Variable not in ('baBICy1', 'baBICy2'); run;

*Percentage of INCORRECT inclusion of variables in union model using S simulated datasets;
proc means data=savedata.bicnewsim&S.corrbyoutcome2plus stackodsoutput n mean maxdec=4;
 var &varsNoUnion anyVarNoUnion;
 ods output summary=means2 (rename=(N=Simulations));
run;
data means2;
 set means2;
 Percentage=Mean*100;
proc print; run;

proc freq data=savedata.bicnewsim&S.corrbyoutcome2plus; tables sumVarNoUnion / nocum nofreq out=freq2 (keep=sumVarNoUnion percent rename=(percent=Percentage)); run;

data means_freq2;
 set means2 freq2;
 if sumVarNoUnion=1 then do; variable="model_1wrong"; simulations=&S; end;
 else if sumVarNoUnion=0 then delete;
proc print; run;

proc means data=savedata.bicnewsim&S.corrbyoutcome2plus  stackodsoutput n mean maxdec=4;
 var &varsNoUnion;
 ods output summary=meansNoUnion (rename=(N=Simulations));
run;
data meansNoUnion;
 set meansNoUnion;
 Percentage=Mean*100;
proc print; run;
proc means data=meansNoUnion n mean; var Mean; run;

*Percentage of INCORRECT inclusion of variables in baBIC using S simulated datasets;
proc means data=savedata.bicnewsim&S.corrbyoutcome2plus  stackodsoutput n mean maxdec=4;
 var &varsNoUnion OTHERSIT HYPERTENSION OTHERARM OTHERLIFT OTHERSTOOP HEARAID MSTAT OTHERWALK;
 ods output summary=meansNobaBIC (rename=(N=Simulations));
run;
data meansNobaBIC;
 set meansNobaBIC;
 Percentage=Mean*100;
proc print; run;
proc means data=meansNobaBIC n mean; var Percentage; run;

ods select all; /*to print results below*/
ods listing close; /*turn of the output window / "listing" output, so I don't get WARNING: Data too long for column*/

ods path(prepend) work.templat(update);

proc template;
   define style mystyle;
   parent=styles.htmlblue;
   style usertext from usertext /
      foreground=black font_weight=bold;
   end;
run;
ods rtf file="path\Results_baBIC&S.corrStatsScenario2.rtf" startpage=no style=mystyle;
ods text= "Summary statistics for best model in original data vs best models using &S simulated datasets with uncorrelated outcomes";
proc print data=savedata.BICnewsim&S.corrByOutStatsPlus; run;

ods startpage=now;
ods text="Percentage of each variable selected in the final model using &S simulated datasets with uncorrelated outcomes";
proc print data=means_freq noobs; var Variable Simulations Percentage; run;
ods text="Mean of Percentage of correct Inclusion for selected variables (i.e. gender and age were forced in) in the final model using &S simulated datasets";
proc means data=meansbaBIC n mean; var Mean; where Variable not in ('baBICy1', 'baBICy2'); run;
 
ods startpage=now;
ods text="Percentage of incorrect inclusion/variable NOT-selected in Union model using &S simulated datasets with uncorrelated outcomes";
proc print data=means_freq2 noobs; var Variable Simulations Percentage; run;
ods text="Mean of Percentage of incorrect inclusion/variable NOT-selected in Union model using &S simulated datasets with uncorrelated outcomes";
proc means data=meansNoUnion n mean; var Mean; run;
ods rtf close;

proc delete data= BIC_full bic_final BICsim2 varfindata varfindata2 varfindata3 varfindata4 gralstatssim gralstats gralstats2 gralstats3 means means2 freq freq2 means_freq means_freq2 meansbaBIC meansNoUnion _null ; run; quit;

