***********************************************************************************************************************************************************************************;
*Program: 1.SAS_BICbackwardIndOutcomeCR                                                                                                                                            ;                                                               
*Purpose: BIC backward elimination by Outcome using HRS original data set. It uses Cox regression for Death, and Competing-risk regression for rest of outcomes                    ;                                     
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

data finaldata; set savedata.originaldata; run;
sasfile WORK.finaldata load; /*load finaldata in memory*/

***************************************************** DEFINE ARGUMENTS FOR MACROS ********************************************************;
*Note: all the macro variables for outcomes need to be in the same order, e.g. : adl iadl walk death; 
%let DATA=finaldata;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth status_iadldifdth status_walkdepdth death; /*status variables*/
%let ALLTIME=time_adldepdth time_iadldifdth time_walkdepdth time2death; /*time-to-event variables*/
%let ALLLABEL= adl iadl walk death; /*labels for outcomes*/
%let ALLFLAGCOX= no no no yes; /*whether a Cox model is fit or a Competing-risk model is fit*/
%let NUMPRED=37; /*number of predictors without including predictors that are forced in (e.g. dAGE, SEX*/
%let NUMPREDPLUSONE=38; /*number of predictors defined in &NUMPRED (e.g.37) plus one*/


***************************************************** BACKWARD ELIMINATION 4 INDIVIDUAL OUTCOMES **************************************************************;
/*Macro outcome: 
1-) Calls macro BEST_BIC (calls macro DeleteOneVar)
2-) For each outcome, creates one dataset with results from backward elimination
3-) Create a dataset with the best model for each individual outcome
*/

%macro outcome;
 %do i=1 %to &NUMOUTCOMES;
    %let OUTCOME=%scan(&ALLOUTCOME,&i); /*extract the ith outcome, ith time, ith label*/
    %let TIME=%scan(&ALLTIME,&i);
	%let LABEL=%scan(&ALLLABEL,&i);
	%let FLAGCOX=%scan(&ALLFLAGCOX,&i);

	%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
	%best_bic;
	%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

	data savedata.BIC_&label; set BIC_&label (drop=DELELIST); run;
	sasfile WORK.BIC_&label close;

	/*Create new BIC_&label dataset to merge with other outcomes*/
	data BIC_&label (rename=(VARINMODEL=VARINMODEL_&label); 
     set BIC_&label (drop=DELELIST DELEVAR);
    run;
	/*For each outcome: select the best individual model*/
	proc sort data=BIC_&label; by descending BIC_&label; 
    data BIC_&label;
    set BIC_&label point=nobs nobs=nobs;
	output;
	stop;
   run;
 %end;
  /*Merge 4 datasets corresponding to each outcome. Each dataset has one line with the best model for each outcome*/
  data BIC;
   merge BIC_adl BIC_iadl BIC_walk BIC_death;
  run;
  proc delete data=BIC_adl BIC_iadl BIC_walk BIC_death; run; quit;
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

	%if &FLAGCOX=no %then %do;
	    proc phreg data = &DATA; 
          class &BASE;
          model &time*&outcome(0) = &BASE / eventcode=1;
		  output out=BSOUT xbeta=xb;
		  ods output FITSTATISTICS=FITS1;
        run;
		data BSOUT; 
 		 set BSOUT; 
 		 if &outcome=2 then do; /*the status and time need to be changed here since CONCORDANCE option below doesnt understand status=2*/
  		  &outcome=0;
  		  &time=15.0278689;
         end;
        run;
		proc phreg data = BSOUT CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
          class &BASE;
          model &time*&outcome(0) = &BASE / nofit;
		  roc 'CompRiskC' pred=xb;
		  ods output CONCORDANCE=concord IAUC=iauc;
       run;
	   proc delete data=BSOUT; run; quit;
	%end;
	%else %if &FLAGCOX=yes %then %do;
	    proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
          class &BASE;
          model &time*&outcome(0) = &BASE;
		  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
        run;
	%end;

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

	%do j=1 %to &NUMPRED;
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
 	%do k=1 %to %eval(&NUMPREDPLUSONE-&j);
        %let DELEVAR=%scan(&DELE,&k); /*select the jth word to delete. &DELE is defined in 'best_bic' macro*/
		%let VARNAME=%sysfunc(compbl(%sysfunc(tranwrd(&BASE,&DELEVAR,%str( ))))); /*select the final set of variables to run model by replacing the deleted variable with blank*/

	  %if &FLAGCOX=no %then %do;
	    proc phreg data = &DATA; 
          class &VARNAME;
          model &time*&outcome(0) = &VARNAME / eventcode=1;
		  output out=BSOUT xbeta=xb;
		  ods output FITSTATISTICS=FITS1;
        run;
		data BSOUT; 
 		 set BSOUT; 
 		 if &outcome=2 then do; /*the status and time need to be changed here since CONCORDANCE option below doesnt understand status=2*/
  		  &outcome=0;
  		  &time=15.0278689;
         end;
        run;
		proc phreg data = BSOUT CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
           class &VARNAME;
           model &time*&outcome(0) = &VARNAME / nofit;
		   roc 'CompRiskC' pred=xb;
		   ods output CONCORDANCE=concord IAUC=iauc;
        run;
		proc delete data=BSOUT; run; quit; 
	  %end;

	  %else %if &FLAGCOX=yes %then %do;
	     proc phreg data = &DATA CONCORDANCE=HARRELL rocoptions(method=RECURSIVE iauc); 
          class &VARNAME;
          model &time*&outcome(0) = &VARNAME;
		  ods output FITSTATISTICS=FITS1 CONCORDANCE=concord IAUC=iauc;
        run;
	  %end;

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

    %if &k=1 %then %do; /*in the 1st line of CTABLE_&label create CTABLE_&label dataset*/
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


%outcome;

sasfile WORK.finaldata close;

/***********************************************************************************************************************************************/
*Create union of the best individual models for each outcome;

data savedata.BIC_BestModelsUnion (drop=delims i x: n i temp);
 set BIC ;
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

ods select all; /*to print results below*/
ods listing close; /*turn of the output window / "listing" output, so I don't get WARNING: Data too long for column*/

ods csv file="path\Results_BIC_BestModelsUnion.csv";
proc print data=savedata.BIC_BestModelsUnion; run;
ods csv close;
ods csv file="path\Results_BIC_ADL.csv";
proc print data=savedata.BIC_adl; run;
ods csv close;
ods csv file="path\Results_BIC_IDL.csv";
proc print data=savedata.BIC_iadl; run;
ods csv close;
ods csv file="path\Results_BIC_WALK.csv";
proc print data=savedata.BIC_walk; run;
ods csv close;
ods csv file="path\Results_BIC_DEATH.csv";
proc print data=savedata.BIC_death; run;
ods csv close;

ods listing; /* turn back on the output window / "listing" output*/
