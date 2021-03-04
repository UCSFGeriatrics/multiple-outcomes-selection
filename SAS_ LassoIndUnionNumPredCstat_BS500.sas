***********************************************************************************************************************************************************************************;
*Program: SAS_ LassoIndUnionNumPredCstat_BS500                                                                                                                                     ;                                                               
*Purpose: Compute summary statistics for number of predictors and C-statistic for LASSO Individual and Union Methods using 500 bootstrap samples                                   ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2021.03.01																				                                                                               ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
libname outdata "path";
libname outdata2 "path";

/***********************************************************************************************************************************************/
*Create union and intersection of the best individual models for each outcome in each bs dataset;
%let S=500;

/* Import lasso_bs data */
PROC IMPORT OUT= WORK.lasso_bs
	DATAFILE= "path\lasso_bs.csv" 
	DBMS=CSV REPLACE;
	guessingrows=444444; /*GUESSINGROWS option is used to determine the maximum length for each variable and is set to an arbitrarily large number so that it doesn't truncate unusual long strings (eg. row 51 for ADL*/
	/*When GUESSINGROWS is small, fewer resources are expended, but the possibility of incorrect determinations, such as a character field being assigned to a numeric variable, is increased */
	GETNAMES=YES;
	DATAROW=2; 
RUN;
/*500 observations and 5 variables.*/

data outdata.Lasso_bs&S.By2  (drop=delims i x: n i temp);
  set lasso_bs ;
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
proc print data=outdata.Lasso_bs&S.By2 (obs=3); run;

*Summary stats of bs dataset models by outcome;

*Custom percentiles for 95% bootstrap confidence interval: 
proc stdize data=outdata.Lasso_bs&S.By2 PctlMtd=ORD_STAT outstat=outdata.Lasso_bs&S.By2_95CI pctlpts=2.5, 97.5;
 var numVarsfinsim_union numVarsfinsim_intersect numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath;
run;
/*Specify the PCTLMTD= option so that the algorithm uses the traditional "sort the data" algorithm for computing percentiles,
rather than a newer one-pass algorithm. Although the one-pass algorithm is very fast and well-suited for computing the median, 
it is not recommended for computing extreme percentiles such as the 2.5th and 97.5th.*/
 
data outdata.Lasso_bs&S.By2_95CI;
 set outdata.Lasso_bs&S.By2_95CI;
 where _type_ =: 'P';
run;

proc print data=outdata.Lasso_bs&S.By2_95CI noobs; run;

*Rest of statistics;
proc means data=outdata.Lasso_bs&S.By2 stackodsoutput n mean std stderr clm maxdec=4 min max median p25 p75; 
 var numVarsfinsim_union numVarsfinsim_intersect numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath ;
 ods output summary=outdata.Lasso_bs&S.By2gralstats(rename=(Mean=Mean_bs_ByOutcome));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.Lasso_bs&S.By2gralstats
            OUTFILE= "path\Lasso_bs&S.By2gralstats.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc delete data=Lasso_bs data1 UniWidePctls ; run; quit;

*****************************************************PREPARE DATA FOR MACRO ********************************************************;
proc sort data=outdata2.bs500 out=bsample; by newid; run; 

*Need to change some predictors names in originaldata (i.e. remove numbers from name of predictors that have numbers since in LASSO I removed them);
data originaldata; 
 set savedata.originaldata (rename=(COGDLRC3G=COGDLRCG COGIMRC3G=COGIMRCG EYE2G=EYEG OTHERCLIM3G=OTHERCLIMG));
proc sort; by newid; run;

*Merge with original dataset to get the covariates;
data bsample2; 
  merge originaldata bsample (in=A);
  by newid;
  if A;
run;
/*2,765,500 observations and 64+1=65 variables.*/

data bsample2;
 set bsample2; 
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
proc sort; by replicate newid; run;

proc contents data=bsample2; run;
proc freq data=bsample2; tables replicate; run;

proc delete data=bsample; run; quit;


***************************************************** Calculate Optimism Corrected C-statistic for individual and union models ******************************************************************;

/*** Calculate C-statistic of individual and union in Case-study data: ***/

*Get union model from LASSO individual models in Case-study data;
PROC IMPORT OUT= WORK.lasso_original 
            DATAFILE= "path\lasso_original.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data outdata.lasso_original  (drop=delims i x: n i temp);
  set lasso_original ;
  length  union intersect $1000;

*New method;
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
proc print data=outdata.lasso_original; run;

options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
*options errorabend; /*so the process stops as soon as an error is encountered*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

%macro getcstat(label=, model=, cox=, data=, predictors=, time=, status=);

%if &cox=no %then %do;
	proc phreg data = &data; 
	  class &predictors;
	  model &time*&status(0) = &predictors / eventcode=1;
	  output out=BSOUT xbeta=xb;
	run; 
	data BSOUT; 
	 set BSOUT; 
	 if &status=2 then do;
	  &status=0;
	  &time=15.0278689;
	 end;
	run;
	proc phreg data = BSOUT CONCORDANCE=HARRELL(SE) ; 
		class &predictors;
		model &time*&status(0) = &predictors /nofit;
		roc 'CompRiskC' pred=xb;
		ods output CONCORDANCE=concord (keep=Estimate StdErr) ;
	run;
	proc delete data=bsout ; run; quit;
%end;

%else %if &cox=yes %then %do;
	proc phreg data = &data CONCORDANCE=HARRELL(SE) ; 
	  class &predictors;
	  model &time*&status(0) = &predictors;
	  ods output CONCORDANCE=concord (keep=Estimate StdErr) ;
	run; 
%end;

data concord;
 set concord; 
 length model $30;
 model="&label._&model";
run;

proc append base=cstat data=concord force; run;
proc delete data=concord ; run; quit;

%mend getcstat;

*ADL;
%getcstat (label=adl, model=ind, cox=no, data=originaldata, 
           predictors=dAGE SEX COGDLRCG DRIVE EDU EXERCISE HYPERTENSION INCONTINENCE OTHERARM OTHERCHAIR OTHERCLIMG OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP SMOKING, 
           time=time_adldepdth, status=status_adldepdth );
%getcstat (label=adl, model=union, cox=no, data=originaldata, 
           predictors=dAGE SEX COGDLRCG DRIVE EDU EXERCISE HYPERTENSION INCONTINENCE OTHERARM OTHERCHAIR OTHERCLIMG OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP SMOKING CANCER
                      COGIMRCG EYEG FALL HEARAID PAIN CESDALL qFAGE STROKE ALCOHOL ARTHRITIS DIABETES HEARING HEARTFAILURE LALONE LUNG MSTAT OTHERWALK qBMI qMAGE SHLT VOLUNTEER,
           time=time_adldepdth, status=status_adldepdth );

*IADL;
%getcstat (label=iadl, model=ind, cox=no, data=originaldata, 
           predictors=dAGE SEX CANCER COGDLRCG COGIMRCG DRIVE EDU EYEG FALL HEARAID INCONTINENCE OTHERLIFT OTHERSIT PAIN CESDALL qFAGE SMOKING STROKE, 
           time=time_iadldifdth, status=status_iadldifdth );
%getcstat (label=iadl, model=union, cox=no, data=originaldata, 
           predictors=dAGE SEX COGDLRCG DRIVE EDU EXERCISE HYPERTENSION INCONTINENCE OTHERARM OTHERCHAIR OTHERCLIMG OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP SMOKING CANCER
                      COGIMRCG EYEG FALL HEARAID PAIN CESDALL qFAGE STROKE ALCOHOL ARTHRITIS DIABETES HEARING HEARTFAILURE LALONE LUNG MSTAT OTHERWALK qBMI qMAGE SHLT VOLUNTEER,
           time=time_iadldifdth, status=status_iadldifdth );
*WALK;
%getcstat (label=walk, model=ind, cox=no, data=originaldata, 
           predictors=dAGE SEX COGDLRCG COGIMRCG DRIVE EXERCISE FALL HYPERTENSION INCONTINENCE OTHERCHAIR OTHERCLIMG OTHERLIFT OTHERSTOOP,
		   time=time_walkdepdth, status=status_walkdepdth );
%getcstat (label=walk, model=union, cox=no, data=originaldata, 
           predictors=dAGE SEX COGDLRCG DRIVE EDU EXERCISE HYPERTENSION INCONTINENCE OTHERARM OTHERCHAIR OTHERCLIMG OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP SMOKING CANCER
                      COGIMRCG EYEG FALL HEARAID PAIN CESDALL qFAGE STROKE ALCOHOL ARTHRITIS DIABETES HEARING HEARTFAILURE LALONE LUNG MSTAT OTHERWALK qBMI qMAGE SHLT VOLUNTEER,
           time=time_walkdepdth, status=status_walkdepdth );

*Death;
%getcstat (label=death, model=ind, cox=yes, data=originaldata, 
           predictors=dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRCG COGIMRCG DIABETES DRIVE EDU EXERCISE FALL HEARAID HEARING HEARTFAILURE HYPERTENSION LALONE LUNG MSTAT
                      OTHERCHAIR OTHERCLIMG OTHERLIFT OTHERPUSH OTHERSIT OTHERWALK qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER,
           time=time2death, status=death );
%getcstat (label=death, model=union, cox=yes, data=originaldata, 
           predictors=dAGE SEX COGDLRCG DRIVE EDU EXERCISE HYPERTENSION INCONTINENCE OTHERARM OTHERCHAIR OTHERCLIMG OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP SMOKING CANCER
                      COGIMRCG EYEG FALL HEARAID PAIN CESDALL qFAGE STROKE ALCOHOL ARTHRITIS DIABETES HEARING HEARTFAILURE LALONE LUNG MSTAT OTHERWALK qBMI qMAGE SHLT VOLUNTEER,
           time=time2death, status=death );

*Save cstat data;
data outdata.cstat_original; set cstat; run;
proc delete data=cstat; run; quit;

*Add 95% CI to original data;
data outdata.cstat_original_95CI;
  set outdata.cstat_original;
  lcl_95=estimate - quantile('normal', 0.975)*stderr;
  ucl_95=estimate + quantile('normal', 0.975)*stderr;
run;

proc print data=outdata.cstat_original_95CI; run;


***************************************************** Calculate C-stat for each outcome in bs data using the union and individual models from bs dataset ******************************************************************;

options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
options errorabend; /*so the process stops as soon as an error is encountered*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

*Define macro variables;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth status_iadldifdth status_walkdepdth death;
%let ALLTIME=time_adldepdth time_iadldifdth time_walkdepdth time2death;
%let ALLLABEL= adl iadl walk death; /*labels for outcomes*/
proc sql noprint; select max(replicate) format 3. into :S from bsample2; quit; /*create macro variable with total number of bs dataset datasets*/
%put "&S"; 

data union_intersect; set outdata.lasso_bs500by2; run;


%macro cstat(predictors=, common_pred=, model=);
 %do i=1 %to &S;

  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the first id of ith bs dataset*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the last id of ith bs dataset*/

  data bsample3;
   set bsample2 (FIRSTOBS=&fobs OBS=&lobs);
  run;
  sasfile WORK.bsample3 load;

	%if &common_pred=yes %then %do;
	  data _null_;
	   set union_intersect (keep=replicate &predictors);
	   where replicate=&i;
	   call symputx ('VARNAME' , &predictors);
	  run;

	  %do j=1 %to &NUMOUTCOMES;
	    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the jth outcome, jth time, jth label*/
	    %let TIME=%scan(&ALLTIME,&j);
		%let LABEL=%scan(&ALLLABEL,&j);

		proc phreg data = bsample3 CONCORDANCE=HARRELL; 
	      class &VARNAME;
	      model &time*&outcome(0) = &VARNAME;
		  ods output CONCORDANCE=concord ;
	    run;

		data CTABLE_&label;
	     set concord (keep= estimate rename=(estimate=c_&label));
		 length VARINMODEL $1000;
	     replicate=&i;
		 VARINMODEL="&VARNAME";
	    run;

		proc delete data=concord; run; quit;

     %end; /*j loop*/

	 data ctable; 
       merge CTABLE_adl CTABLE_iadl(drop=VARINMODEL) CTABLE_walk(drop=VARINMODEL) CTABLE_death(drop=VARINMODEL);
	   by replicate;
       C_avg=mean(c_adl,c_iadl, c_walk, c_death);
     run;

	proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

   %end; /*&common pred DO*/


   %else %if &common_pred=no %then %do;

	  %do j=1 %to &NUMOUTCOMES;
	    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the jth outcome, jth time, jth label*/
	    %let TIME=%scan(&ALLTIME,&j);
		%let LABEL=%scan(&ALLLABEL,&j);
		%let PREDICTOR=%scan(&predictors,&j);

		data _null_;
	      set union_intersect (keep=replicate &PREDICTOR);
	      where replicate=&i;
	      call symputx ('VARNAME' , &PREDICTOR);
	     run;

		proc phreg data = bsample3 CONCORDANCE=HARRELL; 
	      class &VARNAME;
	      model &time*&outcome(0) = &VARNAME;
		  ods output CONCORDANCE=concord ;
	    run;

		data CTABLE_&label;
	     set concord (keep= estimate rename=(estimate=c_&label));
		 length VARINMODEL_&label $1000;
	     replicate=&i;
		 VARINMODEL_&label="&VARNAME";
	    run;

		proc delete data=concord; run; quit;

     %end; /*j loop*/

	 data ctable; 
       merge CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death;
	   by replicate;
       C_avg=mean(c_adl,c_iadl, c_walk, c_death);
     run;

	proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

   %end; /*&common pred DO*/

  sasfile WORK.bsample3 close;
  proc delete data=bsample3; run; quit;


  proc append base=ctablesim_&model data=ctable force; run;
  proc delete data=ctable; run; quit;

  %end; /*i loop*/
%mend cstat;

*Union model;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%cstat (predictors=union, common_pred=yes, model=union); 
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%cstat (predictors=VARINMODEL_adl VARINMODEL_iadl VARINMODEL_walk VARINMODEL_death, common_pred=no, model=ind);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

/*
======MONITORING: 2020-10-07, 15:02======
======MONITORING: 2020-10-07, 15:15======
======MONITORING: 2020-10-07, 15:26======
*/

*Save permanent datasets;
data outdata.lasso_bs500byUnionC; set ctablesim_union; run;
data outdata.lasso_bs500byIndC; set ctablesim_ind; run;

*Union;
PROC EXPORT DATA= outdata.lasso_bs500byUnionC
            OUTFILE= "path\lasso_bs500byUnionC.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
*Summary stats of bs dataset models by outcome;
proc means data=outdata.lasso_bs500byUnionC stackodsoutput n mean clm maxdec=3; 
 var C_adl C_iadl C_walk C_death C_avg;
 ods output summary=outdata.lasso_bs500byUnionCmeans(rename=(Mean=Mean_BS_ByOutcome_UnionModel));
proc sort; by variable; run;
PROC EXPORT DATA= outdata.lasso_bs500byUnionCmeans
            OUTFILE= "path\lasso_bs500byUnionCmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Individual;
PROC EXPORT DATA= outdata.lasso_bs500byIndC
            OUTFILE= "path\lasso_bs500byIndC.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
*Summary stats of bs dataset models by outcome;
proc means data=outdata.lasso_bs500byIndC stackodsoutput n mean clm maxdec=3; 
 var C_adl C_iadl C_walk C_death C_avg;
 ods output summary=outdata.lasso_bs500byIndCmeans(rename=(Mean=Mean_BS_ByOutcome_IndModel));
proc sort; by variable; run;
PROC EXPORT DATA= outdata.lasso_bs500byIndCmeans
            OUTFILE= "path\lasso_bs500byIndCmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


proc delete data=bsample2 ctablesim_union ctablesim_ind; run; quit;



/****************************************************************************************************************************************************/
/*** Fit each of the BS model (obtained using BS data) on the original case study data and calculate their C-statistic (C-stat-Original) ***/

data originaldata; 
 set originaldata; 
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;


options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
*options errorabend; /*so the process stops as soon as an error is encountered*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

*Define macro variables;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth status_iadldifdth status_walkdepdth death;
%let ALLTIME=time_adldepdth time_iadldifdth time_walkdepdth time2death;
%let ALLLABEL= adl iadl walk death; /*labels for outcomes*/
proc sql noprint; select max(replicate) format 3. into :S from union_intersect; quit; /*create macro variable with total number of bs dataset datasets*/
%put "&S"; 

%macro c_bs_ori(predictors=, common_pred=, model=);
 %do i=1 %to &S;
 /*For each bs dataset define VARNAME as the union/intersect*/

	%if &common_pred=yes %then %do;
	  data _null_;
	   set union_intersect (keep=replicate &predictors);
	   where replicate=&i;
	   call symputx ('VARNAME' , &predictors);
	  run;

	  %do j=1 %to &NUMOUTCOMES;
	    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the jth outcome, jth time, jth label*/
	    %let TIME=%scan(&ALLTIME,&j);
		%let LABEL=%scan(&ALLLABEL,&j);

		proc phreg data = originaldata CONCORDANCE=HARRELL; 
	      class &VARNAME;
	      model &time*&outcome(0) = &VARNAME;
		  ods output CONCORDANCE=concord ;
	    run;

		data CTABLE_&label;
	     set concord (keep= estimate rename=(estimate=cbs_ori_&label));
		 length VARINMODEL $1000;
	     replicate=&i;
		 VARINMODEL="&VARNAME";
	    run;

		proc delete data=concord; run; quit;

     %end; /*j loop*/

	 data ctable; 
       merge CTABLE_adl CTABLE_iadl(drop=VARINMODEL) CTABLE_walk(drop=VARINMODEL) CTABLE_death(drop=VARINMODEL);
	   by replicate;
      cbs_ori_avg=mean(cbs_ori_adl, cbs_ori_iadl, cbs_ori_walk, cbs_ori_death);
     run;

	proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

   %end; /*&common pred DO*/

  %else %if &common_pred=no %then %do;

	  %do j=1 %to &NUMOUTCOMES;
	    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the jth outcome, jth time, jth label*/
	    %let TIME=%scan(&ALLTIME,&j);
		%let LABEL=%scan(&ALLLABEL,&j);
		%let PREDICTOR=%scan(&predictors,&j);

		data _null_;
	      set union_intersect (keep=replicate &PREDICTOR);
	      where replicate=&i;
	      call symputx ('VARNAME' , &PREDICTOR);
	     run;

		proc phreg data = originaldata CONCORDANCE=HARRELL; 
	      class &VARNAME;
	      model &time*&outcome(0) = &VARNAME;
		  ods output CONCORDANCE=concord ;
	    run;

		data CTABLE_&label;
	     set concord (keep= estimate rename=(estimate=cbs_ori_&label));
		 length VARINMODEL_&label $1000;
	     replicate=&i;
		 VARINMODEL_&label="&VARNAME";
	    run;

		proc delete data=concord; run; quit;

     %end; /*j loop*/

	 data ctable; 
       merge CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death;
	   by replicate;
      cbs_ori_avg=mean(cbs_ori_adl, cbs_ori_iadl, cbs_ori_walk, cbs_ori_death);
     run;

	proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

   %end; /*&common pred DO*/

  proc append base=ctable_ori_sim_&model data=ctable force; run;
  proc delete data=ctable; run; quit;

  %end; /*i loop*/

%mend c_bs_ori;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%c_bs_ori (predictors=union, common_pred=yes, model=union);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_bs_ori (predictors=VARINMODEL_adl VARINMODEL_iadl VARINMODEL_walk VARINMODEL_death, common_pred=no, model=ind);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
/*
======MONITORING: 2020-10-07, 15:39======
======MONITORING: 2020-10-07, 15:55======
======MONITORING: 2020-10-07, 16:09======
*/

*Save permanent datasets;
data outdata.ctable_ori_sim_union; set ctable_ori_sim_union; run;
data outdata.ctable_ori_sim_ind; set ctable_ori_sim_ind; run;


/*** Calculate degree of optimism:
Optimism= Average (Absolute difference: C-stat-BS- C-stat-Original) across 500 BS
Include the average optimism for each outcome.

To compute the average Optimism for the 3 outcomes:
1-) Obtain average C-stat-BS across outcomes for each BS (C-stat-BS-avg)
2-) Obtain average C-stat-original across outcomes for each BS (C-stat-original-avg)
3-) For each BS: Compute Absolute difference: C-stat-BS-avg - C-stat-Original-avg
4-) Compute Average (Absolute difference: C-stat-BS-avg - C-stat-original-avg) across 500 BS

The corrected C-stat final models = C-stat of original sample (without Wolbers approximation) – degree of optimism (using Wolbers approximation). 
***/
%let S=500;

*Individual models;
ods select all;
proc contents data=outdata.ctable_ori_sim_ind; run;
proc contents data=outdata.lasso_bs500byindc; run;

data cop_ind (keep=replicate optimism_ind_adl optimism_ind_iadl optimism_ind_walk optimism_ind_death);
 merge outdata.lasso_bs500byindc (keep=replicate C_adl C_iadl C_walk C_death) outdata.ctable_ori_sim_ind (keep=replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death );
 by replicate;
 optimism_ind_adl=abs(C_adl-cbs_ori_adl);
 optimism_ind_iadl=abs(C_iadl-cbs_ori_iadl);
 optimism_ind_walk=abs(C_walk-cbs_ori_walk);
 optimism_ind_death=abs(C_death-cbs_ori_death);
run;

*QC;
proc print data=outdata.lasso_bs500byindc (obs=3); var replicate C_adl C_iadl C_walk C_death; run;
proc print data=outdata.ctable_ori_sim_ind (obs=3); var replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death; run;
proc print data=cop_ind (obs=3); var replicate optimism_ind_adl optimism_ind_iadl optimism_ind_walk optimism_ind_death; run;
proc means data=cop_ind; run;

*Union model;
proc contents data=outdata.lasso_bs500byUnionc; run;
proc contents data=outdata.ctable_ori_sim_union; run;

data cop_union (keep=replicate optimism_union_adl optimism_union_iadl optimism_union_walk optimism_union_death optimism_union_avg);
 merge outdata.lasso_bs500byUnionc(keep=replicate C_adl C_iadl C_walk C_death C_avg) 
       outdata.ctable_ori_sim_union (keep=replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death cbs_ori_avg);
 by replicate;
 optimism_union_adl=abs(C_adl-cbs_ori_adl);
 optimism_union_iadl=abs(C_iadl-cbs_ori_iadl);
 optimism_union_walk=abs(C_walk-cbs_ori_walk);
 optimism_union_death=abs(C_death-cbs_ori_death);
 optimism_union_avg=abs(C_avg-cbs_ori_avg);
run;

*QC;
proc print data=outdata.lasso_bs500byUnionc (obs=3); var VARINMODEL replicate C_adl C_iadl C_walk C_death C_avg; run;
proc print data=outdata.ctable_ori_sim_union (obs=3); var VARINMODEL replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death cbs_ori_avg; run;
proc print data=cop_union (obs=3); var replicate optimism_union_adl optimism_union_iadl optimism_union_walk optimism_union_death optimism_union_avg; run;
proc means data=cop_union; run;

*Merge 2 cop: ind, union;
data outdata.c_opt_ind_union;
 merge cop_ind cop_union ;
 by replicate;
run;
proc contents data=outdata.c_opt_ind_union; run;

*Compute average optimism;
proc means data=outdata.c_opt_ind_union stackodsoutput n mean std stderr clm maxdec=4; 
 var optimism_ind_adl optimism_ind_iadl optimism_ind_walk optimism_ind_death optimism_union_adl optimism_union_iadl optimism_union_walk optimism_union_death optimism_union_avg;
 ods output summary=outdata.c_opt_ind_union_avg;
proc sort; by variable; run;

*Compute Optimism corrected C-statistic;

data cstat_original;
 set outdata.cstat_original;
 length Variable $32;
 if model="adl_ind" then Variable="optimism_ind_adl";
 else if model="iadl_ind" then Variable="optimism_ind_iadl";
 else if model="walk_ind" then Variable="optimism_ind_walk";
 else if model="death_ind" then Variable="optimism_ind_death";

 else if model="adl_union" then Variable="optimism_union_adl";
 else if model="iadl_union" then Variable="optimism_union_iadl";
 else if model="walk_union" then Variable="optimism_union_walk";
 else if model="death_union" then Variable="optimism_union_death";

  if Variable not =" ";
proc sort; by variable; run;

*Calculate C_stat_union_avg in Original data;
proc means data=cstat_original stackodsoutput mean maxdec=4; 
 var Estimate;
 where model in ("adl_union", "iadl_union", "walk_union", "death_union");
 ods output summary=c_stat_union_avg (keep=Mean rename=(Mean=Estimate));
run;

data c_stat_union_avg; set c_stat_union_avg; length Variable $32; Variable="optimism_union_avg"; run;

data cstat_original; set cstat_original c_stat_union_avg; proc sort; by variable; run;

data outdata.lasso_c_statCorrect;
 merge cstat_original (keep=Variable Estimate) outdata.c_opt_ind_union_avg ;
 by Variable;
 C_stat_correct=Estimate-Mean;
proc sort; by variable; run;
proc print data=outdata.lasso_c_statCorrect; run;

PROC EXPORT DATA= outdata.lasso_c_statCorrect
            OUTFILE= "path\lasso_c_statCorrect.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

/****************************************************************************************************************************************************************************************/
*Compute Location-shifted bootstrap confidence interval;

/*
Reference: as of 2.26.2021
Noma H, Shinozaki T, Iba K, Teramukai S, Furukawa TA. Confidence intervals of prediction accuracy measures for multivariable prediction models based
on the bootstrap-based optimism correction methods. arXiv preprint arXiv:2005.01457. https://arxiv.org/ftp/arxiv/papers/2005/2005.01457.pdf
*/

/*Method description:
Algorithm 1 (Location-shifted bootstrap confidence interval)
1. For a multivariable prediction model, let theta_hat_app be the apparent predictive measure for the derivation population and
   let theta_hat be the optimism-corrected predictive measure obtained from the Harrell’s bias correction, 0.632, or 0.632+ method.
2. In the computational processes of theta_hat, we can obtain a bootstrap estimate of the sampling distribution of theta_hat_app from the B bootstrap samples.
   Compute the bootstrap confidence interval of theta_app from the bootstrap distribution, (theta_hat_app_L, theta_hat_app_U); 
   for the 95% confidence interval, they are typically calculated by the 2.5th and 97.5th percentiles of the bootstrap distribution.
3. Calculate the bias estimate by optimism, delta_hat=theta_hat_app-theta_hat
4. Then, the location-shifted bootstrap confidence interval is computed as (theta_hat_app_L- theta_hat, theta_hat_app_U- theta_hat)
*/


*Custom percentiles individual;
proc stdize data=outdata.lasso_bs500byIndC PctlMtd=ORD_STAT outstat=ind pctlpts=2.5, 97.5;
 var C_adl C_iadl C_walk C_death ;
run;
 
data ind;
 set ind;
 where _type_ =: 'P';
run;

proc print data=ind noobs; run;

proc transpose data=ind out=wide ;
   by _type_;
   var C_adl C_iadl C_walk C_death;
run;
data wide (drop=_NAME_ _type_ _label_);
 set wide (rename=(col1=P2_5));
 length Variable $32;
 if _NAME_="c_adl" then Variable="optimism_ind_adl";
 else if _NAME_="c_iadl" then Variable="optimism_ind_iadl";
 else if _NAME_="c_walk" then Variable="optimism_ind_walk";
 else if _NAME_="c_death" then Variable="optimism_ind_death";

 if _type_="P97_5000" and Variable="optimism_ind_adl" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_ind_iadl" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_ind_walk" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_ind_death" then P97_5=P2_5;
run;
data wide2_5 (drop=p97_5);
 set wide ;
 where P97_5=.;
proc sort; by Variable; run;
data wide97_5 (drop=p2_5);
 set wide ;
 where P97_5 ne .;
proc sort; by Variable; run;
data ind;
 retain Variable P2_5 P97_5;
 merge wide2_5 wide97_5;
 by Variable;
run;
proc print data=ind; run;

proc delete data=wide wide2_5 wide97_5; run;

*Custom percentiles union;
proc stdize data=outdata.lasso_bs500byUnionC PctlMtd=ORD_STAT outstat=union pctlpts=2.5, 97.5;
 var C_adl C_iadl C_walk C_death ;
run;
 
data union;
 set union;
 where _type_ =: 'P';
run;

proc print data=union noobs; run;

proc transpose data=union out=wide ;
   by _type_;
   var C_adl C_iadl C_walk C_death;
run;
data wide (drop=_NAME_ _type_ _label_);
 set wide (rename=(col1=P2_5));
 length Variable $32;
 if _NAME_="c_adl" then Variable="optimism_union_adl";
 else if _NAME_="c_iadl" then Variable="optimism_union_iadl";
 else if _NAME_="c_walk" then Variable="optimism_union_walk";
 else if _NAME_="c_death" then Variable="optimism_union_death";

 if _type_="P97_5000" and Variable="optimism_union_adl" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_union_iadl" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_union_walk" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_union_death" then P97_5=P2_5;
run;
data wide2_5 (drop=p97_5);
 set wide ;
 where P97_5=.;
proc sort; by Variable; run;
data wide97_5 (drop=p2_5);
 set wide ;
 where P97_5 ne .;
proc sort; by Variable; run;
data union;
 retain Variable P2_5 P97_5;
 merge wide2_5 wide97_5;
 by Variable;
run;
proc print data=union; run;

proc delete data=wide wide2_5 wide97_5; run;

data all;
 set ind union;
proc sort; by variable; run; 
proc print data=all; run;

proc print data=outdata.lasso_c_statcorrect; run;

data all2;
 merge outdata.lasso_c_statcorrect all;
 by Variable;
 P2_5correct=P2_5-Mean; /*Mean: optimism estimate*/
 P97_5correct=P97_5-Mean;
run;
proc print data=all2; run;

*Save permanent data;
data outdata.lasso_c_statcorrect_percentiles;
 set all2;
run;

PROC EXPORT DATA= outdata.lasso_c_statcorrect_percentiles
            OUTFILE= "path\lasso_c_statcorrect_percentiles.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;






