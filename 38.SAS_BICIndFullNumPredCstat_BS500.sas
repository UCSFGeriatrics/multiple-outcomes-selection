***********************************************************************************************************************************************************************************;
*Program: 38.SAS_BICIndFullNumPredCstat_BS500                                                                                                                                      ;                                                               
*Purpose: Compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in 500 bootstrap samples                          ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2020.10.21																				                                                                               ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
libname outdata "path";

/***********************************************************************************************************************************************/
*Create union and intersection of the best individual models for each outcome in each bs dataset;
%let S=500;

/*Merge all 4 datasets with the best individual model within each outcome for each bs dataset */
data BICbs;
   merge outdata.BICbs_adl outdata.BICbs_iadl outdata.BICbs_walk outdata.BICbs_death;
   by replicate;
run; /* 500 observations and 25 variables.*/

data outdata.BICbs&S.By2  (drop=delims i x: n i temp);
  set BICbs ;
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
/*proc print data=savedata.BICbs&S.By2 (obs=3); var union intersect ; run;*/
*Summary stats of bs dataset models by outcome;

*Summary stats of bs dataset models by outcome;

*Custom percentiles for 95% bootstrap confidence interval: https://blogs.sas.com/content/iml/2013/10/23/percentiles-in-a-tabular-format.html;
proc stdize data=outdata.BICbs&S.By2 PctlMtd=ORD_STAT outstat=outdata.BICbs&S.By2_95CI pctlpts=2.5, 97.5;
 var numVarsfinsim_union numVarsfinsim_intersect numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath;
run;
/*Specify the PCTLMTD= option so that the algorithm uses the traditional "sort the data" algorithm for computing percentiles,
rather than a newer one-pass algorithm. Although the one-pass algorithm is very fast and well-suited for computing the median, 
it is not recommended for computing extreme percentiles such as the 2.5th and 97.5th.*/
 
data outdata.BICbs&S.By2_95CI;
 set outdata.BICbs&S.By2_95CI;
 where _type_ =: 'P';
run;

proc print data=outdata.BICbs&S.By2_95CI noobs; run;

*Rest of statistics;
proc means data=outdata.BICbs&S.By2 stackodsoutput n mean std stderr clm median p25 p75 maxdec=4; 
 var numVarsfinsim_union numVarsfinsim_intersect numVarsfinsimadl numVarsfinsimiadl numVarsfinsimwalk numVarsfinsimdeath C_adl C_iadl C_walk C_death iauc_adl iauc_iadl iauc_walk iauc_death VARSEC_adl VARSEC_iadl VARSEC_walk VARSEC_death ;
 ods output summary=outdata.BICbs&S.Bygralstats(rename=(Mean=Mean_bs_ByOutcome));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.BICbs&S.Bygralstats
            OUTFILE= "path\BICbs&S.Bygralstats.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc delete data=BICbs; run; quit;

*****************************************************PREPARE DATA FOR MACRO ********************************************************;
proc sort data=outdata.bs500 out=bsample; by newid; run; 

*Merge with original dataset to get the covariates;
data bsample2; 
  merge savedata.originaldata bsample (in=A);
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

***************************************************** Calculate C-stat for each outcome using the union and intersect models from bs dataset ******************************************************************;
%macro cstat(predictors=);
 %do i=1 %to &S;
 /*For each bs dataset define VARNAME as the union/intersect*/
  data _null_;
   set outdata.BICbs500By2 (keep=replicate &predictors);
   where replicate=&i;
   call symputx ('VARNAME' , &predictors);
  run;

  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the first id of ith bs dataset*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the last id of ith bs dataset*/

  data bsample3;
   set bsample2 (FIRSTOBS=&fobs OBS=&lobs);
  run;
  sasfile WORK.bsample3 load;

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

  sasfile WORK.bsample3 close;
  proc delete data=bsample3; run; quit;

  data ctable; 
    merge CTABLE_adl CTABLE_iadl(drop=VARINMODEL) CTABLE_walk(drop=VARINMODEL) CTABLE_death(drop=VARINMODEL);
	by replicate;
    C_avg=mean(c_adl,c_iadl, c_walk, c_death);
  run;

  proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

  proc append base=ctablesim_&predictors data=ctable force; run;
  proc delete data=ctable; run; quit;

  %end; /*i loop*/
%mend cstat;

*Union model;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%cstat (predictors=union);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

proc sort data=ctablesim_union out=outdata.BICbs&S.ByUnionC; by replicate; run;

PROC EXPORT DATA= outdata.BICbs&S.ByUnionC
            OUTFILE= "path\BICbs&S.ByUnionC.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of bs dataset models by outcome;
proc means data=outdata.BICbs&S.ByUnionC stackodsoutput n mean clm maxdec=3; 
 var C_adl C_iadl C_walk C_death C_avg;
 ods output summary=outdata.BICbs&S.ByUnionCmeans(rename=(Mean=Mean_BS_ByOutcome_UnionModel));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.BICbs&S.ByUnionCmeans
            OUTFILE= "path\BICbs&S.ByUnionCmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Intersection model;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%cstat (predictors=intersect);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

proc sort data=ctablesim_intersect out=outdata.BICbs&S.ByIntersectC; by replicate; run;

PROC EXPORT DATA= outdata.BICbs&S.ByIntersectC
            OUTFILE= "path\BICbs&S.ByIntersectC.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of bs dataset models by outcome;
proc means data=outdata.BICbs&S.ByIntersectC stackodsoutput n mean clm maxdec=3; 
 var C_adl C_iadl C_walk C_death C_avg;
 ods output summary=outdata.BICbs&S.ByIntersectCmeans(rename=(Mean=Mean_BS_ByOutcome_IntersectModel));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.BICbs&S.ByIntersectCmeans
            OUTFILE= "path\BICbs&S.ByIntersectCmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc delete data=bsample2 ctablesim_union ctablesim_intersect; run; quit;


***************************************************** Calculate C-stat for each outcome using the Full model ******************************************************************;

%macro cstat(model=);

%let VARNAME=dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
		     FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
	         OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
	         qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER; /*all 39 initial variables*/

 %do i=1 %to &S;

  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the first id of ith bs dataset*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the last id of ith bs dataset*/

  data bsample3;
   set bsample2 (FIRSTOBS=&fobs OBS=&lobs);
  run;
  sasfile WORK.bsample3 load;

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

  sasfile WORK.bsample3 close;
  proc delete data=bsample3; run; quit;

  data ctable; 
    merge CTABLE_adl CTABLE_iadl(drop=VARINMODEL) CTABLE_walk(drop=VARINMODEL) CTABLE_death(drop=VARINMODEL);
	by replicate;
    C_avg=mean(c_adl,c_iadl, c_walk, c_death);
  run;

  proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

  proc append base=ctablesim_&model data=ctable force; run;
  proc delete data=ctable; run; quit;

  %end; /*i loop*/
%mend cstat;

*Full model;
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%cstat (model=full);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
/*
======MONITORING: 2020-09-20, 9:26======
======MONITORING: 2020-09-20, 9:40======
*/
proc sort data=ctablesim_full out=outdata.BICbs&S.FullC; by replicate; run;
PROC EXPORT DATA= outdata.BICbs&S.FullC
            OUTFILE= "path\BICbs&S.FullC.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*Summary stats of bs dataset models by outcome;
proc means data=outdata.BICbs&S.FullC stackodsoutput n mean clm maxdec=3; 
 var C_adl C_iadl C_walk C_death C_avg;
 ods output summary=outdata.BICbs&S.FullCmeans(rename=(Mean=Mean_BS_Full));
proc sort; by variable; run;

PROC EXPORT DATA= outdata.BICbs&S.FullCmeans
            OUTFILE= "path\BICbs&S.FullCmeans.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


***************************************************** Calculate Optimism Corrected C-statistic for individual, union, and intersect models ******************************************************************;

/*** Calculate C-statistic of individual, union, baBIC, intersection, and Full model in Case-study data: ***/
options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
*options errorabend; /*so the process stops as soon as an error is encountered*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

data originaldata; set savedata.originaldata; run;

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
%getcstat (label=adl, model=ind, cox=no, data=originaldata, predictors=dAGE SEX DRIVE INCONTINENCE EDU DIABETES  EXERCISE  OTHERARM OTHERLIFT OTHERSTOOP, time=time_adldepdth, status=status_adldepdth );
%getcstat (label=adl, model=union, cox=no, data=originaldata, 
           predictors=dAGE SEX DIABETES DRIVE EDU EXERCISE INCONTINENCE OTHERARM OTHERLIFT OTHERSTOOP COGDLRC3G HEARAID OTHERSIT SMOKING HYPERTENSION OTHERCLIM3G HEARTFAILURE 
                       LUNG MSTAT OTHERPUSH OTHERWALK qBMI VOLUNTEER,
           time=time_adldepdth, status=status_adldepdth );
%getcstat (label=adl, model=baBIC, cox=no, data=originaldata, predictors=dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE COGDLRC3G SMOKING OTHERCLIM3G HEARTFAILURE  LUNG OTHERPUSH qBMI VOLUNTEER, time=time_adldepdth, status=status_adldepdth );
%getcstat (label=adl, model=intersect, cox=no, data=originaldata, predictors=dAGE SEX DRIVE, time=time_adldepdth, status=status_adldepdth );
%getcstat (label=adl, model=full, cox=no, data=originaldata,
           predictors=dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
				      FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
                      OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
				      qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER,
          time=time_adldepdth, status=status_adldepdth );

*IADL;
%getcstat (label=iadl, model=ind, cox=no, data=originaldata, predictors=dAGE SEX DRIVE INCONTINENCE EDU OTHERSIT COGDLRC3G SMOKING HEARAID, time=time_iadldifdth, status=status_iadldifdth );
%getcstat (label=iadl, model=union, cox=no, data=originaldata, 
           predictors=dAGE SEX DIABETES DRIVE EDU EXERCISE INCONTINENCE OTHERARM OTHERLIFT OTHERSTOOP COGDLRC3G HEARAID OTHERSIT SMOKING HYPERTENSION OTHERCLIM3G HEARTFAILURE 
                       LUNG MSTAT OTHERPUSH OTHERWALK qBMI VOLUNTEER,
           time=time_iadldifdth, status=status_iadldifdth );
%getcstat (label=iadl, model=baBIC, cox=no, data=originaldata, predictors=dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE COGDLRC3G SMOKING OTHERCLIM3G HEARTFAILURE  LUNG OTHERPUSH qBMI VOLUNTEER, time=time_iadldifdth, status=status_iadldifdth );
%getcstat (label=iadl, model=intersect, cox=no, data=originaldata, predictors=dAGE SEX DRIVE, time=time_iadldifdth, status=status_iadldifdth );
%getcstat (label=iadl, model=full, cox=no, data=originaldata,
           predictors=dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
				      FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
                      OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
				      qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER,
          time=time_iadldifdth, status=status_iadldifdth );

*WALK;
%getcstat (label=walk, model=ind, cox=no, data=originaldata, predictors=dAGE SEX DRIVE INCONTINENCE OTHERSIT HYPERTENSION OTHERCLIM3G, time=time_walkdepdth, status=status_walkdepdth );
%getcstat (label=walk, model=union, cox=no, data=originaldata, 
           predictors=dAGE SEX DIABETES DRIVE EDU EXERCISE INCONTINENCE OTHERARM OTHERLIFT OTHERSTOOP COGDLRC3G HEARAID OTHERSIT SMOKING HYPERTENSION OTHERCLIM3G HEARTFAILURE 
                       LUNG MSTAT OTHERPUSH OTHERWALK qBMI VOLUNTEER,
           time=time_walkdepdth, status=status_walkdepdth );
%getcstat (label=walk, model=baBIC, cox=no, data=originaldata, predictors=dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE COGDLRC3G SMOKING OTHERCLIM3G HEARTFAILURE  LUNG OTHERPUSH qBMI VOLUNTEER, time=time_walkdepdth, status=status_walkdepdth );
%getcstat (label=walk, model=intersect, cox=no, data=originaldata, predictors=dAGE SEX DRIVE, time=time_walkdepdth, status=status_walkdepdth );
%getcstat (label=walk, model=full, cox=no, data=originaldata,
           predictors=dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
				      FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
                      OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
				      qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER,
          time=time_walkdepdth, status=status_walkdepdth );

*Death;
%getcstat (label=death, model=ind, cox=yes, data=originaldata, 
           predictors=dAGE SEX DRIVE DIABETES EXERCISE COGDLRC3G SMOKING HYPERTENSION OTHERCLIM3G HEARTFAILURE  LUNG MSTAT  OTHERPUSH OTHERWALK qBMI VOLUNTEER,
           time=time2death, status=death );
%getcstat (label=death, model=union, cox=yes, data=originaldata, 
           predictors=dAGE SEX DIABETES DRIVE EDU EXERCISE INCONTINENCE OTHERARM OTHERLIFT OTHERSTOOP COGDLRC3G HEARAID OTHERSIT SMOKING HYPERTENSION OTHERCLIM3G HEARTFAILURE 
                       LUNG MSTAT OTHERPUSH OTHERWALK qBMI VOLUNTEER,
           time=time2death, status=death );
%getcstat (label=death, model=baBIC, cox=yes, data=originaldata, predictors=dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE COGDLRC3G SMOKING OTHERCLIM3G HEARTFAILURE  LUNG OTHERPUSH qBMI VOLUNTEER, time=time2death, status=death );
%getcstat (label=death, model=intersect, cox=yes, data=originaldata, predictors=dAGE SEX DRIVE, time=time2death, status=death );
%getcstat (label=death, model=full, cox=yes, data=originaldata,
           predictors=dAGE SEX ALCOHOL ARTHRITIS CANCER COGDLRC3G COGIMRC3G DIABETES DRIVE EDU EXERCISE EYE2G
				      FALL HEARAID HEARING HEARTFAILURE HYPERTENSION INCONTINENCE LALONE LUNG MSTAT OTHERARM
                      OTHERCHAIR OTHERCLIM3G OTHERDIME OTHERLIFT OTHERPUSH OTHERSIT OTHERSTOOP OTHERWALK PAIN CESDALL
				      qBMI qFAGE qMAGE SHLT SMOKING STROKE VOLUNTEER,
          time=time2death, status=death );

*Save cstat data;
data outdata.cstat_original; set cstat; run;

/*** Fit each of the BS model (obtained using BS data) on the original case study data and calculate their C-statistic (C-stat-Original) ***/

data originaldata; 
 set savedata.originaldata; 
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;

data union_intersect; set outdata.bicbs500by2; run;


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

%c_bs_ori (predictors=intersect, common_pred=yes, model=intersect);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

%c_bs_ori (predictors=VARINMODEL_adl VARINMODEL_iadl VARINMODEL_walk VARINMODEL_death, common_pred=no, model=ind);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
/*
======MONITORING: 2020-09-20, 5:43======
======MONITORING: 2020-09-20, 5:58======
======MONITORING: 2020-09-20, 6:09======
======MONITORING: 2020-09-20, 6:22======
*/

*Save permanent datasets;
data outdata.ctable_ori_sim_union; set ctable_ori_sim_union; run;
data outdata.ctable_ori_sim_intersect; set ctable_ori_sim_intersect; run;
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
proc contents data=outdata.ctable_ori_sim_ind; run;
proc contents data=outdata.BICbs&S.By2; run;

data cop_ind (keep=replicate optimism_ind_adl optimism_ind_iadl optimism_ind_walk optimism_ind_death);
 merge outdata.BICbs&S.By2 (keep=replicate C_adl C_iadl C_walk C_death) outdata.ctable_ori_sim_ind (keep=replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death );
 by replicate;
 optimism_ind_adl=abs(C_adl-cbs_ori_adl);
 optimism_ind_iadl=abs(C_iadl-cbs_ori_iadl);
 optimism_ind_walk=abs(C_walk-cbs_ori_walk);
 optimism_ind_death=abs(C_death-cbs_ori_death);
run;

*QC;
proc print data=outdata.BICbs&S.By2 (obs=3); var replicate C_adl C_iadl C_walk C_death; run;
proc print data=outdata.ctable_ori_sim_ind (obs=3); var replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death; run;
proc print data=cop_ind (obs=3); var replicate optimism_ind_adl optimism_ind_iadl optimism_ind_walk optimism_ind_death; run;
proc means data=cop_ind; run;

*Union model;
proc contents data=outdata.BICbs&S.ByUnionC; run;
proc contents data=outdata.ctable_ori_sim_union; run;

data cop_union (keep=replicate optimism_union_adl optimism_union_iadl optimism_union_walk optimism_union_death optimism_union_avg);
 merge outdata.BICbs&S.ByUnionC (keep=replicate C_adl C_iadl C_walk C_death C_avg) 
       outdata.ctable_ori_sim_union (keep=replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death cbs_ori_avg);
 by replicate;
 optimism_union_adl=abs(C_adl-cbs_ori_adl);
 optimism_union_iadl=abs(C_iadl-cbs_ori_iadl);
 optimism_union_walk=abs(C_walk-cbs_ori_walk);
 optimism_union_death=abs(C_death-cbs_ori_death);
 optimism_union_avg=abs(C_avg-cbs_ori_avg);
run;

*QC;
proc print data=outdata.BICbs&S.ByUnionC (obs=3); var VARINMODEL replicate C_adl C_iadl C_walk C_death C_avg; run;
proc print data=outdata.ctable_ori_sim_union (obs=3); var VARINMODEL replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death cbs_ori_avg; run;
proc print data=cop_union (obs=3); var replicate optimism_union_adl optimism_union_iadl optimism_union_walk optimism_union_death optimism_union_avg; run;
proc means data=cop_union; run;

*Intersection model;
proc contents data=outdata.BICbs&S.ByIntersectC; run;
proc contents data=outdata.ctable_ori_sim_intersect; run;

data cop_intersect (keep=replicate optimism_intersect_adl optimism_intersect_iadl optimism_intersect_walk optimism_intersect_death optimism_intersect_avg);
 merge outdata.BICbs&S.ByIntersectC (keep=replicate C_adl C_iadl C_walk C_death C_avg) 
       outdata.ctable_ori_sim_intersect (keep=replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death cbs_ori_avg);
 by replicate;
 optimism_intersect_adl=abs(C_adl-cbs_ori_adl);
 optimism_intersect_iadl=abs(C_iadl-cbs_ori_iadl);
 optimism_intersect_walk=abs(C_walk-cbs_ori_walk);
 optimism_intersect_death=abs(C_death-cbs_ori_death);
 optimism_intersect_avg=abs(C_avg-cbs_ori_avg);
run;

*QC;
proc print data=outdata.BICbs&S.ByIntersectC (obs=3); var VARINMODEL replicate C_adl C_iadl C_walk C_death C_avg; run;
proc print data=outdata.ctable_ori_sim_intersect (obs=3); var VARINMODEL replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death cbs_ori_avg; run;
proc print data=cop_intersect (obs=3); var replicate optimism_intersect_adl optimism_intersect_iadl optimism_intersect_walk optimism_intersect_death optimism_intersect_avg; run;
proc means data=cop_intersect; run;

*Full model;
proc contents data=outdata.BICbs&S.FullC; run;
proc print data=outdata.cstat_original; run;

*Get C-stats of Full model in original data;
data _null_; set outdata.cstat_original; where model="adl_full"; call symputx ('c_ori_adl' , Estimate); run;
%put &c_ori_adl;
data _null_; set outdata.cstat_original; where model="iadl_full"; call symputx ('c_ori_iadl' , Estimate); run;
%put &c_ori_iadl;
data _null_; set outdata.cstat_original; where model="walk_full"; call symputx ('c_ori_walk' , Estimate); run;
%put &c_ori_walk;
data _null_; set outdata.cstat_original; where model="death_full"; call symputx ('c_ori_death' , Estimate); run;
%put &c_ori_death;

*Get C-stat_full-avg;
proc means data=outdata.cstat_original stackodsoutput mean maxdec=4; 
 var Estimate;
 where model in ("adl_full", "iadl_full", "walk_full", "death_full");
 ods output summary=c_stat_full_avg (keep=Mean rename=(Mean=Estimate));
run;
data _null_; set c_stat_full_avg; call symputx ('c_ori_avg' , Estimate); run;
%put &c_ori_avg;

data cop_full (keep=replicate optimism_full_adl optimism_full_iadl optimism_full_walk optimism_full_death optimism_full_avg);
 set outdata.BICbs&S.FullC (keep=replicate C_adl C_iadl C_walk C_death C_avg) ;
 optimism_full_adl=abs(C_adl-&c_ori_adl);
 optimism_full_iadl=abs(C_iadl-&c_ori_iadl);
 optimism_full_walk=abs(C_walk-&c_ori_walk);
 optimism_full_death=abs(C_death-&c_ori_death);
 optimism_full_avg=abs(C_avg-&c_ori_avg);
run;

*QC;
proc print data=outdata.BICbs&S.FullC (obs=3); var VARINMODEL replicate C_adl C_iadl C_walk C_death C_avg; run;
proc print data=cop_full (obs=3); var replicate optimism_full_adl optimism_full_iadl optimism_full_walk optimism_full_death optimism_full_avg; run;
proc means data=cop_full; run;

*Merge 3 cop: ind, union, intersect;
data outdata.c_opt_ind_union_int_full;
 merge cop_ind cop_union cop_intersect cop_full;
 by replicate;
run;
proc contents data=outdata.c_opt_ind_union_int_full; run;

*Compute average optimism;
proc means data=outdata.c_opt_ind_union_int_full stackodsoutput n mean std stderr clm maxdec=4; 
 var optimism_ind_adl optimism_ind_iadl optimism_ind_walk optimism_ind_death optimism_union_adl optimism_union_iadl optimism_union_walk optimism_union_death optimism_union_avg 
     optimism_intersect_adl optimism_intersect_iadl optimism_intersect_walk optimism_intersect_death optimism_intersect_avg
     optimism_full_adl optimism_full_iadl optimism_full_walk optimism_full_death optimism_full_avg;
 ods output summary=outdata.c_opt_ind_union_int_full_avg;
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
 else if model="death_union" then Variable="optimism_union_death";

 else if model="adl_intersect" then Variable="optimism_intersect_adl";
 else if model="iadl_intersect" then Variable="optimism_intersect_iadl";
 else if model="walk_intersect" then Variable="optimism_intersect_walk";
 else if model="death_intersect" then Variable="optimism_intersect_death";

 else if model="adl_full" then Variable="optimism_full_adl";
 else if model="iadl_full" then Variable="optimism_full_iadl";
 else if model="walk_full" then Variable="optimism_full_walk";
 else if model="death_full" then Variable="optimism_full_death";

 if Variable not =" ";
proc sort; by variable; run;

*Calculate C_stat_union_avg in Original data;
proc means data=cstat_original stackodsoutput mean maxdec=4; 
 var Estimate;
 where model in ("adl_union", "iadl_union", "walk_union", "death_union");
 ods output summary=c_stat_union_avg (keep=Mean rename=(Mean=Estimate));
run;

proc means data=cstat_original stackodsoutput mean maxdec=4; 
 var Estimate;
 where model in ("adl_intersect", "iadl_intersect", "walk_intersect", "death_intersect");
 ods output summary=c_stat_intersect_avg (keep=Mean rename=(Mean=Estimate));
run;

data c_stat_union_avg; set c_stat_union_avg; length Variable $32; Variable="optimism_union_avg"; run;
data c_stat_intersect_avg; set c_stat_intersect_avg; length Variable $32; Variable="optimism_intersect_avg"; run;
data c_stat_full_avg; set c_stat_full_avg; length Variable $32; Variable="optimism_full_avg"; run;


data cstat_original; set cstat_original c_stat_union_avg c_stat_intersect_avg c_stat_full_avg; proc sort; by variable; run;

data outdata.c_statCorrect;
 merge cstat_original (keep=Variable Estimate) outdata.c_opt_ind_union_int_full_avg ;
 by Variable;
 C_stat_correct=Estimate-Mean;
proc sort; by variable; run;
proc print data=outdata.c_statCorrect; run;

PROC EXPORT DATA= outdata.c_statCorrect
            OUTFILE= "path\c_statCorrect.csv" 																						
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

