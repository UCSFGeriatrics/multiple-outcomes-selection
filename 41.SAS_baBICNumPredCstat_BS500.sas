***********************************************************************************************************************************************************************************;
*Program: 41.SAS_baBICNumPredCstat_BS500                                                                                                                                           ;                                                               
*Purpose: Compute summary statistics for number of predictors and C-statistic for baBIC method in 500 bootstrap samples                                                            ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2021.03.01																				                                                                               ;
*Modified: 2022.07.09																				                                                                               ;
*Reason modified: Recompute C-stat-Original as described below                                                                                                                     ;
***********************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";
libname outdata "path";

options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
*options errorabend; /*so the process stops as soon as an error is encountered*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/


***************************************************** Number of predictors ******************************************************************;


data baBICbs; 
 set outdata.bicrep500;
 numVarsfinsim=countw(VARINMODEL, '');
run;
proc means data=baBICbs n mean std stderr clm median p25 p75 maxdec=4; var numVarsfinsim; run;


*Custom percentiles for 95% bootstrap confidence interval:;
proc stdize data=baBICbs PctlMtd=ORD_STAT outstat=outdata.baBICbs500_95CI pctlpts=2.5, 97.5;
 var numVarsfinsim;
run;
/*Specify the PCTLMTD= option so that the algorithm uses the traditional "sort the data" algorithm for computing percentiles,
rather than a newer one-pass algorithm. Although the one-pass algorithm is very fast and well-suited for computing the median, 
it is not recommended for computing extreme percentiles such as the 2.5th and 97.5th.*/
 
data outdata.baBICbs500_95CI;
 set outdata.baBICbs500_95CI;
 where _type_ =: 'P';
run;

proc print data=outdata.baBICbs500_95CI noobs; run;



*****************************************************PREPARE DATA FOR MACRO ********************************************************;

data originaldata; 
 set savedata.originaldata; 
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;
proc freq data=originaldata; tables status_adldepdth status_iadldifdth status_walkdepdth; run; 

proc sort data=savedata.bs500 out=bsample; by newid; run; 

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
proc freq data=bsample2; tables status_adldepdth status_iadldifdth status_walkdepdth; run; 

proc delete data=bsample; run; quit;


***************************************************** Calculate Optimism Corrected C-statistic for baBIC models ******************************************************************;

/*** Fit each of the BS model (obtained using BS data) on the original case study data and calculate their C-statistic (C-stat-Original)

In previous version, we used the variables selected in each bootstrap model to fit the model with these variables in the original data
This could be thought as bootstrap of the selection method.

The general steps are:
1) Obtain final model and corresponding C-statistic on the case study data, namely C-statistic-apparent
2) Obtain final models of each bootstrap sample and compute the C-statistic of each bootstrap model, namely C-statistic-boot
3) Compute the C-statistic of each bootstrap model evaluated in the original case-study data, namely C-statistic-original

A more correct way is:
"Freeze" the model obtained in 2 and obtain the C-statistic of each bootstrap model evaluated in the original data, this means:
3a) use the coefficient estimates from bootstrap model to obtain predictions in original data:

Model 1: PHREG and STORE=item-store statement: requests that the fitted model be saved to an item store
Model 2: PLM restore=item-store created in 1)
         SCORE data=&DATAorig out=BSOUT predicted: score (Linear predictor) new observations based on the item store that was created in Model1

3b) use predictions in 3a) as covariate in model fitted in orginal data
Model 3: PHREG with MODEL statement and NOFIT option and ROC statement using PRED=predicted 
         This gives the same C-statistic as having the MODEL statement with covariate predicted

These updates do not cause any significant changes in our results

***/


*Define macro variables;
%let DATAorig=originaldata;
%let DATAboot=bsample3;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth status_iadldifdth status_walkdepdth death;
%let ALLTIME=time_adldepdth time_iadldifdth time_walkdepdth time2death;
%let ALLLABEL= adl iadl walk death; /*labels for outcomes*/
proc sql noprint; select max(replicate) format 3. into :S from baBICbs; quit; /*create macro variable with total number of bs dataset datasets*/
%put "&S"; 

%macro c_bs_ori(model=);
 %do i=1 %to &S;

  proc sql noprint; select (&i-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the first id of ith bs dataset*/
  proc sql noprint; select &i*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='BSAMPLE2'; quit; /*create macro variable with the last id of ith bs dataset*/

  data bsample3;
   set bsample2 (FIRSTOBS=&fobs OBS=&lobs);
  run;
  sasfile WORK.bsample3 load;

    /*For each bs dataset define VARNAME as the baBIC model*/
	  data _null_;
	   set baBICbs (keep=replicate VARINMODEL);
	   where replicate=&i;
	   call symputx ('VARNAME' , VARINMODEL);
	  run;

	  %do j=1 %to &NUMOUTCOMES;
	    %let OUTCOME=%scan(&ALLOUTCOME,&j); /*extract the jth outcome, jth time, jth label*/
	    %let TIME=%scan(&ALLTIME,&j);
		%let LABEL=%scan(&ALLLABEL,&j);

	    /*Get bootstrap model fitted in bootstrap data  */
	    proc phreg data = &DATAboot;
	      class &VARNAME;
	      model &time*&outcome(0) = &VARNAME;
		  store bootmodel; /*requests that the fitted model be saved to an item store  */
	    run;
        /*Get linear predictions (predicted) using fitted model above in original data  */
	    proc plm restore=bootmodel;
	     score data=&DATAorig out=BSOUT predicted; /* score (Linear predictor) new observations based on the item store bootmodel that was created above*/
	    run;
	    /*Using linear predictions "predicted" above compute the C-statistic-original*/
        proc phreg data = BSOUT CONCORDANCE=HARRELL; 
	     class &VARNAME;
         model &time*&outcome(0) = &VARNAME / nofit;
	     roc 'Original' pred=predicted;
	     ods output CONCORDANCE=concord;
        run;

		data CTABLE_&label;
	     set concord (keep= estimate rename=(estimate=cbs_ori_&label));
		 length VARINMODEL $1000;
	     replicate=&i;
		 VARINMODEL="&VARNAME";
	    run;

		proc delete data=concord BSOUT; run; quit;

     %end; /*j loop*/

	 sasfile WORK.bsample3 close;
     proc delete data=bsample3; run; quit;

	 data ctable; 
       merge CTABLE_adl CTABLE_iadl(drop=VARINMODEL) CTABLE_walk(drop=VARINMODEL) CTABLE_death(drop=VARINMODEL);
	   by replicate;
      cbs_ori_avg=mean(cbs_ori_adl, cbs_ori_iadl, cbs_ori_walk, cbs_ori_death);
     run;

	proc delete data=CTABLE_adl CTABLE_iadl CTABLE_walk CTABLE_death; run; quit;

    proc append base=ctable_ori_sim_&model data=ctable force; run;
    proc delete data=ctable; run; quit;

  %end; /*i loop*/

%mend c_bs_ori;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%c_bs_ori (model=baBIC);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

/*
======MONITORING: 2022-06-23, 9:43======
======MONITORING: 2022-06-23, 10:01======
*/

*Save permanent dataset;
data outdata.ctable_ori_sim_baBIC; set ctable_ori_sim_baBIC; run;

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

*baBIC model;
proc contents data=outdata.bicrep500; run;
proc contents data=outdata.ctable_ori_sim_baBIC; run;

proc print data=outdata.bicrep500 (obs=2); var VARINMODEL C_adl C_iadl C_walk C_death C_avg; run;
proc print data=outdata.ctable_ori_sim_baBIC (obs=2); run;

data cop_baBIC (keep=replicate optimism_baBIC_adl optimism_baBIC_iadl optimism_baBIC_walk optimism_baBIC_death optimism_baBIC_avg);
 merge outdata.bicrep500 (keep=replicate C_adl C_iadl C_walk C_death C_avg) 
       outdata.ctable_ori_sim_baBIC (keep=replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death cbs_ori_avg);
 by replicate;
 optimism_baBIC_adl=abs(C_adl-cbs_ori_adl);
 optimism_baBIC_iadl=abs(C_iadl-cbs_ori_iadl);
 optimism_baBIC_walk=abs(C_walk-cbs_ori_walk);
 optimism_baBIC_death=abs(C_death-cbs_ori_death);
 optimism_baBIC_avg=abs(C_avg-cbs_ori_avg);
run;

ods select all; 

*QC;
proc print data=outdata.bicrep500 (obs=3); var VARINMODEL replicate C_adl C_iadl C_walk C_death C_avg; run;
proc print data=outdata.ctable_ori_sim_baBIC (obs=3); var VARINMODEL replicate cbs_ori_adl cbs_ori_iadl cbs_ori_walk cbs_ori_death cbs_ori_avg; run;
proc print data=cop_baBIC (obs=3); var replicate optimism_baBIC_adl optimism_baBIC_iadl optimism_baBIC_walk optimism_baBIC_death optimism_baBIC_avg; run;
proc means data=cop_baBIC; run;

*Create permanent data set with baBIC optimism;
data outdata.c_opt_baBIC; set cop_baBIC; run;

*Compute average optimism;
proc means data=outdata.c_opt_baBIC stackodsoutput n mean std stderr clm maxdec=4; 
 var optimism_baBIC_adl optimism_baBIC_iadl optimism_baBIC_walk optimism_baBIC_death optimism_baBIC_avg;
 ods output summary=outdata.c_opt_baBIC_avg;
proc sort; by variable; run;

*Compute Optimism corrected C-statistic;

data cstat_original;
 set outdata.cstat_original;
 length Variable $32;
 if model="adl_baBIC" then Variable="optimism_baBIC_adl";
 else if model="iadl_baBIC" then Variable="optimism_baBIC_iadl";
 else if model="walk_baBIC" then Variable="optimism_baBIC_walk";
 else if model="death_baBIC" then Variable="optimism_baBIC_death";
 if Variable not =" ";
proc sort; by variable; run;

*Calculate C_stat_baBIC_avg in Original data;
proc means data=cstat_original stackodsoutput mean maxdec=4; 
 var Estimate;
 ods output summary=c_stat_baBIC_avg (keep=Mean rename=(Mean=Estimate));
run;
data c_stat_baBIC_avg; set c_stat_baBIC_avg; length Variable $32; Variable="optimism_baBIC_avg"; run;

data cstat_original; set cstat_original c_stat_baBIC_avg; proc sort; by variable; run;

data outdata.c_statCorrectbaBIC;
 merge cstat_original (keep=Variable Estimate) outdata.c_opt_baBIC_avg ;
 by Variable;
 C_stat_correct=Estimate-Mean;
proc sort; by variable; run;
proc print data=outdata.c_statCorrectbaBIC; run;

PROC EXPORT DATA= outdata.c_statCorrectbaBIC
            OUTFILE= "path\c_statCorrectbaBIC.csv" 
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


/**********************************
2. In the computational processes of theta_hat, we can obtain a bootstrap estimate of the sampling distribution of ??_hat_app from the B bootstrap samples.
   Compute the bootstrap confidence interval of theta_app from the bootstrap distribution, (theta_hat_app_L, theta_hat_app_U); 
   for the 95% confidence interval, they are typically calculated by the 2.5th and 97.5th percentiles of the bootstrap distribution.
*/

*Custom percentiles baBIC;
proc stdize data=outdata.bicrep500 PctlMtd=ORD_STAT outstat=baBIC pctlpts=2.5, 97.5;
 var C_adl C_iadl C_walk C_death ;
run;
 
data baBIC;
 set baBIC;
 where _type_ =: 'P';
run;

proc print data=baBIC noobs; run;

proc transpose data=baBIC out=wide ;
   by _type_;
   var C_adl C_iadl C_walk C_death;
run;
data wide (drop=_NAME_ _type_);
 set wide (rename=(col1=P2_5));
 length Variable $32;
 if _NAME_="C_adl" then Variable="optimism_baBIC_adl";
 else if _NAME_="C_iadl" then Variable="optimism_baBIC_iadl";
 else if _NAME_="C_walk" then Variable="optimism_baBIC_walk";
 else if _NAME_="C_death" then Variable="optimism_baBIC_death";

 if _type_="P97_5000" and Variable="optimism_baBIC_adl" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_baBIC_iadl" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_baBIC_walk" then P97_5=P2_5;
 else if _type_="P97_5000" and Variable="optimism_baBIC_death" then P97_5=P2_5;
run;
data wide2_5 (drop=p97_5);
 set wide ;
 where P97_5=.;
proc sort; by Variable; run;
data wide97_5 (drop=p2_5);
 set wide ;
 where P97_5 ne .;
proc sort; by Variable; run;
data baBIC;
 retain Variable P2_5 P97_5;
 merge wide2_5 wide97_5;
 by Variable;
run;
proc print data=baBIC; run;

proc delete data=wide wide2_5 wide97_5; run;

proc print data=outdata.c_statCorrectbaBIC; run;

data baBIC2;
 merge outdata.c_statCorrectbaBIC baBIC;
 by Variable;
 P2_5correct=P2_5-Mean; /*Mean: optimism estimate*/
 P97_5correct=P97_5-Mean;
run;
proc print data=baBIC2; run;

*Save permanent data;
data outdata.c_statcorrectbaBIC_percentiles;
 set baBIC2;
run;

PROC EXPORT DATA= outdata.c_statcorrectbaBIC_percentiles
            OUTFILE= "path\c_statcorrectbaBIC_percentiles.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
