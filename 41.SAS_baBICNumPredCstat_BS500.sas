***********************************************************************************************************************************************************************************;
*Program: 41.SAS_baBICNumPredCstat_BS500                                                                                                                                           ;                                                               
*Purpose: Compute summary statistics for number of predictors and C-statistic for baBIC method in 500 bootstrap samples                                                            ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2021.03.01																				                                                                               ;
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

***************************************************** Calculate Optimism Corrected C-statistic for baBIC models ******************************************************************;

/*** Fit each of the BS model (obtained using BS data) on the original case study data and calculate their C-statistic (C-stat-Original) ***/

data originaldata; 
 set savedata.originaldata; 
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;

data baBICbs; 
 set outdata.bicrep500;
 numVarsfinsim=countw(VARINMODEL, '');
run;
proc means data=baBICbs n mean std stderr clm median p25 p75 maxdec=4; var numVarsfinsim; run;


*Custom percentiles for 95% bootstrap confidence interval:
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


*Define macro variables;
%let NUMOUTCOMES=4; /*number of outcomes*/
%let ALLOUTCOME=status_adldepdth status_iadldifdth status_walkdepdth death;
%let ALLTIME=time_adldepdth time_iadldifdth time_walkdepdth time2death;
%let ALLLABEL= adl iadl walk death; /*labels for outcomes*/
proc sql noprint; select max(replicate) format 3. into :S from baBICbs; quit; /*create macro variable with total number of bs dataset datasets*/
%put "&S"; 

%macro c_bs_ori(model=);
 %do i=1 %to &S;
 /*For each bs model define VARNAME as VARINMODEL*/

	  data _null_;
	   set baBICbs (keep=replicate VARINMODEL);
	   where replicate=&i;
	   call symputx ('VARNAME' , VARINMODEL);
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

    proc append base=ctable_ori_sim_&model data=ctable force; run;
    proc delete data=ctable; run; quit;

  %end; /*i loop*/

%mend c_bs_ori;

%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%c_bs_ori (model=baBIC);
%PUT ======MONITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

/*
======MONITORING: 2020-10-05, 14:52======
======MONITORING: 2020-10-05, 15:05======
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
