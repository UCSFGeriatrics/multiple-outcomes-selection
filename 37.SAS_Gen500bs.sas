***********************************************************************************************************************************************************************************;
*Program: 37.SAS_Gen500bs                                                                                                                                                          ;                                                               
*Purpose: Generation of 500 bootstrap samples from HRS case-study data                                                                                                             ;                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2021.01.28																				                                                                               ;
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
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

*Create input data;
data input;
 set savedata.originaldata (keep=newid RAESTRAT RAEHSAMP);
proc sort; by RAESTRAT RAEHSAMP; run;
/*  5,531 observations and 3 variables  */


/********************************************************************************************************************************************************************************/
/*** Create bootstrap samples using PROC SURVEYSELECT ***/

*Create sample size input data set to provide the stratum sample sizes in the bootstrapping; 
proc freq data=input noprint;
  tables RAESTRAT*RAEHSAMP/out=nsize(rename=(count=_nsize_));
run;

*Run SURVEYSELECT to generate data with replicated Ids;
proc surveyselect data=input out=bsample method=urs sampsize=nsize reps=500 outhits seed=1953;
 strata RAESTRAT RAEHSAMP;
run;
/*Total Sample size is : 5,531*500=2,765,500 observations and 10 variables.  */

/*
Notes: 
sampsize=nsize: so that the bootstrap samples have the same number of Ids within each combination of RAESTRAT*RAEHSAMP
outhits:
	includes a distinct copy of each selected unit in the OUT= output data set when the same sampling unit is selected more than once.
	By default, the output data set contains a single copy of each unit selected, even when a unit is selected more than once, 
	and the variable NumberHits records the number of hits (selections) for each unit.
	If you specify the OUTHITS option, the output data set contains m copies of a sampling unit for which NumberHits is m;
	for example, the output data set contains three copies of a unit that is selected three times (NumberHits is 3).
*/


/*QC*/
*How many Ids in original data are selected in 500 BS data at least one? A\ All Ids are selected in at least one BS;
proc sort data=bsample out=test; by newid; run; /*2765500 observations and 7 variables.*/
data test;
 set test;
 by newid;
 if first.newid;
run;
/*
NOTE: There were 2765500 observations read from the data set WORK.TEST.
NOTE: The data set WORK.TEST has 5531 observations and 7 variables.
*/

proc contents data=bsample; run;
proc print data=bsample (obs=20); run;

 *Create permanent dataset with BS samples ;
data outdata.bs500 (keep=newid Replicate);
 set  bsample;
proc sort; by newid; run;
/*2,765,500 observations and 2 variables.*/



