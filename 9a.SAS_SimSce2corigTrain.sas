***********************************************************************************************************************************************************************************;
*Program: 9a.SAS_SimSce2corigTrain                                                                                                                                                 ;
*Purpose: Import training simulated data sets under Scenario 2 (Individual method) with case-study censoring from R and obtain some general statistics                             ;                                     
*Statistician: Grisell Diaz-Ramirez																																				   ;
*Started: 2020.11.25																																							   ;
*Finished: 2021.01.28																																							   ;
***********************************************************************************************************************************************************************************;

libname savedata "path";
libname outtrain "path";

/*********************************************************************** Prepare simulated and orginal datasets to get stats  *********************************************************************************/

options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
*Import data with simulations done in R by "sim.survdata" function;
%macro impdata (inputdata=, rep=, outputdata=);
	%do i=1 %to &rep;
		PROC IMPORT OUT= temp
		            DATAFILE= "path/&inputdata.&i..csv" 
		            DBMS=CSV REPLACE;
		     GETNAMES=YES;
		     DATAROW=2; 
		RUN;

		data temp (rename=sim2=sim);
         set temp;
         sim2=sim+10*(&i-1);
		 drop sim;
        run;
 
        proc append base=&outputdata data=temp force; run;
		proc delete data=temp; run; quit;
	%end; 

	proc sort data=&outputdata; by newid; run;
%mend impdata;

%impdata(inputdata=sim10trainIndcorig_20210109_, rep=50, outputdata=outtrain.sim500trainIndcorig);
/*N(5531)*Sim(500)*4=11,062,000 and 7 variables.*/

*Merge with original dataset to get the covariates;
data simdata2; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) 
        outtrain.sim500trainIndcorig;
  by newid;
run;
/*N(5531)*Sim(500)*4=11,062,000 and 56+6=62 variables.*/
proc contents data=simdata2; run;

*QC;
proc freq data=simdata2;
 tables outcome*status*failed /list missing;
run;

proc means data=simdata2 n nmiss min max p25 median mean p75; 
 var time y;
 class outcome;
run;

proc freq data=simdata2; tables sim*outcome /list missing; run;

*Modify dataset to apply Wolber modification to Competing-risk regression;
data finaldata;
 set savedata.originaldata;
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;
/*5531 observations and 64 variables.*/


data finaldata2; set savedata.originaldata; sim=0; proc sort; by sim newid; run;

/*Change format of simulated dataset from long where outcomes are stacked to wide where outcomes are merged*/
*ADL;
data adl (keep=sim newid time_adldepdth status_adldepdth);
 set simdata2 (rename=(time=time_adldepdth status=status_adldepdth));
 where outcome="adl";
proc sort; by sim newid; run; /*2,765,500 observations and 4 variables.*/
*IADL;
data iadl (keep=sim newid time_iadldifdth status_iadldifdth);
 set simdata2 (rename=(time=time_iadldifdth status=status_iadldifdth));
 where outcome="iadl";
proc sort; by sim newid; run; /*2,765,500 observations and 4 variables.*/
*Walk;
data walk (keep=sim newid time_walkdepdth status_walkdepdth);
 set simdata2 (rename=(time=time_walkdepdth status=status_walkdepdth));
 where outcome="walk";
proc sort; by sim newid; run; /*2,765,500 observations and 4 variables.*/
*Death;
data death (keep=sim newid time2death death);
 set simdata2 (rename=(time=time2death status=death));
 where outcome="death";
proc sort; by sim newid; run; /*2,765,500 observations and 4 variables.*/

data simdata3;
 merge adl iadl walk death;
 by sim newid;
run; /*2,765,500 observations and 10 variables.*/


/********************************************************************* Compare stats of simulated datasets vs original dataset  *****************************************************************************/
ods select all; /*to print results below*/
%let SIM=500; /*number of simulations*/
title;
ods rtf file='path\Results_StatsSimTrainIndcorigVsOriginal.rtf' startpage=no;
ods text= "Simulated datasets (S=&SIM and original censoring)";
proc means data=simdata2 N min max mean std; var newid; class sim; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata N min max mean std; var newid; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring) ADL";
proc freq data=simdata2 ; table status; where outcome="adl"; run;
ods text= "Original dataset with Wolber modification";
proc freq data=finaldata; table status_adldepdth; run;
ods text= "Original dataset without Wolber modification";
proc freq data=savedata.originaldata; table status_adldepdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring) IADL";
proc freq data=simdata2 ; table status; where outcome="iadl"; run;
ods text= "Original dataset with Wolber modification";
proc freq data=finaldata; table status_iadldifdth; run;
ods text= "Original dataset without Wolber modification";
proc freq data=savedata.originaldata; table status_iadldifdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring) WALK";
proc freq data=simdata2 ; table status; where outcome="walk"; run;
ods text= "Original dataset with Wolber modification";
proc freq data=finaldata; table status_walkdepdth; run;
ods text= "Original dataset without Wolber modification";
proc freq data=savedata.originaldata; table status_walkdepdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring) DEATH";
proc freq data=simdata2 ; table status; where outcome="death"; run;
ods text= "Original dataset without Wolber modification";
proc freq data=savedata.originaldata; table death; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring) ADL";
proc means data=simdata2 n min max mean std median p25 p75; var time; class status; where outcome="adl";  run;
ods text= "Original dataset with Wolber modification";
proc means data=finaldata n min max mean std median p25 p75; var time_adldepdth; class status_adldepdth; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75; var time_adldepdth; class status_adldepdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring) IADL";
proc means data=simdata2 n min max mean std median p25 p75; var time; class status; where outcome="iadl";  run;
ods text= "Original dataset with Wolber modification";
proc means data=finaldata n min max mean std median p25 p75; var time_iadldifdth; class status_iadldifdth; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75; var time_iadldifdth; class status_iadldifdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring) WALK";
proc means data=simdata2 n min max mean std median p25 p75; var time; class status; where outcome="walk";  run;
ods text= "Original dataset with Wolber modification";
proc means data=finaldata n min max mean std median p25 p75; var time_walkdepdth; class status_walkdepdth; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75; var time_walkdepdth; class status_walkdepdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring) DEATH";
proc means data=simdata2 n min max mean std median p25 p75; var time; class status; where outcome="death";  run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75; var time2death; class death; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM and original censoring)";
proc means data=simdata2 n min max mean std median p25 p75; var time; class outcome; run;
ods text= "Original dataset with Wolber modification";
proc means data=finaldata n min max mean std median p25 p75 nolabels; var time_adldepdth time2death time_iadldifdth time_walkdepdth ; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75 nolabels; var time_adldepdth time2death time_iadldifdth time_walkdepdth ; run;

ods rtf close;


