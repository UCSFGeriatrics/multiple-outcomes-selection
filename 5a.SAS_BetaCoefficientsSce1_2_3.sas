***********************************************************************************************************************************************************************************;
*Program: 5a.SAS_BetaCoefficientsSce1_2_3                                                                                                                                          ;                                                               
*Purpose: Compute Beta coefficients used in Scenarios 1, 2, and 3 of the simulation study                                       						                           ;
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                               ;
*Finished: 2020.10.22																				                                                                               ;
***********************************************************************************************************************************************************************************;

libname savedata 'path';

data originaldata; 
 set savedata.originaldata;
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;

*Separate 4 outcomes in 4 datasets;
data adl (rename=(status_adldepdth=status time_adldepdth=time)) ;
 set originaldata(drop=status_iadldifdth status_walkdepdth death time_iadldifdth time_walkdepdth time2death);
 length outcome $16;
 outcome="adl";
run; /*5531 observations and 59 variables.*/
data iadl (rename=(status_iadldifdth=status time_iadldifdth=time)) ;
 set originaldata(drop=status_adldepdth status_walkdepdth death time_adldepdth time_walkdepdth time2death);
 length outcome $16;
 outcome="iadl";
run; /*5531 observations and 59 variables.*/
data walk (rename=(status_walkdepdth=status time_walkdepdth=time)) ;
 set originaldata(drop=status_adldepdth status_iadldifdth death time_adldepdth time_iadldifdth time2death);
 length outcome $16;
 outcome="walk";
run; /*5531 observations and 59 variables.*/
data death (rename=(death=status time2death=time)) ;
 set originaldata(drop=status_adldepdth status_iadldifdth status_walkdepdth  time_adldepdth time_iadldifdth time_walkdepdth );
 length outcome $16;
 outcome="death";
run; /*5531 observations and 59 variables.*/

*Stack all 3 datasets for competing-risk;
data compdata;
 set adl iadl walk death;
proc sort; by outcome newid; run; /*sort by outcome because this is important to use in BY statement of the PHREG procedure*/


/*************************************************************************************************************************************************************************************/
*baBIC model;

proc phreg data = compdata; 
	  by outcome; /*fit 4 regression models, eg.: adl, iadl, walk, death, one/outcome*/
      class  dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE COGDLRC3G SMOKING OTHERCLIM3G HEARTFAILURE  LUNG OTHERPUSH qBMI VOLUNTEER;
      model time*status(0) =  dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE COGDLRC3G SMOKING OTHERCLIM3G HEARTFAILURE  LUNG OTHERPUSH qBMI VOLUNTEER ;
	  ods output ParameterEstimates=betas; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Export betas data to csv;
PROC EXPORT DATA= betas
            OUTFILE= "path\coefTablebaBICModel.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc delete data=betas; run; quit;


*************************************************************************************************************************************************************************************/
*Individual model;

*Individual model ADL;
proc phreg data = adl; 
      class dAGE SEX DRIVE INCONTINENCE EDU DIABETES  EXERCISE  OTHERARM OTHERLIFT OTHERSTOOP  ;
      model time*status(0) = dAGE SEX DRIVE INCONTINENCE EDU DIABETES  EXERCISE  OTHERARM OTHERLIFT OTHERSTOOP ;
	  ods output ParameterEstimates=betas_adl; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Individual model IADL;
proc phreg data = iadl; 
      class dAGE SEX DRIVE INCONTINENCE EDU OTHERSIT COGDLRC3G SMOKING HEARAID ;
      model time*status(0) = dAGE SEX DRIVE INCONTINENCE EDU OTHERSIT COGDLRC3G SMOKING HEARAID  ;
	  ods output ParameterEstimates=betas_iadl; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Individual model WALK;
proc phreg data = walk; 
      class dAGE SEX DRIVE INCONTINENCE OTHERSIT HYPERTENSION OTHERCLIM3G ;
      model time*status(0) =  dAGE SEX DRIVE INCONTINENCE OTHERSIT HYPERTENSION OTHERCLIM3G ;
	  ods output ParameterEstimates=betas_walk; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Individual model: death;
proc phreg data = death; 
      class dAGE SEX DRIVE DIABETES EXERCISE COGDLRC3G SMOKING HYPERTENSION OTHERCLIM3G HEARTFAILURE  LUNG MSTAT  OTHERPUSH OTHERWALK qBMI VOLUNTEER  ;
      model time*status(0) = dAGE SEX DRIVE DIABETES EXERCISE COGDLRC3G SMOKING HYPERTENSION OTHERCLIM3G HEARTFAILURE  LUNG MSTAT  OTHERPUSH OTHERWALK qBMI VOLUNTEER ;
	  ods output ParameterEstimates=betas_death; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

data betas_adl; set betas_adl (drop=label);  length outcome $8; outcome="adl"; run;
data betas_iadl; set betas_iadl (drop=label);  length outcome $8; outcome="iadl"; run;
data betas_walk; set betas_walk (drop=label);  length outcome $8; outcome="walk"; run;
data betas_death; set betas_death (drop=label);  length outcome $8; outcome="death"; run;

data betas; set betas_adl betas_iadl betas_walk betas_death; run;


*Export betas data to csv;
PROC EXPORT DATA= betas
            OUTFILE= "path\coefTableIndModel.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc delete data=betas betas_adl betas_iadl betas_walk betas_death; run; quit;

/*************************************************************************************************************************************************************************************/
*Full model;
proc phreg data = compdata; 
	  by outcome; /*fit 4 regression models, eg.: adl, iadl, walk, death, one/outcome*/
      class dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT COGDLRC3G SMOKING HYPERTENSION OTHERCLIM3G OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE LUNG MSTAT
OTHERPUSH OTHERWALK qBMI VOLUNTEER ALCOHOL ARTHRITIS CANCER COGIMRC3G EYE2G FALL HEARING LALONE OTHERCHAIR OTHERDIME PAIN CESDALL  qFAGE qMAGE SHLT STROKE ;
      model time*status(0) = dAGE SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT COGDLRC3G SMOKING HYPERTENSION OTHERCLIM3G OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE LUNG MSTAT
OTHERPUSH OTHERWALK qBMI VOLUNTEER ALCOHOL ARTHRITIS CANCER COGIMRC3G EYE2G FALL HEARING LALONE OTHERCHAIR OTHERDIME PAIN CESDALL  qFAGE qMAGE SHLT STROKE ;
	  ods output ParameterEstimates=betas; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Export betas data to csv;
PROC EXPORT DATA= betas
            OUTFILE= "path\coefTableFullModel.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc delete data=betas; run; quit;


