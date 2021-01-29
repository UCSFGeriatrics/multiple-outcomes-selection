***********************************************************************************************************************************************************************************;
*Program: 5a.SAS_BetaCoefficientsSce1_2_3                                                                                                                                          ;                                                               
*Purpose: Create coefficient tables for Scenario 1, 2 and 3  and covariates data to use in simulations                                                                             ;
*Statistician: Grisell Diaz-Ramirez																																				   ;
*Started: 2017.10.09																																							   ;
*Finished: 2021.01.28																																							   ;
***********************************************************************************************************************************************************************************;

libname savedata 'path';

data finaldata; 
 set savedata.originaldata;
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end;
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;

/*************************************************************************************************************************************************************************************/
*Generate covariates table with dummy variables for categorical covariates with more than 2 levels ;

data covariates;
	array age(10)    (10*0); /*set all the elements of the array= 0 */
	array alcoh(3)   (3*0); 
	array canc(3)    (3*0); 
	array cogdl(3)   (3*0); 
	array cogim(3)   (3*0); 
	array fal(3)     (3*0); 
	array hear(5)    (5*0); 
	array lun(3)     (3*0); 
	array climb(3)   (3*0); 
	array walk(3)    (3*0);
	array bmi(5)     (5*0); 
	array father(4)  (4*0);
	array mother(4)  (4*0);
	array health(5)  (5*0);
	array smoke(3)   (3*0); 
	array strok(3)   (3*0); 

	set finaldata;

    COGDLRC3G=COGDLRC3G+1; /* categorical predictors need to be 1,2,3,etc, instead of 0,1,2 */
	COGIMRC3G=COGIMRC3G+1; 

	if dAGE ne . then age(dAGE) = 1; /* if dAGE = 3, then dAGE(age) = 1 will assign a value of 1 to the third element in the array, i.e., assign 1 to age3 */
	if ALCOHOL ne . then alcoh(ALCOHOL) = 1; 
	if CANCER ne . then canc(CANCER) = 1; 
	if COGDLRC3G ne . then cogdl(COGDLRC3G) = 1; 
	if COGIMRC3G ne . then cogim(COGIMRC3G) = 1; 
	if FALL ne . then fal(FALL) = 1; 
	if HEARING ne . then hear(HEARING) = 1; 
	if LUNG ne . then lun(LUNG) = 1; 
	if OTHERCLIM3G ne . then climb(OTHERCLIM3G) = 1; 
	if OTHERWALK ne . then walk(OTHERWALK) = 1; 
	if qBMI ne . then bmi(qBMI) = 1; 
	if qFAGE ne . then father(qFAGE) = 1; 
	if qMAGE ne . then mother(qMAGE) = 1; 
	if SHLT ne . then health(SHLT) = 1; 
	if SMOKING ne . then smoke(SMOKING) = 1; 
	if STROKE ne . then strok(STROKE) = 1; 

    output;

	if dAGE ne . then age(dAGE) = 0; /* need to reset age3=0 for the rest of the records */
	if ALCOHOL ne . then alcoh(ALCOHOL) =0; 
	if CANCER ne . then canc(CANCER) =0; 
	if COGDLRC3G ne . then cogdl(COGDLRC3G) =0; 
	if COGIMRC3G ne . then cogim(COGIMRC3G) =0; 
	if FALL ne . then fal(FALL) =0; 
	if HEARING ne . then hear(HEARING) =0; 
	if LUNG ne . then lun(LUNG) =0; 
	if OTHERCLIM3G ne . then climb(OTHERCLIM3G) =0; 
	if OTHERWALK ne . then walk(OTHERWALK) =0; 
	if qBMI ne . then bmi(qBMI) =0; 
	if qFAGE ne . then father(qFAGE) =0; 
	if qMAGE ne . then mother(qMAGE) =0; 
	if SHLT ne . then health(SHLT) =0; 
	if SMOKING ne . then smoke(SMOKING) =0; 
	if STROKE ne . then strok(STROKE) =0; 
run;
/*5531 observations and 127 variables.*/

*QC;
proc freq data=covariates; tables dAGE*age1*age2*age3*age4*age5*age6*age7*age8*age9*age10 /list; run;
proc freq data=covariates; tables ALCOHOL*alcoh1*alcoh2*alcoh3 /list; run;
proc freq data=covariates; tables CANCER*canc1*canc2*canc3 /list; run;
proc freq data=covariates; tables COGDLRC3G*cogdl1*cogdl2*cogdl3 /list; run;
proc freq data=covariates; tables COGIMRC3G*cogim1*cogim2*cogim3 /list; run;
proc freq data=covariates; tables FALL*fal1*fal2*fal3 /list; run;
proc freq data=covariates; tables HEARING*hear1*hear2*hear3*hear4*hear5 /list; run;
proc freq data=covariates; tables lung*lun1*lun2*lun3 /list; run;
proc freq data=covariates; tables OTHERCLIM3G*climb1*climb2*climb3 /list; run;
proc freq data=covariates; tables OTHERWALK*walk1*walk2*walk3 /list; run;
proc freq data=covariates; tables qBMI*bmi1*bmi2*bmi3*bmi4*bmi5 /list; run;
proc freq data=covariates; tables qFAGE*father1*father2*father3*father4 /list; run;
proc freq data=covariates; tables qMAGE*mother1*mother2*mother3*mother4 /list; run;
proc freq data=covariates; tables SHLT*health1*health2*health3*health4*health5 /list; run;
proc freq data=covariates; tables SMOKING*smoke1*smoke2*smoke3 /list; run;
proc freq data=covariates; tables STROKE*strok1*strok2*strok3 /list; run;

*Separate 4 outcomes in 4 datasets;
data adl (rename=(status_adldepdth=status time_adldepdth=time)) ;
 set covariates(drop=status_iadldifdth status_walkdepdth death time_iadldifdth time_walkdepdth time2death);
 length outcome $16;
 outcome="adl";
run; /*5531 observations and 59 variables.*/
data iadl (rename=(status_iadldifdth=status time_iadldifdth=time)) ;
 set covariates(drop=status_adldepdth status_walkdepdth death time_adldepdth time_walkdepdth time2death);
 length outcome $16;
 outcome="iadl";
run; /*5531 observations and 59 variables.*/
data walk (rename=(status_walkdepdth=status time_walkdepdth=time)) ;
 set covariates(drop=status_adldepdth status_iadldifdth death time_adldepdth time_iadldifdth time2death);
 length outcome $16;
 outcome="walk";
run; /*5531 observations and 59 variables.*/
data death (rename=(death=status time2death=time)) ;
 set covariates(drop=status_adldepdth status_iadldifdth status_walkdepdth  time_adldepdth time_iadldifdth time_walkdepdth );
 length outcome $16;
 outcome="death";
run; /*5531 observations and 122 variables.*/

*Stack all 4 datasets;
data compdata;
 set adl iadl walk death;
proc sort; by outcome newid; run; /*sort by outcome because this is important to use in BY statement of the PHREG procedure*/

data covariates;
 set covariates(keep=newid age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT cogdl1 cogdl2 smoke1 smoke2
                             HYPERTENSION climb1 climb2 OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE lun1 lun2 MSTAT
                             OTHERPUSH walk1 walk2 bmi1 bmi2 bmi3 bmi4 VOLUNTEER alcoh1 alcoh2 ARTHRITIS canc1 canc2 cogim1 cogim2 EYE2G
                             fal1 fal2 hear1 hear2 hear3 hear4 LALONE OTHERCHAIR OTHERDIME PAIN CESDALL  father1 father2 father3 mother1 mother2 mother3
                             health1 health2 health3 health4 strok1 strok2);
proc sort; by newid; run;
/*5531 observations and 71 variables*/

PROC EXPORT DATA= covariates
            OUTFILE= "path\covariates.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


/*************************************************************************************************************************************************************************************/
*baBIC model;

proc phreg data = compdata; 
	  by outcome; /*fit 4 regression models, eg.: adl, iadl, walk, death, one/outcome*/
	  class SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE HEARTFAILURE OTHERPUSH VOLUNTEER;
      model time*status(0) =  age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE cogdl1 cogdl2 smoke1 smoke2 climb1 climb2
                              HEARTFAILURE lun1 lun2 OTHERPUSH bmi1 bmi2 bmi3 bmi4 VOLUNTEER ;
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
      class SEX DRIVE INCONTINENCE EDU DIABETES  EXERCISE  OTHERARM OTHERLIFT OTHERSTOOP  ;
      model time*status(0) = age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU DIABETES  EXERCISE OTHERARM OTHERLIFT OTHERSTOOP ;
	  ods output ParameterEstimates=betas_adl; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Individual model IADL;
proc phreg data = iadl; 
      class SEX DRIVE INCONTINENCE EDU OTHERSIT HEARAID ;
      model time*status(0) = age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU OTHERSIT cogdl1 cogdl2 smoke1 smoke2 HEARAID  ;
	  ods output ParameterEstimates=betas_iadl; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Individual model WALK;
proc phreg data = walk; 
      class SEX DRIVE INCONTINENCE OTHERSIT HYPERTENSION ;
      model time*status(0) =  age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE OTHERSIT HYPERTENSION climb1 climb2 ;
	  ods output ParameterEstimates=betas_walk; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Individual model: death;
proc phreg data = death; 
      class SEX DRIVE DIABETES EXERCISE HYPERTENSION HEARTFAILURE  MSTAT  OTHERPUSH VOLUNTEER  ;
      model time*status(0) = age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE DIABETES EXERCISE cogdl1 cogdl2 smoke1 smoke2 HYPERTENSION climb1 climb2 HEARTFAILURE
                              lun1 lun2 MSTAT  OTHERPUSH walk1 walk2 bmi1 bmi2 bmi3 bmi4 VOLUNTEER ;
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
      class SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT HYPERTENSION OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE MSTAT
            OTHERPUSH VOLUNTEER ARTHRITIS EYE2G LALONE OTHERCHAIR OTHERDIME PAIN CESDALL ;

      model time*status(0) = age1 age2 age3 age4 age5 age6 age7 age8 age9 SEX DRIVE INCONTINENCE EDU DIABETES EXERCISE OTHERSIT cogdl1 cogdl2 smoke1 smoke2
                             HYPERTENSION climb1 climb2 OTHERARM OTHERLIFT  OTHERSTOOP HEARAID HEARTFAILURE lun1 lun2 MSTAT
                             OTHERPUSH walk1 walk2 bmi1 bmi2 bmi3 bmi4 VOLUNTEER alcoh1 alcoh2 ARTHRITIS canc1 canc2 cogim1 cogim2 EYE2G
                             fal1 fal2 hear1 hear2 hear3 hear4 LALONE OTHERCHAIR OTHERDIME PAIN CESDALL  father1 father2 father3 mother1 mother2 mother3
                             health1 health2 health3 health4 strok1 strok2 ;
	  ods output ParameterEstimates=betas; /*"betas" dataset, variables of interest: Parameter, Estimate, StdErr, ProbChiSq, Label*/
run; 

*Export betas data to csv;
PROC EXPORT DATA= betas
            OUTFILE= "path\coefTableFullModel.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc delete data=betas; run; quit;


