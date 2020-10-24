****************************************************************************************************************************************************************************************************;
*Program: 8a.SAS_SimSce2CorrTrain                                                                                                                                                                   ;                                                               
*Purpose: Generate simulated training datasets with correlated outcomes and predictors from Best Individual models of HRS case-study data. Compute some statistics (Scenario 2, correlated outcomes);                                     
*Statisticians: Grisell Diaz-Ramirez and Siqi Gan   																                                                                                ;
*Finished: 2020.04.24																				                                                                                                ;
****************************************************************************************************************************************************************************************************;

/*Check system options specified at SAS invocation*/
proc options option=work; run;
proc options option=utilloc; run;
proc options group=(memory performance); run;

libname savedata "path";  
options nomlogic nomprint nomrecall nosymbolgen;
options nosource nonotes; /*nosource: suppress the listing of the SAS statements to the log, causes only source lines that contain errors to be written to the log*/
options noquotelenmax; /*supress warning: THE QUOTED STRING CURRENTLY BEING PROCESSED HAS BECOME MORE THAN 262 CHARACTERS LONG*/
ods select none; /*to create output data sets through the ODS OUTPUT statement and suppress the display of all output*/

*Modify dataset to apply Wolber modification to Competing-risk regression;
data finaldata;
 set savedata.originaldata;
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end; /*Longest possible time that respondents were followed*/
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;

/*Create dataset/outcome only with covariates in the final ADL individual model for all Ids*/
data finaldata2adl; 
  set finaldata (keep=newid dAGE SEX DIABETES DRIVE EDU EXERCISE INCONTINENCE OTHERARM OTHERLIFT OTHERSTOOP);
run;

*Generate random numbers from Multivariate Normal Distribution given correlation among outcomes;
***Generate correlation matrix;
data corrdata; set savedata.originaldata; run;
proc corr data=corrdata OUTP=pearson_corr;
 var time_adldepdth time_iadldifdth time_walkdepdth time2death;
run;

***Fix all the means to be equal to zero and SD=1;
proc print data=pearson_corr; run;
data pearson_corr2 (type=CORR);
 set pearson_corr;
 if _TYPE_="MEAN" then do; time_adldepdth=0; time_iadldifdth=0; time_walkdepdth=0; time2death=0; end;
 else if _TYPE_="STD" then do; time_adldepdth=1; time_iadldifdth=1; time_walkdepdth=1; time2death=1; end;
run;
proc print data=pearson_corr2; run;
***Use PROC SIMNORM to generate 4 normal random variates that have means=0, SD=1, and specified correlations;
proc simnorm data=pearson_corr2 outsim=ssim
 numreal = 2765500 seed = 12345;
 var time_adldepdth time_iadldifdth time_walkdepdth time2death;
run;
proc corr data=ssim;
 var time_adldepdth time_iadldifdth time_walkdepdth time2death;
run;

***Stack four outcomes in one column;
data out1 (rename=(time_adldepdth=time)); set ssim (keep=time_adldepdth); obs=_N_; run; 
data out2 (rename=(time_iadldifdth=time)); set ssim (keep=time_iadldifdth); obs=_N_; run; 
data out3 (rename=(time_walkdepdth=time)); set ssim (keep=time_walkdepdth); obs=_N_; run;
data out4 (rename=(time2death=time)); set ssim (keep=time2death); obs=_N_; run; 
data outall; set out1 out2 out3 out4; obs=_N_; run; 

***PROBNORM function returns the probability that an observation from the standard normal distribution is less than or equal to x;
data unidata2;
 set outall;
 u=probnorm(time);
run; 

*Create data frame for 5,531(N)*500(simulations)*4(outcomes)= 2,212,400 random S(t) values with 0-1 range;
data unidata (drop=i j k);
 do i=1 to 4;
  outcome=i;
  do j=1 to 5531;
   newid=j;
   do k=1 to 500;
     sim=k;
	 output;
   end;
  end;
 end;
run; 
data unidata; set unidata; obs=_N_;run;
data unidata3; merge unidata unidata2; by obs; run;
data unidata3; set unidata3 (drop=obs time); run;

*How many uniform values we have >=0.995;
proc means data=unidata3; var u;  where u>=0.995; run;
*How many uniform values we have <=0.01;
proc means data=unidata3; var u;  where u<=0.01; run;

*Change values of u>0.995 to u=0.99 or u<=0.01 to 0.01 or round u values to 2 decimal places;
data unidata3;
 set unidata3;
 if u>=0.995 then u=0.99;
 else if u<=0.01 then u=0.01;
 else u=round(u,0.01);
run;

proc delete data=corrdata pearson_corr pearson_corr2 ssim out1 out2 out3 out4 outall unidata unidata2; run; quit;

*Split and save unidata3 for each outcome;
data savedata.unidata3adl savedata.unidata3iadl savedata.unidata3walk savedata.unidata3death;
 set unidata3;
 if outcome=1 then output savedata.unidata3adl;
 else if outcome=2 then output savedata.unidata3iadl;
 else if outcome=3 then output savedata.unidata3walk;
 else if outcome=4 then output savedata.unidata3death;
run;

*IADL;
data savedata.unidata3iadl;
 set savedata.unidata3iadl;
 if outcome=2 then outcome=1;
run;
proc means data=savedata.unidata3iadl; var newid; class outcome sim; run;

*WALK;
data savedata.unidata3walk;
 set savedata.unidata3walk;
 if outcome=3 then outcome=1;
run;
proc means data=savedata.unidata3walk; var newid; class outcome sim; run;

*DEATH;
data savedata.unidata3death;
 set savedata.unidata3death;
 if outcome=4 then outcome=1;
run;
proc means data=savedata.unidata3death; var newid; class outcome sim; run;

proc delete data=unidata3; run; quit;

*Create fake dataset with S2=0.99-0.01;
data fake (drop=i);
 S2=1;
 do i=1 to 99;
  S2=round(S2-0.01,0.01);
  output;
 end;
run;

*For outcome of interest only (ADL);
data unidata3;
 set savedata.unidata3adl;
run;

sasfile WORK.unidata3.data load; /*load: opens the file, allocates the buffers, and reads the data into memory*/
sasfile WORK.fake load;

%macro SimGen;
 %let ORIGDATA=finaldata; /*original dataset*/
 %let ALLCOVDATA=finaldata2adl; /*datasets with covariates only*/
 %let UNIDATA=unidata3; /*dataset with simulated normal probabilities (correlated outcomes) / simulated uniform values (uncorrelated outcomes)*/
 %let NUMOUTCOMES=1; /*number of outcomes*/
 %let ALLOUTCOME=status_adldepdth; /*status*/
 %let ALLTIME=time_adldepdth;/*time-to-event*/
 %let ALLLABEL= adl; /*label*/
 %let N=5531; /*sample size of each simulated dataset*/
 %let SIM=500; /*number of simulations*/
 %let VARNAMEadl=dAGE SEX DIABETES DRIVE EDU EXERCISE INCONTINENCE OTHERARM OTHERLIFT OTHERSTOOP; /*name of variables in final BIC-model for adl*/

 %do i=1 %to &NUMOUTCOMES;
   %let COVDATA=%scan(&ALLCOVDATA,&i); /*extract the ith data*/
   %let OUTCOME=%scan(&ALLOUTCOME,&i); /*extract the ith outcome, and ith time*/
   %let TIME=%scan(&ALLTIME,&i);
   %let LABEL=%scan(&ALLLABEL,&i);

    proc phreg data = &ORIGDATA; /*get Si(t) at each time point for all Ids: longid*/
	 class &VARNAMEadl;
	 model &TIME*&OUTCOME(0) = &VARNAMEadl;
	 baseline out=longid(keep=newid &TIME S) covariates=&COVDATA survival=S;
    run;

   /*longid number of observations= number of time-to-events for each outcome*N(5,531)Ids.
	Example ADL: 1,838 time-to-event   *    5,531 =10,165,978
            IADL: 1,919 time-to-event  *    5,531 =10,613,989
            Walk: 999 time-to-event    *    5,531 =5,525,469
            Death: 3,514 time-to-event *    5,531 =19,435,934
   */

   sasfile WORK.longid.data load;

   %do j=1 %to &N;

     proc sql noprint; select (&j-1)*(nobs/&N)+1 into :fobs from dictionary.tables where libname='WORK' and memname='LONGID'; quit; /*create macro variable with the first obs of jth id*/
     proc sql noprint; select &j*(nobs/&N) into :lobs from dictionary.tables where libname='WORK' and memname='LONGID'; quit; /*create macro variable with the last obs of jth id*/

	 /*When longid is read, this is done from memory without additional I/O requests*/
     data longid_&i._&j (rename=(&TIME=time));
	 	set longid (FIRSTOBS=&fobs OBS=&lobs);
	 run;

	*Create maxtime variable for maximum follow-up time;
	 proc sql noprint;
	  select max(time) into :maxtime
	  from longid_&i._&j;
	 quit;

	*Create id variable for each id;
	data _null_;
	 set longid_&i._&j (keep=newid) point=nobs nobs=nobs;
	 call symputx ('id' ,newid);
	 output;
	 stop;
	run;

	 *Create coarse lookup table: from S<=0.99 to S<=0.01;
	 data longid_&i._&j (drop=count);
	  set longid_&i._&j;
	  retain count;
	  if _N_=1 then do; count=0.01; end; 
	  S2=round(1-count,0.01); /*keep the Survival percentiles with two decimal places*/
	  if S<=S2 then do; count+0.01; status=1; output; end; /*output the 1st time when condition S<=S2 is true and advance to new S2*/
	 run;

	 *Merge fake dataset with longid_&i._&j;
	 proc sql;
      create table longid_&i._&j.b as
      select a.*,
	         b.newid, b.time, b.status
	  from fake a left join longid_&i._&j b on a.S2=b.S2
      order by S2 descending;
 	 quit;

	 *Set time=maxtime if time=., status=0 if status=.;
	 data longid_&i._&j.b;
	  set longid_&i._&j.b;
	  if newid=. then newid=&id;
	  if time=. then time=&maxtime;
	  if status=. then status=0;
	 run;

	 proc delete data=longid_&i._&j; run; quit;
	 sasfile WORK.longid_&i._&j.b load;

     %do k=1 %to &SIM;
      *Select u value from 11,062,000 normal probabilities generated above [5,531(N)*500(simulations)*4(outcomes)= 11,062,000];
       proc sql noprint; /*create a macro variable with nth value from unidata3 to compare it with S(t)*/
        select u format=4.2 into :u
        from &UNIDATA
        where outcome=&i and newid=&j and sim=&k;
       quit;

       data id_&i._&j._&k (drop=S2);
        set longid_&i._&j.b;
		length outcome $5;
        where S2=&u;
        outcome="&label"; sim=&k;
       run;

	   %if &k=1 %then %do; /*in the 1st simulation(k=1): create dataset for each Id*/
	    proc append base=id_&i._&j data=id_&i._&j._&k force; run;
	    sasfile WORK.id_&i._&j load;
       %end;
	   %else %do; /*for the rest of the simulations: keep updating id_&i._&j dataset in memory*/
        proc append base=id_&i._&j data=id_&i._&j._&k force; run;
       %end; 

	   proc delete data=id_&i._&j._&k; run; quit;
     %end; /*end k loop*/

     sasfile WORK.longid_&i._&j.b close; /*Closes WORK.longid_&i._&j.b, and frees allocated buffers*/
	 proc delete data=longid_&i._&j.b; run; quit;
 
	 %if &j=1 %then %do; /*for the 1st Id: create dataset for outcome*/
	  proc append base=id_&label data=id_&i._&j force; run;
	  sasfile WORK.id_&label load;
     %end;
	 %else %do; /*for the rest of the ids: keep updating id_&label dataset in memory*/
      proc append base=id_&label data=id_&i._&j force; run;
     %end; 
     sasfile WORK.id_&i._&j close; /*Closes WORK.id_&i._&j, and frees allocated buffers*/
	 proc delete data=id_&i._&j; run; quit;

   %end; /*end j loop*/

 %PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
 sasfile WORK.longid close; /*Closes longid, and frees allocated buffers*/
 proc delete data=longid; run; quit;
 data savedata.id_&label; set id_&label; run; /*create permanent dataset id_&label*/
 sasfile WORK.id_&label close; 
 proc delete data=id_&label; run; quit;

%end; /*end i loop*/

%mend SimGen;


%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%SimGen;
%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

sasfile WORK.unidata3 close; 
proc delete data=unidata3; run; quit;

sasfile WORK.fake close; 
proc delete data=fake finaldata2adl; run; quit;

/***********************************************************************************************************************************************************************************/
*Do the same for IADL;

/*Create dataset/outcome only with covariates in  the final IADL individual model for all Ids*/
data finaldata2iadl; 
  set finaldata (keep=newid dAGE SEX COGDLRC3G DRIVE EDU HEARAID INCONTINENCE OTHERSIT SMOKING); 
run;

*Create fake dataset with S2=0.99-0.01;
data fake (drop=i);
 S2=1;
 do i=1 to 99;
  S2=round(S2-0.01,0.01);
  output;
 end;
run;

*For outcome of interest only (IADL);
data unidata3;
 set savedata.unidata3iadl;
run;

sasfile WORK.unidata3.data load; /*load: opens the file, allocates the buffers, and reads the data into memory*/ 
sasfile WORK.fake load;

%macro SimGen;
 %let ORIGDATA=finaldata; /*original dataset*/
 %let ALLCOVDATA=finaldata2iadl; /*datasets with covariates only*/
 %let UNIDATA=unidata3; /*dataset with simulated normal probabilities (correlated outcomes) / simulated uniform values (uncorrelated outcomes)*/
 %let NUMOUTCOMES=1; /*number of outcomes*/
 %let ALLOUTCOME=status_iadldifdth; /*status*/
 %let ALLTIME=time_iadldifdth;/*time-to-event*/
 %let ALLLABEL= iadl; /*label*/
 %let N=5531; /*sample size of each simulated dataset*/
 %let SIM=500; /*number of simulations*/
 %let VARNAMEiadl=dAGE SEX COGDLRC3G DRIVE EDU HEARAID INCONTINENCE OTHERSIT SMOKING; /*name of variables in final BIC-model for iadl*/

 %do i=1 %to &NUMOUTCOMES;
   %let COVDATA=%scan(&ALLCOVDATA,&i); /*extract the ith data*/
   %let OUTCOME=%scan(&ALLOUTCOME,&i); /*extract the ith outcome, and ith time*/
   %let TIME=%scan(&ALLTIME,&i);
   %let LABEL=%scan(&ALLLABEL,&i);

    proc phreg data = &ORIGDATA; /*get Si(t) at each time point for all Ids: longid*/
	 class &VARNAMEiadl;
	 model &TIME*&OUTCOME(0) = &VARNAMEiadl;
	 baseline out=longid(keep=newid &TIME S) covariates=&COVDATA survival=S;
    run;

   /*longid number of observations= number of time-to-events for each outcome*N(5,531)Ids.
	Example ADL: 1,838 time-to-event   *    5,531 =10,165,978
            IADL: 1,919 time-to-event  *    5,531 =10,613,989
            Walk: 999 time-to-event    *    5,531 =5,525,469
            Death: 3,514 time-to-event *    5,531 =19,435,934
   */

   sasfile WORK.longid.data load;

   %do j=1 %to &N;

     proc sql noprint; select (&j-1)*(nobs/&N)+1 into :fobs from dictionary.tables where libname='WORK' and memname='LONGID'; quit; /*create macro variable with the first obs of jth id*/
     proc sql noprint; select &j*(nobs/&N) into :lobs from dictionary.tables where libname='WORK' and memname='LONGID'; quit; /*create macro variable with the last obs of jth id*/

	 /*When longid is read, this is done from memory without additional I/O requests*/
     data longid_&i._&j (rename=(&TIME=time));
	 	set longid (FIRSTOBS=&fobs OBS=&lobs);
	 run;

	*Create maxtime variable for maximum follow-up time;
	 proc sql noprint;
	  select max(time) into :maxtime
	  from longid_&i._&j;
	 quit;

	*Create id variable for each id;
	data _null_;
	 set longid_&i._&j (keep=newid) point=nobs nobs=nobs;
	 call symputx ('id' ,newid);
	 output;
	 stop;
	run;

	 *Create coarse lookup table: from S<=0.99 to S<=0.01;
	 data longid_&i._&j (drop=count);
	  set longid_&i._&j;
	  retain count;
	  if _N_=1 then do; count=0.01; end; 
	  S2=round(1-count,0.01); /*keep the Survival percentiles with two decimal places*/
	  if S<=S2 then do; count+0.01; status=1; output; end; /*output the 1st time when condition S<=S2 is true and advance to new S2*/
	 run;

	 *Merge fake dataset with longid_&i._&j;
	 proc sql;
      create table longid_&i._&j.b as
      select a.*,
	         b.newid, b.time, b.status
	  from fake a left join longid_&i._&j b on a.S2=b.S2
      order by S2 descending;
 	 quit;

	 *Set time=maxtime if time=., status=0 if status=.;
	 data longid_&i._&j.b;
	  set longid_&i._&j.b;
	  if newid=. then newid=&id;
	  if time=. then time=&maxtime;
	  if status=. then status=0;
	 run;

	 proc delete data=longid_&i._&j; run; quit;
	 sasfile WORK.longid_&i._&j.b load;

     %do k=1 %to &SIM;
      *Select u value from 11,062,000 normal probabilities generated above [5,531(N)*500(simulations)*4(outcomes)= 11,062,000];
       proc sql noprint; /*create a macro variable with nth value from unidata3 to compare it with S(t)*/
        select u format=4.2 into :u
        from &UNIDATA
        where outcome=&i and newid=&j and sim=&k;
       quit;

       data id_&i._&j._&k (drop=S2);
        set longid_&i._&j.b;
		length outcome $5;
        where S2=&u;
        outcome="&label"; sim=&k;
       run;

	   %if &k=1 %then %do; /*in the 1st simulation(k=1): create dataset for each Id*/
	    proc append base=id_&i._&j data=id_&i._&j._&k force; run;
	    sasfile WORK.id_&i._&j load;
       %end;
	   %else %do; /*for the rest of the simulations: keep updating id_&i._&j dataset in memory*/
        proc append base=id_&i._&j data=id_&i._&j._&k force; run;
       %end; 

	   proc delete data=id_&i._&j._&k; run; quit;
     %end; /*end k loop*/

     sasfile WORK.longid_&i._&j.b close; /*Closes WORK.longid_&i._&j.b, and frees allocated buffers*/
	 proc delete data=longid_&i._&j.b; run; quit;
 
	 %if &j=1 %then %do; /*for the 1st Id: create dataset for outcome*/
	  proc append base=id_&label data=id_&i._&j force; run;
	  sasfile WORK.id_&label load;
     %end;
	 %else %do; /*for the rest of the ids: keep updating id_&label dataset in memory*/
      proc append base=id_&label data=id_&i._&j force; run;
     %end; 
     sasfile WORK.id_&i._&j close; /*Closes WORK.id_&i._&j, and frees allocated buffers*/
	 proc delete data=id_&i._&j; run; quit;

   %end; /*end j loop*/

 %PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
 sasfile WORK.longid close; /*Closes longid, and frees allocated buffers*/
 proc delete data=longid; run; quit;
 data savedata.id_&label; set id_&label; run; /*create permanent dataset id_&label*/
 sasfile WORK.id_&label close; 
 proc delete data=id_&label; run; quit;

%end; /*end i loop*/

%mend SimGen;


%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%SimGen;
%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

sasfile WORK.unidata3 close; 
proc delete data=unidata3; run; quit;

sasfile WORK.fake close; 
proc delete data=fake finaldata2iadl; run; quit;

/***********************************************************************************************************************************************************************************/
*Do the same for WALK;

/*Create dataset/outcome only with covariates in the final WALK individual model  for all Ids*/
data finaldata2walk; 
  set finaldata (keep=newid dAGE SEX DRIVE HYPERTENSION INCONTINENCE OTHERCLIM3G OTHERSIT); 
run;

*Create fake dataset with S2=0.99-0.01; 
data fake (drop=i);
 S2=1;
 do i=1 to 99;
  S2=round(S2-0.01,0.01);
  output;
 end;
run;

*For outcome of interest only (WALK);
data unidata3;
 set savedata.unidata3walk;
run;

sasfile WORK.unidata3.data load; /*load: opens the file, allocates the buffers, and reads the data into memory*/
sasfile WORK.fake load;

%macro SimGen;
 %let ORIGDATA=finaldata; /*original dataset*/
 %let ALLCOVDATA=finaldata2walk; /*datasets with covariates only*/
 %let UNIDATA=unidata3; /*dataset with simulated normal probabilities (correlated outcomes) / simulated uniform values (uncorrelated outcomes)*/
 %let NUMOUTCOMES=1; /*number of outcomes*/
 %let ALLOUTCOME=status_walkdepdth; /*status*/
 %let ALLTIME=time_walkdepdth;/*time-to-event*/
 %let ALLLABEL= walk; /*label*/
 %let N=5531; /*sample size of each simulated dataset*/
 %let SIM=500; /*number of simulations*/
 %let VARNAMEwalk=dAGE SEX DRIVE HYPERTENSION INCONTINENCE OTHERCLIM3G OTHERSIT; /*name of variables in final BIC-model for walk*/

 %do i=1 %to &NUMOUTCOMES;
   %let COVDATA=%scan(&ALLCOVDATA,&i); /*extract the ith data*/
   %let OUTCOME=%scan(&ALLOUTCOME,&i); /*extract the ith outcome, and ith time*/
   %let TIME=%scan(&ALLTIME,&i);
   %let LABEL=%scan(&ALLLABEL,&i);

    proc phreg data = &ORIGDATA; /*get Si(t) at each time point for all Ids: longid*/
	 class &VARNAMEwalk;
	 model &TIME*&OUTCOME(0) = &VARNAMEwalk;
	 baseline out=longid(keep=newid &TIME S) covariates=&COVDATA survival=S;
    run;

	/*longid number of observations= number of time-to-events for each outcome*N(5,531)Ids.
	Example ADL: 1,838 time-to-event   *    5,531 =10,165,978
            IADL: 1,919 time-to-event  *    5,531 =10,613,989
            Walk: 999 time-to-event    *    5,531 =5,525,469
            Death: 3,514 time-to-event *    5,531 =19,435,934
   */

   sasfile WORK.longid.data load;

   %do j=1 %to &N;

     proc sql noprint; select (&j-1)*(nobs/&N)+1 into :fobs from dictionary.tables where libname='WORK' and memname='LONGID'; quit; /*create macro variable with the first obs of jth id*/
     proc sql noprint; select &j*(nobs/&N) into :lobs from dictionary.tables where libname='WORK' and memname='LONGID'; quit; /*create macro variable with the last obs of jth id*/

	 /*When longid is read, this is done from memory without additional I/O requests*/
     data longid_&i._&j (rename=(&TIME=time));
	 	set longid (FIRSTOBS=&fobs OBS=&lobs);
	 run;

	*Create maxtime variable for maximum follow-up time;
	 proc sql noprint;
	  select max(time) into :maxtime
	  from longid_&i._&j;
	 quit;

	*Create id variable for each id;
	data _null_;
	 set longid_&i._&j (keep=newid) point=nobs nobs=nobs;
	 call symputx ('id' ,newid);
	 output;
	 stop;
	run;

	 *Create coarse lookup table: from S<=0.99 to S<=0.01;
	 data longid_&i._&j (drop=count);
	  set longid_&i._&j;
	  retain count;
	  if _N_=1 then do; count=0.01; end; 
	  S2=round(1-count,0.01); /*keep the Survival percentiles with two decimal places*/
	  if S<=S2 then do; count+0.01; status=1; output; end; /*output the 1st time when condition S<=S2 is true and advance to new S2*/
	 run;

	 *Merge fake dataset with longid_&i._&j;
	 proc sql;
      create table longid_&i._&j.b as
      select a.*,
	         b.newid, b.time, b.status
	  from fake a left join longid_&i._&j b on a.S2=b.S2
      order by S2 descending;
 	 quit;

	 *Set time=maxtime if time=., status=0 if status=.;
	 data longid_&i._&j.b;
	  set longid_&i._&j.b;
	  if newid=. then newid=&id;
	  if time=. then time=&maxtime;
	  if status=. then status=0;
	 run;

	 proc delete data=longid_&i._&j; run; quit;
	 sasfile WORK.longid_&i._&j.b load;

     %do k=1 %to &SIM;
      *Select u value from 11,062,000 normal probabilities generated above [5,531(N)*500(simulations)*4(outcomes)= 11,062,000];
       proc sql noprint; /*create a macro variable with nth value from unidata3 to compare it with S(t)*/
        select u format=4.2 into :u
        from &UNIDATA
        where outcome=&i and newid=&j and sim=&k;
       quit;

       data id_&i._&j._&k (drop=S2);
        set longid_&i._&j.b;
		length outcome $5;
        where S2=&u;
        outcome="&label"; sim=&k;
       run;

	   %if &k=1 %then %do; /*in the 1st simulation(k=1): create dataset for each Id*/
	    proc append base=id_&i._&j data=id_&i._&j._&k force; run;
	    sasfile WORK.id_&i._&j load;
       %end;
	   %else %do; /*for the rest of the simulations: keep updating id_&i._&j dataset in memory*/
        proc append base=id_&i._&j data=id_&i._&j._&k force; run;
       %end; 

	   proc delete data=id_&i._&j._&k; run; quit;
     %end; /*end k loop*/

     sasfile WORK.longid_&i._&j.b close; /*Closes WORK.longid_&i._&j.b, and frees allocated buffers*/
	 proc delete data=longid_&i._&j.b; run; quit;
 
	 %if &j=1 %then %do; /*for the 1st Id: create dataset for outcome*/
	  proc append base=id_&label data=id_&i._&j force; run;
	  sasfile WORK.id_&label load;
     %end;
	 %else %do; /*for the rest of the ids: keep updating id_&label dataset in memory*/
      proc append base=id_&label data=id_&i._&j force; run;
     %end; 
     sasfile WORK.id_&i._&j close; /*Closes WORK.id_&i._&j, and frees allocated buffers*/
	 proc delete data=id_&i._&j; run; quit;

   %end; /*end j loop*/

 %PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
 sasfile WORK.longid close; /*Closes longid, and frees allocated buffers*/
 proc delete data=longid; run; quit;
 data savedata.id_&label; set id_&label; run; /*create permanent dataset id_&label*/
 sasfile WORK.id_&label close; 
 proc delete data=id_&label; run; quit;

%end; /*end i loop*/

%mend SimGen;


%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%SimGen;
%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

sasfile WORK.unidata3 close; 
proc delete data=unidata3; run; quit;

sasfile WORK.fake close; 
proc delete data=fake finaldata2walk; run; quit;

/***********************************************************************************************************************************************************************************/
*Do the same for DEATH;

/*Create dataset/outcome only with covariates in the final Death individual model for all Ids*/  
data finaldata2death; 
  set finaldata (keep=newid dAGE SEX COGDLRC3G DIABETES DRIVE EXERCISE HEARTFAILURE HYPERTENSION LUNG MSTAT OTHERCLIM3G OTHERPUSH OTHERWALK qBMI SMOKING VOLUNTEER);
run;

*Create fake dataset with S2=0.99-0.01;
data fake (drop=i);
 S2=1;
 do i=1 to 99;
  S2=round(S2-0.01,0.01);
  output;
 end;
run;

*For outcome of interest only (DEATH);
data unidata3;
 set savedata.unidata3death;
run;

sasfile WORK.unidata3.data load; /*load: opens the file, allocates the buffers, and reads the data into memory*/
sasfile WORK.fake load;

%macro SimGen;
 %let ORIGDATA=finaldata; /*original dataset*/
 %let ALLCOVDATA=finaldata2death; /*datasets with covariates only*/
 %let UNIDATA=unidata3; /*dataset with simulated normal probabilities (correlated outcomes) / simulated uniform values (uncorrelated outcomes)*/
 %let NUMOUTCOMES=1; /*number of outcomes*/
 %let ALLOUTCOME=death; /*status*/
 %let ALLTIME=time2death;/*time-to-event*/
 %let ALLLABEL= death; /*label*/
 %let N=5531; /*sample size of each simulated dataset*/
 %let SIM=500; /*number of simulations*/
 %let VARNAMEdeath=dAGE SEX COGDLRC3G DIABETES DRIVE EXERCISE HEARTFAILURE HYPERTENSION LUNG MSTAT OTHERCLIM3G OTHERPUSH OTHERWALK qBMI SMOKING VOLUNTEER; /*name of variables in final BIC-model for death*/

 %do i=1 %to &NUMOUTCOMES;
   %let COVDATA=%scan(&ALLCOVDATA,&i); /*extract the ith data*/
   %let OUTCOME=%scan(&ALLOUTCOME,&i); /*extract the ith outcome, and ith time*/
   %let TIME=%scan(&ALLTIME,&i);
   %let LABEL=%scan(&ALLLABEL,&i);

    proc phreg data = &ORIGDATA; /*get Si(t) at each time point for all Ids: longid*/
	 class &VARNAMEdeath;
	 model &TIME*&OUTCOME(0) = &VARNAMEdeath;
	 baseline out=longid(keep=newid &TIME S) covariates=&COVDATA survival=S;
    run;

	/*longid number of observations= number of time-to-events for each outcome*N(5,531)Ids.
	Example ADL: 1,838 time-to-event   *    5,531 =10,165,978
            IADL: 1,919 time-to-event  *    5,531 =10,613,989
            Walk: 999 time-to-event    *    5,531 =5,525,469
            Death: 3,514 time-to-event *    5,531 =19,435,934
   */

   sasfile WORK.longid.data load;

   %do j=1 %to &N;

     proc sql noprint; select (&j-1)*(nobs/&N)+1 into :fobs from dictionary.tables where libname='WORK' and memname='LONGID'; quit; /*create macro variable with the first obs of jth id*/
     proc sql noprint; select &j*(nobs/&N) into :lobs from dictionary.tables where libname='WORK' and memname='LONGID'; quit; /*create macro variable with the last obs of jth id*/

	 /*When longid is read, this is done from memory without additional I/O requests*/
     data longid_&i._&j (rename=(&TIME=time));
	 	set longid (FIRSTOBS=&fobs OBS=&lobs);
	 run;

	*Create maxtime variable for maximum follow-up time;
	 proc sql noprint;
	  select max(time) into :maxtime
	  from longid_&i._&j;
	 quit;

	*Create id variable for each id;
	data _null_;
	 set longid_&i._&j (keep=newid) point=nobs nobs=nobs;
	 call symputx ('id' ,newid);
	 output;
	 stop;
	run;

	 *Create coarse lookup table: from S<=0.99 to S<=0.01;
	 data longid_&i._&j (drop=count);
	  set longid_&i._&j;
	  retain count;
	  if _N_=1 then do; count=0.01; end; 
	  S2=round(1-count,0.01); /*keep the Survival percentiles with two decimal places*/
	  if S<=S2 then do; count+0.01; status=1; output; end; /*output the 1st time when condition S<=S2 is true and advance to new S2*/
	 run;

	 *Merge fake dataset with longid_&i._&j;
	 proc sql;
      create table longid_&i._&j.b as
      select a.*,
	         b.newid, b.time, b.status
	  from fake a left join longid_&i._&j b on a.S2=b.S2
      order by S2 descending;
 	 quit;

	 *Set time=maxtime if time=., status=0 if status=.;
	 data longid_&i._&j.b;
	  set longid_&i._&j.b;
	  if newid=. then newid=&id;
	  if time=. then time=&maxtime;
	  if status=. then status=0;
	 run;

	 proc delete data=longid_&i._&j; run; quit;
	 sasfile WORK.longid_&i._&j.b load;

     %do k=1 %to &SIM;
      *Select u value from 11,062,000 normal probabilities generated above [5,531(N)*500(simulations)*4(outcomes)= 11,062,000];
       proc sql noprint; /*create a macro variable with nth value from unidata3 to compare it with S(t)*/
        select u format=4.2 into :u
        from &UNIDATA
        where outcome=&i and newid=&j and sim=&k;
       quit;

       data id_&i._&j._&k (drop=S2);
        set longid_&i._&j.b;
		length outcome $5;
        where S2=&u;
        outcome="&label"; sim=&k;
       run;

	   %if &k=1 %then %do; /*in the 1st simulation(k=1): create dataset for each Id*/
	    proc append base=id_&i._&j data=id_&i._&j._&k force; run;
	    sasfile WORK.id_&i._&j load;
       %end;
	   %else %do; /*for the rest of the simulations: keep updating id_&i._&j dataset in memory*/
        proc append base=id_&i._&j data=id_&i._&j._&k force; run;
       %end; 

	   proc delete data=id_&i._&j._&k; run; quit;
     %end; /*end k loop*/

     sasfile WORK.longid_&i._&j.b close; /*Closes WORK.longid_&i._&j.b, and frees allocated buffers*/
	 proc delete data=longid_&i._&j.b; run; quit;
 
	 %if &j=1 %then %do; /*for the 1st Id: create dataset for outcome*/
	  proc append base=id_&label data=id_&i._&j force; run;
	  sasfile WORK.id_&label load;
     %end;
	 %else %do; /*for the rest of the ids: keep updating id_&label dataset in memory*/
      proc append base=id_&label data=id_&i._&j force; run;
     %end; 
     sasfile WORK.id_&i._&j close; /*Closes WORK.id_&i._&j, and frees allocated buffers*/
	 proc delete data=id_&i._&j; run; quit;

   %end; /*end j loop*/

 %PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
 sasfile WORK.longid close; /*Closes longid, and frees allocated buffers*/
 proc delete data=longid; run; quit;
 data savedata.id_&label; set id_&label; run; /*create permanent dataset id_&label*/
 sasfile WORK.id_&label close; 
 proc delete data=id_&label; run; quit;

%end; /*end i loop*/

%mend SimGen;


%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;
%SimGen;
%PUT ======MORNITORING: %SYSFUNC(DATE(),YYMMDD10.), %LEFT(%SYSFUNC(TIME(),HHMM8.))======;

sasfile WORK.unidata3 close; 
proc delete data=unidata3; run; quit;

sasfile WORK.fake close; 
proc delete data=fake finaldata2death; run; quit;

/***********************************************************************************************************************************************************************************/
/***********************************************************************************************************************************************************************************/
*Combine results from all outcomes;

*Stack simulated datasets for all 4 outcomes;
data savedata.sim500corrByTrain;
  set savedata.id_adl savedata.id_iadl savedata.id_walk savedata.id_death;
run;
/*5,531(N)*500(simulations)*4(outcomes)= 11,062,000 observations and 5 variables: newid time status outcome sim*/

proc sort data=savedata.sim500corrByTrain out=simdata; by newid; run; 

*Merge with original dataset to get the covariates;
data simdata2; 
  merge savedata.originaldata (drop=time_adldepdth status_adldepdth time_iadldifdth status_iadldifdth time_walkdepdth status_walkdepdth time2death death) 
        simdata;
  by newid;
proc sort; by sim outcome newid; run;
proc contents data=simdata2; run;


/**************************************************************************** Correlation matrix among outcomes  *********************************************************************************/

*Modify dataset to apply Wolber modification to Competing-risk regression; 
data finaldata;
 set savedata.originaldata;
 if status_adldepdth=2 then do; status_adldepdth=0; time_adldepdth=15.0278689; end; /*Longest possible time that respondents were followed*/
 if status_iadldifdth=2 then do; status_iadldifdth=0; time_iadldifdth=15.0278689; end;
 if status_walkdepdth=2 then do; status_walkdepdth=0; time_walkdepdth=15.0278689; end;
run;

data finaldata2; set savedata.originaldata; sim=0; proc sort; by sim newid; run;

/*Change format of simulated dataset from long where outcomes are stacked to wide where outcomes are merged*/
*ADL;
data adl (keep=sim newid time_adldepdth status_adldepdth);
 set simdata2 (rename=(time=time_adldepdth status=status_adldepdth));
 where outcome="adl";
proc sort; by sim newid; run; 
*IADL;
data iadl (keep=sim newid time_iadldifdth status_iadldifdth);
 set simdata2 (rename=(time=time_iadldifdth status=status_iadldifdth));
 where outcome="iadl";
proc sort; by sim newid; run; 
*Walk;
data walk (keep=sim newid time_walkdepdth status_walkdepdth);
 set simdata2 (rename=(time=time_walkdepdth status=status_walkdepdth));
 where outcome="walk";
proc sort; by sim newid; run; 
*Death;
data death (keep=sim newid time2death death);
 set simdata2 (rename=(time=time2death status=death));
 where outcome="death";
proc sort; by sim newid; run; 

data simdata3;
 merge adl iadl walk death;
 by sim newid;
run; 

%macro corr (inputdata=, corr=);
data simdata4; set &inputdata; proc sort; by sim newid; run;
data simdata4; set finaldata2 simdata4; proc sort; by sim newid; run;

proc sql noprint; select max(sim) format 3. into :maxsim from simdata4; quit; /*create macro variable with total number of simulated datasets*/
%put "&maxsim"; 

proc sql noprint; select count (distinct sim) format 3. into :S from simdata4; quit; /*create macro variable with total number of simulated datasets plus original dataset &S=101*/
%put "&S"; 

 %do l=1 %to &S; /*do this for each simulated dataset up to the last simulated dataset*/
    proc sql noprint; select (&l-1)*(nobs/&S)+1 into :fobs from dictionary.tables where libname='WORK' and memname='SIMDATA4'; quit; /*create macro variable with the first id of lth simulation*/
    proc sql noprint; select &l*(nobs/&S) into :lobs from dictionary.tables where libname='WORK' and memname='SIMDATA4'; quit; /*create macro variable with the last id of lth simulation*/

    data simdata5;
	 set simdata4 (FIRSTOBS=&fobs OBS=&lobs);
	run;

	proc sql noprint; select sim into :sim from simdata5; quit; /*create macro variable with simulation number*/

	proc corr data=simdata5 OUTP=pearson_corr; var time_adldepdth time_iadldifdth time_walkdepdth time2death; run;
	data pearson_corr2 (keep=padl_iadl padl_walk padl_death piadl_walk piadl_death pwalk_death sim);
		set pearson_corr end=last;
		retain padl_iadl padl_walk padl_death piadl_walk piadl_death pwalk_death;
		if _NAME_='time_adldepdth' then do; padl_iadl=time_iadldifdth; padl_walk=time_walkdepdth; padl_death=time2death; end;
		else if _NAME_='time_iadldifdth' then do; piadl_walk=time_walkdepdth; piadl_death=time2death; end;
		else if _NAME_='time_walkdepdth' then pwalk_death=time2death;
		if last;
		sim=&sim;
	run;

    proc corr data=simdata5 OUTS=spearman_corr; var time_adldepdth time_iadldifdth time_walkdepdth time2death; run;
	data spearman_corr2 (keep=padl_iadl padl_walk padl_death piadl_walk piadl_death pwalk_death sim);
		set spearman_corr end=last;
		retain padl_iadl padl_walk padl_death piadl_walk piadl_death pwalk_death;
		if _NAME_='time_adldepdth' then do; padl_iadl=time_iadldifdth; padl_walk=time_walkdepdth; padl_death=time2death; end;
		else if _NAME_='time_iadldifdth' then do; piadl_walk=time_walkdepdth; piadl_death=time2death; end;
		else if _NAME_='time_walkdepdth' then pwalk_death=time2death;
		if last;
		sim=&sim;
	run;

	proc append base=PearsonSim&maxsim data=pearson_corr2 force; run;
	proc append base=SpearmanSim&maxsim data=spearman_corr2 force; run;

	proc delete data=pearson_corr2 pearson_corr spearman_corr2 spearman_corr simdata5; run; quit;

 %end;

 ***Pearson;
data averagep; set PearsonSim&maxsim; if _N_=1 then delete; run; /*Don't need the first row of this dataset: I want the average of the correlation in the simulated datasets-first row is from original data*/
proc sql;
     create table averagep2 as
     select mean (padl_iadl) as padl_iadl,
			mean (padl_walk) as padl_walk,
			mean (padl_death) as padl_death,
			mean (piadl_walk) as piadl_walk,
			mean (piadl_death) as piadl_death,
			mean (pwalk_death) as pwalk_death
	from averagep;
quit;

data savedata.PearsonSim&maxsim.&corr.By; set PearsonSim&maxsim averagep2; run;
PROC EXPORT DATA= savedata.PearsonSim&maxsim.&corr.By
            OUTFILE= "path\PearsonSim&maxsim.&corr.By.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
proc delete data=averagep averagep2 PearsonSim&maxsim; run; quit;

***Spearman;
data averages; set SpearmanSim&maxsim; if _N_=1 then delete; run;
proc sql;
     create table averages2 as
     select mean (padl_iadl) as padl_iadl,
			mean (padl_walk) as padl_walk,
			mean (padl_death) as padl_death,
			mean (piadl_walk) as piadl_walk,
			mean (piadl_death) as piadl_death,
			mean (pwalk_death) as pwalk_death
	from averages;
quit;
data savedata.SpearmanSim&maxsim.&corr.By; set SpearmanSim&maxsim averages2; run;
PROC EXPORT DATA= savedata.SpearmanSim&maxsim.&corr.By
            OUTFILE= "path\SpearmanSim&maxsim.&corr.By.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
proc delete data=averages averages2 SpearmanSim&maxsim simdata4; run; quit;

%mend corr;
%corr (inputdata=simdata3, corr=corr); /*Simulated datasets with correlated outcomes*/

/********************************************************************* Compare stats of simulated datasets vs original dataset  *****************************************************************************/
ods select all; /*to print results below*/
%let SIM=500; /*number of simulations*/
title;
ods rtf file="path\Results_Statssim500corrByTrainVsOriginal.rtf" startpage=no;
ods text= "Simulated datasets (S=&SIM)";
proc means data=simdata2 N min max mean std; var newid; class sim; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata N min max mean std; var newid; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) ADL";
proc freq data=simdata2 ; table status; where outcome="adl"; run;
ods text= "Original dataset with Wolber modification";
proc freq data=finaldata; table status_adldepdth; run;
ods text= "Original dataset without Wolber modification";
proc freq data=savedata.originaldata; table status_adldepdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) IADL";
proc freq data=simdata2 ; table status; where outcome="iadl"; run;
ods text= "Original dataset with Wolber modification";
proc freq data=finaldata; table status_iadldifdth; run;
ods text= "Original dataset without Wolber modification";
proc freq data=savedata.originaldata; table status_iadldifdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) WALK";
proc freq data=simdata2 ; table status; where outcome="walk"; run;
ods text= "Original dataset with Wolber modification";
proc freq data=finaldata; table status_walkdepdth; run;
ods text= "Original dataset without Wolber modification";
proc freq data=savedata.originaldata; table status_walkdepdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) DEATH";
proc freq data=simdata2 ; table status; where outcome="death"; run;
ods text= "Original dataset without Wolber modification";
proc freq data=savedata.originaldata; table death; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) ADL";
proc means data=simdata2 n min max mean std median p25 p75; var time; class status; where outcome="adl";  run;
ods text= "Original dataset with Wolber modification";
proc means data=finaldata n min max mean std median p25 p75; var time_adldepdth; class status_adldepdth; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75; var time_adldepdth; class status_adldepdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) IADL";
proc means data=simdata2 n min max mean std median p25 p75; var time; class status; where outcome="iadl";  run;
ods text= "Original dataset with Wolber modification";
proc means data=finaldata n min max mean std median p25 p75; var time_iadldifdth; class status_iadldifdth; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75; var time_iadldifdth; class status_iadldifdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) WALK";
proc means data=simdata2 n min max mean std median p25 p75; var time; class status; where outcome="walk";  run;
ods text= "Original dataset with Wolber modification";
proc means data=finaldata n min max mean std median p25 p75; var time_walkdepdth; class status_walkdepdth; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75; var time_walkdepdth; class status_walkdepdth; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) DEATH";
proc means data=simdata2 n min max mean std median p25 p75; var time; class status; where outcome="death";  run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75; var time2death; class death; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM)";
proc means data=simdata2 n min max mean std median p25 p75; var time; class outcome; run;
ods text= "Original dataset with Wolber modification";
proc means data=finaldata n min max mean std median p25 p75 nolabels; var time_adldepdth time2death time_iadldifdth time_walkdepdth ; run;
ods text= "Original dataset without Wolber modification";
proc means data=savedata.originaldata n min max mean std median p25 p75 nolabels; var time_adldepdth time2death time_iadldifdth time_walkdepdth ; run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) Pearson Correlation";
proc print data=savedata.pearsonsim500corrByTrain (firstobs=1 obs=10); run;
proc print data=savedata.pearsonsim500corrByTrain (firstobs=491 obs=502); run;

ods startpage=now;
ods text= "Simulated datasets (S=&SIM) Spearman Correlation";
proc print data=savedata.spearmansim500corrByTrain (firstobs=1 obs=10); run;
proc print data=savedata.spearmansim500corrByTrain (firstobs=491 obs=502); run;

ods rtf close;





