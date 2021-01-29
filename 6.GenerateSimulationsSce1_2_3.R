############### Multiple outcomes: Simulations   #########################
setwd("path")

#install.packages("coxed")
#install.packages("tidyverse")
library(coxed)
library(tidyverse)

#As of 1/28/2021 The newest version of the package with latest updates is available on GitHub: https://github.com/jkropko/coxed.
#install.packages("devtools")
#library(devtools)
#install_github("jkropko/coxed") 
#install_github("jkropko/coxed", lib="path")

covariates <- read.csv('covariates.csv')

#####################################################################################################################################
############### Scenario 1: baBIC method   ##########################################################################################

#Select the covariates in the same order as they appear in the beta vector
covarsbaBIC <- dplyr::select(covariates, age1, age2, age3, age4, age5, age6, age7, age8, age9, SEX, DRIVE, INCONTINENCE,
                                         EDU, DIABETES, EXERCISE, cogdl1, cogdl2, smoke1, smoke2, climb1, climb2,
                                         HEARTFAILURE, lun1, lun2, OTHERPUSH, bmi1, bmi2, bmi3, bmi4, VOLUNTEER)
#Create vectors with betas
betas_baBIC<-read.csv('coefTablebaBICModel.csv')
betas_baBICadl<-betas_baBIC$Estimate[betas_baBIC$outcome=="adl"]
betas_baBICiadl<-betas_baBIC$Estimate[betas_baBIC$outcome=="iadl"]
betas_baBICwalk<-betas_baBIC$Estimate[betas_baBIC$outcome=="walk"]
betas_baBICdeath<-betas_baBIC$Estimate[betas_baBIC$outcome=="death"]
rm(betas_baBIC)

#Function to process data
createSimData <- function(simulation, outcome, dataname) {
  
  #Create a new list and add datasets with simulation results to the new list
  outcomelist = list()
  for (i in 1:length(simulation)) {
    outcomedat <- cbind(simulation[[i]]$data, newid=covariates$newid, sim=i, outcome=outcome)
    outcomelist[[i]] <- outcomedat # add outcomedat dataset to your list
  }
  
  #Create data with all simulated datasets that are in the list
  outcome_all = do.call(rbind, outcomelist)
  
  #Create new time and status variables
  outcome_all<-mutate(outcome_all, time = y/365.25, status=(ifelse(failed=="TRUE", 1, 0)) )
  
  #Select only variables of interest
  finaldata <- dplyr::select(outcome_all, newid, sim, outcome, time, status, y, failed )
  
  #Save finaldata and assign new name
  assign(deparse(substitute(dataname)), finaldata, envir=.GlobalEnv)
  
}

#Simulate uncorrelated outcomes by using the function 4 times in a row
#Successive runs of the simulation function are independent, so the outcomes should be independent in expectation
set.seed(20210109)

######################################################################
#Scenario 1 censored=original train
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later
  
  simbaBICtrain_adl <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICadl, censor=0.6655, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simbaBICtrain_iadl <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICiadl, censor=0.6498, num.data.frames = 10) 
  simbaBICtrain_walk <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICwalk, censor=0.8190, num.data.frames = 10) 
  simbaBICtrain_death <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICdeath, censor=0.3187, num.data.frames = 10)
  
  #Process simulation object
  createSimData (simulation=simbaBICtrain_adl, outcome='adl', dataname=baBIC_adl) 
  createSimData (simulation=simbaBICtrain_iadl, outcome='iadl', dataname=baBIC_iadl)
  createSimData (simulation=simbaBICtrain_walk, outcome='walk', dataname=baBIC_walk)
  createSimData (simulation=simbaBICtrain_death, outcome='death', dataname=baBIC_death)
  
  #Stack 4 datasets
  total <- rbind(baBIC_adl,baBIC_iadl,baBIC_walk, baBIC_death)
  #table(total$outcome, total$status)
  #table(total$outcome)
  
  #Export total dataset
  write.csv(total, file = paste('sim10trainbaBICcorig_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  
  
  rm(simbaBICtrain_adl, simbaBICtrain_iadl, simbaBICtrain_walk, simbaBICtrain_death, baBIC_adl,baBIC_iadl,baBIC_walk, baBIC_death, total)
  
}
# Stop the clock
proc.time() - ptm
#  user    system  elapsed
#56846.54  3520.17 60453.04

rm(r, rep,ptm)

####################################
# Scenario 1: baBIC original censored test
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later

  simbaBICtest_adl <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICadl, censor=0.6655, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simbaBICtest_iadl <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICiadl, censor=0.6498, num.data.frames = 10) 
  simbaBICtest_walk <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICwalk, censor=0.8190, num.data.frames = 10) 
  simbaBICtest_death <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICdeath, censor=0.3187, num.data.frames = 10)

  #Process simulation object
  createSimData (simulation=simbaBICtest_adl, outcome='adl', dataname=baBIC_adl) 
  createSimData (simulation=simbaBICtest_iadl, outcome='iadl', dataname=baBIC_iadl)
  createSimData (simulation=simbaBICtest_walk, outcome='walk', dataname=baBIC_walk)
  createSimData (simulation=simbaBICtest_death, outcome='death', dataname=baBIC_death)

  #Stack 4 datasets
  total <- rbind(baBIC_adl,baBIC_iadl,baBIC_walk, baBIC_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10testbaBICcorig_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  rm(simbaBICtest_adl,simbaBICtest_iadl,simbaBICtest_walk,simbaBICtest_death, baBIC_adl,baBIC_iadl,baBIC_walk, baBIC_death, total)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#56795.96  3554.88 60425.89 

rm(r, rep,ptm)

######################################################################
# Scenario 1: baBIC censored=0.25 train
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later

  simbaBICc25train_adl <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICadl, censor=0.25, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simbaBICc25train_iadl <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICiadl, censor=0.25, num.data.frames = 10) 
  simbaBICc25train_walk <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICwalk, censor=0.25, num.data.frames = 10) 
  simbaBICc25train_death <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICdeath, censor=0.25, num.data.frames = 10)
 
  #Process simulation object
  createSimData (simulation=simbaBICc25train_adl, outcome='adl', dataname=baBIC_adl) 
  createSimData (simulation=simbaBICc25train_iadl, outcome='iadl', dataname=baBIC_iadl)
  createSimData (simulation=simbaBICc25train_walk, outcome='walk', dataname=baBIC_walk)
  createSimData (simulation=simbaBICc25train_death, outcome='death', dataname=baBIC_death)
  
  #Stack 4 datasets
  total <- rbind(baBIC_adl,baBIC_iadl,baBIC_walk, baBIC_death)
  #table(total$outcome, total$status)
  #table(total$outcome)
  
  #Export total dataset
  write.csv(total, file = paste('sim10trainbaBICc25_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  rm(simbaBICc25train_adl,simbaBICc25train_iadl,simbaBICc25train_walk,simbaBICc25train_death,baBIC_adl,baBIC_iadl,baBIC_walk, baBIC_death, total)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#56749.69  3269.27 60115.88 
rm(r, rep,ptm)

#####################################
# Scenario 1: baBIC censored=0.25 test
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later
  
  simbaBICc25test_adl <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICadl, censor=0.25, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simbaBICc25test_iadl <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICiadl, censor=0.25, num.data.frames = 10) 
  simbaBICc25test_walk <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICwalk, censor=0.25, num.data.frames = 10) 
  simbaBICc25test_death <- sim.survdata(N=5531, T=5489, X=covarsbaBIC, beta=betas_baBICdeath, censor=0.25, num.data.frames = 10)

  #Process simulation object
  createSimData (simulation=simbaBICc25test_adl, outcome='adl', dataname=baBIC_adl) 
  createSimData (simulation=simbaBICc25test_iadl, outcome='iadl', dataname=baBIC_iadl)
  createSimData (simulation=simbaBICc25test_walk, outcome='walk', dataname=baBIC_walk)
  createSimData (simulation=simbaBICc25test_death, outcome='death', dataname=baBIC_death)

  #Stack 4 datasets
  total <- rbind(baBIC_adl,baBIC_iadl,baBIC_walk, baBIC_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10testbaBICc25_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#56920.09  4218.77 61296.17

#Remove objects that I don't need to free space/memory, keep only "covariates","createSimData"
rm(list=setdiff(ls(), c("covariates","createSimData")))


#####################################################################################################################################
############### Scenario 2: Individual outcome method   #############################################################################

#Select the covariates in the same order as they appear in the beta vector
covarsadl <- dplyr::select(covariates, age1, age2, age3, age4, age5, age6, age7, age8, age9, SEX, DRIVE, INCONTINENCE, EDU, DIABETES,
                           EXERCISE, OTHERARM, OTHERLIFT, OTHERSTOOP)

covarsiadl <- dplyr::select(covariates, age1, age2, age3, age4, age5, age6, age7, age8, age9, SEX, DRIVE, INCONTINENCE, EDU, OTHERSIT,
                            cogdl1, cogdl2, smoke1, smoke2, HEARAID)

covarswalk <- dplyr::select(covariates, age1, age2, age3, age4, age5, age6, age7, age8, age9, SEX, DRIVE, INCONTINENCE, OTHERSIT,
                            HYPERTENSION, climb1, climb2)

covarsdeath <- dplyr::select(covariates, age1, age2, age3, age4, age5, age6, age7, age8, age9, SEX, DRIVE, DIABETES, EXERCISE, cogdl1,
                             cogdl2, smoke1, smoke2, HYPERTENSION, climb1, climb2, HEARTFAILURE, lun1, lun2, MSTAT, OTHERPUSH,
                             walk1, walk2, bmi1, bmi2, bmi3, bmi4, VOLUNTEER)

#Create vectors with betas
betas_Ind<-read.csv('coefTableIndModel.csv')
betas_Indadl<-betas_Ind$Estimate[betas_Ind$outcome=="adl"]
betas_Indiadl<-betas_Ind$Estimate[betas_Ind$outcome=="iadl"]
betas_Indwalk<-betas_Ind$Estimate[betas_Ind$outcome=="walk"]
betas_Inddeath<-betas_Ind$Estimate[betas_Ind$outcome=="death"]
rm(betas_Ind)

######################################################################
# Scenario 2: Individual outcome original censored train
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later
  
  simIndtrain_adl <- sim.survdata(N=5531, T=5489, X=covarsadl, beta=betas_Indadl, censor=0.6655, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simIndtrain_iadl <- sim.survdata(N=5531, T=5489, X=covarsiadl, beta=betas_Indiadl, censor=0.6498, num.data.frames = 10) 
  simIndtrain_walk <- sim.survdata(N=5531, T=5489, X=covarswalk, beta=betas_Indwalk, censor=0.8190, num.data.frames = 10) 
  simIndtrain_death <- sim.survdata(N=5531, T=5489, X=covarsdeath, beta=betas_Inddeath, censor=0.3187, num.data.frames = 10)
  
  #Process simulation object
  createSimData (simulation=simIndtrain_adl, outcome='adl', dataname=Ind_adl) 
  createSimData (simulation=simIndtrain_iadl, outcome='iadl', dataname=Ind_iadl)
  createSimData (simulation=simIndtrain_walk, outcome='walk', dataname=Ind_walk)
  createSimData (simulation=simIndtrain_death, outcome='death', dataname=Ind_death)

  #Stack 4 datasets
  total <- rbind(Ind_adl,Ind_iadl,Ind_walk, Ind_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10trainIndcorig_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  rm(simIndtrain_adl,simIndtrain_iadl,simIndtrain_walk,simIndtrain_death, Ind_adl,Ind_iadl,Ind_walk, Ind_death, total)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#57129.58  3392.40 60596.53

rm(r, rep,ptm)


#####################################
# Scenario 2: Individual outcome original censored test
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later
  
  simIndtest_adl <- sim.survdata(N=5531, T=5489, X=covarsadl, beta=betas_Indadl, censor=0.6655, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simIndtest_iadl <- sim.survdata(N=5531, T=5489, X=covarsiadl, beta=betas_Indiadl, censor=0.6498, num.data.frames = 10) 
  simIndtest_walk <- sim.survdata(N=5531, T=5489, X=covarswalk, beta=betas_Indwalk, censor=0.8190, num.data.frames = 10) 
  simIndtest_death <- sim.survdata(N=5531, T=5489, X=covarsdeath, beta=betas_Inddeath, censor=0.3187, num.data.frames = 10)

  #Process simulation object
  createSimData (simulation=simIndtest_adl, outcome='adl', dataname=Ind_adl) 
  createSimData (simulation=simIndtest_iadl, outcome='iadl', dataname=Ind_iadl)
  createSimData (simulation=simIndtest_walk, outcome='walk', dataname=Ind_walk)
  createSimData (simulation=simIndtest_death, outcome='death', dataname=Ind_death)

  #Stack 4 datasets
  total <- rbind(Ind_adl,Ind_iadl,Ind_walk, Ind_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10testIndcorig_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  rm(simIndtest_adl,simIndtest_iadl,simIndtest_walk,simIndtest_death,Ind_adl,Ind_iadl,Ind_walk, Ind_death, total)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#57150.25  3769.62 61004.14

rm(r, rep,ptm)

######################################################################
# Scenario 2: Individual outcome censored=0.25 train
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later
  
  simIndc25train_adl <- sim.survdata(N=5531, T=5489, X=covarsadl, beta=betas_Indadl, censor=0.25, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simIndc25train_iadl <- sim.survdata(N=5531, T=5489, X=covarsiadl, beta=betas_Indiadl, censor=0.25, num.data.frames = 10) 
  simIndc25train_walk <- sim.survdata(N=5531, T=5489, X=covarswalk, beta=betas_Indwalk, censor=0.25, num.data.frames = 10) 
  simIndc25train_death <- sim.survdata(N=5531, T=5489, X=covarsdeath, beta=betas_Inddeath, censor=0.25, num.data.frames = 10)
  
  #Process simulation object
  createSimData (simulation=simIndc25train_adl, outcome='adl', dataname=Ind_adl) 
  createSimData (simulation=simIndc25train_iadl, outcome='iadl', dataname=Ind_iadl)
  createSimData (simulation=simIndc25train_walk, outcome='walk', dataname=Ind_walk)
  createSimData (simulation=simIndc25train_death, outcome='death', dataname=Ind_death)

  #Stack 4 datasets
  total <- rbind(Ind_adl,Ind_iadl,Ind_walk, Ind_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10trainIndc25_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  rm(simIndc25train_adl, simIndc25train_iadl, simIndc25train_walk,simIndc25train_death, Ind_adl,Ind_iadl,Ind_walk, Ind_death, total)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#57139.06  3799.16 61037.47 

rm(r, rep,ptm)

#####################################
# Scenario 2: Individual outcome censored=0.25 test
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later

  simIndc25test_adl <- sim.survdata(N=5531, T=5489, X=covarsadl, beta=betas_Indadl, censor=0.25, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simIndc25test_iadl <- sim.survdata(N=5531, T=5489, X=covarsiadl, beta=betas_Indiadl, censor=0.25, num.data.frames = 10) 
  simIndc25test_walk <- sim.survdata(N=5531, T=5489, X=covarswalk, beta=betas_Indwalk, censor=0.25, num.data.frames = 10) 
  simIndc25test_death <- sim.survdata(N=5531, T=5489, X=covarsdeath, beta=betas_Inddeath, censor=0.25, num.data.frames = 10)
  
  #Process simulation object
  createSimData (simulation=simIndc25test_adl, outcome='adl', dataname=Ind_adl) 
  createSimData (simulation=simIndc25test_iadl, outcome='iadl', dataname=Ind_iadl)
  createSimData (simulation=simIndc25test_walk, outcome='walk', dataname=Ind_walk)
  createSimData (simulation=simIndc25test_death, outcome='death', dataname=Ind_death)

  #Stack 4 datasets
  total <- rbind(Ind_adl,Ind_iadl,Ind_walk, Ind_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10testIndc25_20210109_', rep, '.csv', sep=""),row.names=FALSE)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#57088.34  4891.37 62137.88

#Remove objects that I don't need to free space/memory, keep only "covariates","createSimData"
rm(list=setdiff(ls(), c("ptm", "covariates","createSimData")))

#####################################################################################################################################
############### Scenario 3: Full method   ###########################################################################################

covarsFull <- dplyr::select(covariates, age1, age2, age3, age4, age5, age6, age7, age8, age9,
                            SEX, DRIVE, INCONTINENCE, EDU, DIABETES, EXERCISE, OTHERSIT, cogdl1, cogdl2, smoke1, smoke2,
                            HYPERTENSION, climb1, climb2, OTHERARM, OTHERLIFT, OTHERSTOOP, HEARAID, HEARTFAILURE, lun1, lun2, MSTAT,
                            OTHERPUSH, walk1, walk2, bmi1, bmi2, bmi3, bmi4, VOLUNTEER, alcoh1, alcoh2, ARTHRITIS, canc1, canc2, cogim1, cogim2, EYE2G,
                            fal1, fal2, hear1, hear2, hear3, hear4, LALONE, OTHERCHAIR, OTHERDIME, PAIN, CESDALL, father1, father2, father3, mother1, mother2, mother3,
                            health1, health2, health3, health4, strok1, strok2)

#Create vectors with betas
betas_Full<-read.csv('coefTableFullModel.csv')
betas_Fulladl<-betas_Full$Estimate[betas_Full$outcome=="adl"]
betas_Fulliadl<-betas_Full$Estimate[betas_Full$outcome=="iadl"]
betas_Fullwalk<-betas_Full$Estimate[betas_Full$outcome=="walk"]
betas_Fulldeath<-betas_Full$Estimate[betas_Full$outcome=="death"]
rm(betas_Full)


######################################################################
# Scenario 3: Full original censored train
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later

  simFulltrain_adl <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulladl, censor=0.6655, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simFulltrain_iadl <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulliadl, censor=0.6498, num.data.frames = 10) 
  simFulltrain_walk <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fullwalk, censor=0.8190, num.data.frames = 10) 
  simFulltrain_death <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulldeath, censor=0.3187, num.data.frames = 10)

  #Process simulation object
  createSimData (simulation=simFulltrain_adl, outcome='adl', dataname=Full_adl) 
  createSimData (simulation=simFulltrain_iadl, outcome='iadl', dataname=Full_iadl)
  createSimData (simulation=simFulltrain_walk, outcome='walk', dataname=Full_walk)
  createSimData (simulation=simFulltrain_death, outcome='death', dataname=Full_death)

  #Stack 4 datasets
  total <- rbind(Full_adl,Full_iadl,Full_walk, Full_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10trainFullcorig_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  rm(simFulltrain_adl,simFulltrain_iadl,simFulltrain_walk,simFulltrain_death,Full_adl,Full_iadl,Full_walk, Full_death, total)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#56773.91  3783.63 60643.59 

rm(r, rep,ptm)

#####################################
# Scenario 3: Full original censored test
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later
  
  simFulltest_adl <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulladl, censor=0.6655, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simFulltest_iadl <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulliadl, censor=0.6498, num.data.frames = 10) 
  simFulltest_walk <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fullwalk, censor=0.8190, num.data.frames = 10) 
  simFulltest_death <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulldeath, censor=0.3187, num.data.frames = 10)

  #Process simulation object
  createSimData (simulation=simFulltest_adl, outcome='adl', dataname=Full_adl) 
  createSimData (simulation=simFulltest_iadl, outcome='iadl', dataname=Full_iadl)
  createSimData (simulation=simFulltest_walk, outcome='walk', dataname=Full_walk)
  createSimData (simulation=simFulltest_death, outcome='death', dataname=Full_death)

  #Stack 4 datasets
  total <- rbind(Full_adl,Full_iadl,Full_walk, Full_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10testFullcorig_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  rm(simFulltest_adl,simFulltest_iadl,simFulltest_walk,simFulltest_death,Full_adl,Full_iadl,Full_walk, Full_death, total)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#56639.25  3706.95 60439.18

rm(r, rep,ptm)


######################################################################
# Scenario 3: Full censored=0.25 train
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later

  simFullc25train_adl <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulladl, censor=0.25, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simFullc25train_iadl <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulliadl, censor=0.25, num.data.frames = 10) 
  simFullc25train_walk <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fullwalk, censor=0.25, num.data.frames = 10) 
  simFullc25train_death <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulldeath, censor=0.25, num.data.frames = 10)

  #Process simulation object
  createSimData (simulation=simFullc25train_adl, outcome='adl', dataname=Full_adl) 
  createSimData (simulation=simFullc25train_iadl, outcome='iadl', dataname=Full_iadl)
  createSimData (simulation=simFullc25train_walk, outcome='walk', dataname=Full_walk)
  createSimData (simulation=simFullc25train_death, outcome='death', dataname=Full_death)

  #Stack 4 datasets
  total <- rbind(Full_adl,Full_iadl,Full_walk, Full_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10trainFullc25_20210109_', rep, '.csv', sep=""),row.names=FALSE)
  rm(simFullc25train_adl,simFullc25train_iadl,simFullc25train_walk,simFullc25train_death,Full_adl,Full_iadl,Full_walk, Full_death, total)
}
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#56599.66  3698.74 60375.14 

rm(r, rep,ptm)
#####################################
# Scenario 3: Full censored=0.25 test
# Start the clock!
ptm <- proc.time()
for (r in 1:50) {
  rep<-r #save repetition number. I will use it later

  simFullc25test_adl <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulladl, censor=0.25, num.data.frames = 10) #15.0278689yrs*365.25(average number of days)=5488.929 days
  simFullc25test_iadl <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulliadl, censor=0.25, num.data.frames = 10) 
  simFullc25test_walk <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fullwalk, censor=0.25, num.data.frames = 10) 
  simFullc25test_death <- sim.survdata(N=5531, T=5489, X=covarsFull, beta=betas_Fulldeath, censor=0.25, num.data.frames = 10)

  #Process simulation object
  createSimData (simulation=simFullc25test_adl, outcome='adl', dataname=Full_adl) 
  createSimData (simulation=simFullc25test_iadl, outcome='iadl', dataname=Full_iadl)
  createSimData (simulation=simFullc25test_walk, outcome='walk', dataname=Full_walk)
  createSimData (simulation=simFullc25test_death, outcome='death', dataname=Full_death)

  #Stack 4 datasets
  total <- rbind(Full_adl,Full_iadl,Full_walk, Full_death)
  #table(total$outcome, total$status)
  #table(total$outcome)

  #Export total dataset
  write.csv(total, file = paste('sim10testFullc25_20210109_', rep, '.csv', sep=""),row.names=FALSE)
} 
# Stop the clock
proc.time() - ptm
#  user  system elapsed
#56785.34  4523.23 61457.82

#Remove objects that I don't need to free space/memory, keep only "covariates","createSimData"
rm(list=setdiff(ls(), c("ptm", "covariates","createSimData")))



