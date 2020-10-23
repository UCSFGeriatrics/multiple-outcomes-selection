setwd("path")

#install.packages("glmnet")

library(glmnet) ##LASSO

#########################################################################################################
########################### LASSO in Original data ######################################################

mydata <- read.csv('originaldata.csv')

#Select the 39 predictors in full model
predictors <- dplyr::select(mydata, dAGE, SEX, ALCOHOL, ARTHRITIS, CANCER, COGDLRC3G, COGIMRC3G, DIABETES, DRIVE, EDU, EXERCISE, EYE2G, 
                            FALL, HEARAID, HEARING, HEARTFAILURE, HYPERTENSION, INCONTINENCE, LALONE, LUNG, MSTAT, OTHERARM, 
                            OTHERCHAIR, OTHERCLIM3G, OTHERDIME, OTHERLIFT, OTHERPUSH, OTHERSIT, OTHERSTOOP, OTHERWALK, PAIN, CESDALL,
                            qBMI, qFAGE, qMAGE, SHLT, SMOKING, STROKE, VOLUNTEER)
## convert variables to factors;
predictors[, 1:ncol(predictors)] <- lapply(predictors[,1:ncol(predictors)], factor)
#str(predictors)

## Given that the Gradient Boosting algorithm Or R couldn't use factor variables to fit, we will use model.matrix
## function to convert everything into numeric binary variables;

predictors2<-model.matrix(~.,predictors)[,-1]
#str(predictors2)
#colnames(predictors2) #70 coefficients

#Select the outcomes
outcome_death<- dplyr::select(mydata, time2death, death)
outcome_death <- dplyr::rename(outcome_death, time = time2death, status = death)
outcome_death <- as.matrix(outcome_death)

outcome_adl<- dplyr::select(mydata, time_adldepdth, status_adldepdth)
outcome_adl <- dplyr::rename(outcome_adl, time = time_adldepdth, status = status_adldepdth)
#table(outcome_adl$status)
#Change status 2 to censored and time to longest time
outcome_adl$time[outcome_adl$status==2] <- 15.0278689
#table(outcome_adl$time[outcome_adl$status==2])
outcome_adl$status[outcome_adl$status==2] <- 0
#table(outcome_adl$status)
outcome_adl <- as.matrix(outcome_adl)

outcome_iadl<- dplyr::select(mydata, time_iadldifdth, status_iadldifdth)
outcome_iadl <- dplyr::rename(outcome_iadl, time = time_iadldifdth, status = status_iadldifdth)
#table(outcome_iadl$status)
#Change status 2 to censored and time to longest time
outcome_iadl$time[outcome_iadl$status==2] <- 15.0278689
#table(outcome_iadl$time[outcome_iadl$status==2])
outcome_iadl$status[outcome_iadl$status==2] <- 0
#table(outcome_iadl$status)
outcome_iadl <- as.matrix(outcome_iadl)

outcome_walk<- dplyr::select(mydata, time_walkdepdth, status_walkdepdth)
outcome_walk <- dplyr::rename(outcome_walk, time = time_walkdepdth, status = status_walkdepdth)
#table(outcome_walk$status)
#Change status 2 to censored and time to longest time
outcome_walk$time[outcome_walk$status==2] <- 15.0278689
#table(outcome_walk$time[outcome_walk$status==2])
outcome_walk$status[outcome_walk$status==2] <- 0
#table(outcome_walk$status)
outcome_walk <- as.matrix(outcome_walk)


####### DEATH: Do LASSO selection using lambda at minimum BIC ####### 

#1-)Fit LASSO model
lasso_surv <- glmnet(predictors2, outcome_death, family = "cox")

#2-)Calculate BIC=-2logL+k*log(no. uncensored observations)

#no. uncensored observations
#table(mydata$death) #3768
uncens<-table(mydata$death)[[2]]

#df: The number of nonzero coefficients for each value of lambda (=k)

#deviance(lasso_surv)

#lasso_surv$dev.ratio
#dev.ratio	
#The fraction of (null) deviance explained (for "elnet", this is the R-square).
#The deviance calculations incorporate weights if present in the model.
#The deviance is defined to be 2*(loglike_sat - loglike), where loglike_sat is the log-likelihood for the saturated model 
#(a model with a free parameter per observation). Hence dev.ratio=1-dev/nulldev.

#lasso_surv$nulldev #60660.03
#nulldev	
#Null deviance (per observation). This is defined to be 2*(loglike_sat -loglike(Null));
#The NULL model refers to the intercept model, except for the Cox, where it is the 0 model.

#Formula to compute BIC
BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens)
#plot(BIC)

#3-) Find minimum BIC among them
min_BIC <- which.min(BIC) #34

#4-) Use lambda at the minimum BIC
#lasso_surv$lambda
#lasso_surv$lambda[min_BIC]
coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda
#coef_minBIC

#To get name of non zero coefficients for lambda[min_BIC]
coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
#coefNames_minBIC #54 coefficients
#length(coefNames_minBIC)
#Remove number part from predictor
coefNamesOnly_minBIC_death=gsub('[[:digit:]]+', '', coefNames_minBIC)
coefNamesOnly_minBIC_death=unique(coefNamesOnly_minBIC_death) #32 predictors/39
#coefNamesOnly_minBIC_death
#length(coefNamesOnly_minBIC_death)
coefNamesOnly_minBIC_death<-paste0(coefNamesOnly_minBIC_death, collapse = " ") #To collapse the output into a single string

rm(list=setdiff(ls(), c("outcome_adl", "outcome_death", "outcome_iadl", "outcome_walk", "predictors2", "mydata","coefNamesOnly_minBIC_death")))

#Create data with predictors selected using LASSO in original data: replicate=0, VARINMODEL_death
Replicate<-0
lasso_original <- data.frame(Replicate, VARINMODEL_death=coefNamesOnly_minBIC_death, stringsAsFactors=FALSE)


####### ADL: Do LASSO selection using lambda at minimum BIC ####### 

#1-)Fit LASSO model
lasso_surv <- glmnet(predictors2, outcome_adl, family = "cox")

#2-)Calculate BIC=-2logL+k*log(no. uncensored observations)

#no. uncensored observations
uncens<-table(mydata$status_adldepdth)[[2]]

BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
#plot(BIC)

#3-) Find minimum BIC among them
min_BIC <- which.min(BIC) #15

#4-) Use lambda at the minimum BIC
coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda

#To get name of non zero coefficients for lambda[min_BIC]
coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
#coefNames_minBIC #17 coefficients
#length(coefNames_minBIC)
#Remove number part from predictor
coefNamesOnly_minBIC_adl=gsub('[[:digit:]]+', '', coefNames_minBIC)
coefNamesOnly_minBIC_adl=unique(coefNamesOnly_minBIC_adl) #16 predictors/39
#coefNamesOnly_minBIC_adl
#length(coefNamesOnly_minBIC_adl)

coefNamesOnly_minBIC_adl<-paste0(coefNamesOnly_minBIC_adl, collapse = " ") #To collapse the output into a single string

rm(list=setdiff(ls(), c("lasso_original", "outcome_death", "outcome_adl", "outcome_iadl", "outcome_walk", "predictors2", "mydata", "coefNamesOnly_minBIC_death", "coefNamesOnly_minBIC_adl")))

lasso_original<-cbind(lasso_original, VARINMODEL_adl=coefNamesOnly_minBIC_adl)


####### IADL: Do LASSO selection using lambda at minimum BIC ####### 

#1-)Fit LASSO model
lasso_surv <- glmnet(predictors2, outcome_iadl, family = "cox")

#2-)Calculate BIC=-2logL+k*log(no. uncensored observations)

#no. uncensored observations
#table(mydata$status_iadldifdth) #1937
uncens<-table(mydata$status_iadldifdth)[[2]]

#df: The number of nonzero coefficients for each value of lambda (=k)

BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
#plot(BIC)

#3-) Find minimum BIC among them
min_BIC <- which.min(BIC) #17

#4-) Use lambda at the minimum BIC
coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda

#To get name of non zero coefficients for lambda[min_BIC]
coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
#coefNames_minBIC #20 coefficients
#length(coefNames_minBIC)
#Remove number part from predictor
coefNamesOnly_minBIC_iadl=gsub('[[:digit:]]+', '', coefNames_minBIC)
coefNamesOnly_minBIC_iadl=unique(coefNamesOnly_minBIC_iadl) #18 predictors/39
#coefNamesOnly_minBIC_iadl
#length(coefNamesOnly_minBIC_iadl)

coefNamesOnly_minBIC_iadl<-paste0(coefNamesOnly_minBIC_iadl, collapse = " ") #To collapse the output into a single string

rm(list=setdiff(ls(), c("lasso_original","outcome_death", "outcome_adl", "outcome_iadl", "outcome_walk", "predictors2", "mydata", "coefNamesOnly_minBIC_death", "coefNamesOnly_minBIC_adl", "coefNamesOnly_minBIC_iadl")))

lasso_original<-cbind(lasso_original, VARINMODEL_iadl=coefNamesOnly_minBIC_iadl)


####### WALK: Do LASSO selection using lambda at minimum BIC ####### 

#1-)Fit LASSO model
lasso_surv <- glmnet(predictors2, outcome_walk, family = "cox")

#2-)Calculate BIC=-2logL+k*log(no. uncensored observations)

#no. uncensored observations
uncens<-table(mydata$status_walkdepdth)[[2]]

#df: The number of nonzero coefficients for each value of lambda (=k)

BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
#plot(BIC)

#3-) Find minimum BIC among them
min_BIC <- which.min(BIC) #12

#4-) Use lambda at the minimum BIC
coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda

#To get name of non zero coefficients for lambda[min_BIC]
coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
#coefNames_minBIC #13 coefficients
#length(coefNames_minBIC)
#Remove number part from predictor
coefNamesOnly_minBIC_walk=gsub('[[:digit:]]+', '', coefNames_minBIC)
coefNamesOnly_minBIC_walk=unique(coefNamesOnly_minBIC_walk) #13 predictors/39
#coefNamesOnly_minBIC_walk
#length(coefNamesOnly_minBIC_walk)

coefNamesOnly_minBIC_walk<-paste0(coefNamesOnly_minBIC_walk, collapse = " ") #To collapse the output into a single string

rm(list=setdiff(ls(), c("lasso_original", "outcome_death", "outcome_adl", "outcome_iadl", "outcome_walk", "predictors2", "mydata", "coefNamesOnly_minBIC_death", "coefNamesOnly_minBIC_adl", "coefNamesOnly_minBIC_iadl", "coefNamesOnly_minBIC_walk")))

lasso_original<-cbind(lasso_original, VARINMODEL_walk=coefNamesOnly_minBIC_walk)

#Save lasso_original as csv file
write.csv(lasso_original, file = "lasso_original.csv",row.names=FALSE)

rm(list = ls())

##########################################################################################################
########################### LASSO in Bootstrap data ######################################################

## Get predictors and outcomes from original data

setwd("path")
mydata <- read.csv('originaldata.csv')

## Get newid selected in each bootstrap data
library(sas7bdat)
setwd("path")
bs500 <- read.sas7bdat("bs500.sas7bdat")
setwd("path")
save(bs500,file="bs500.Rdata") # 2,765,500 observations and 2 variables

load("bs500.Rdata")

## Merge bs500.Rdata with original data  and only keep the newids selected in the bs500
bsdata <- merge(bs500,mydata,by="newid", all.x = TRUE, all.y = FALSE) #whichever dataset is first is considered x and the second one is y 

#order by Replicate
bsdata <- bsdata[order(bsdata$Replicate),]
rm("bs500")
rm("mydata")

#Create empty lasso_bs data
lasso_bs <- data.frame(Replicate=integer(), VARINMODEL_death=character(), VARINMODEL_adl=character(), VARINMODEL_iadl=character(),
                       VARINMODEL_walk=character(), stringsAsFactors=FALSE)

# For each bootstrap data: Time~1h44min
for (i in 1:500) {
  
  b<-i #save bootstrap sample number
  
  #Select the 39 predictors in full model
  predictors <- subset(bsdata, Replicate==b, 
                       select=c(dAGE, SEX, ALCOHOL, ARTHRITIS, CANCER, COGDLRC3G, COGIMRC3G, DIABETES, DRIVE, EDU, EXERCISE, EYE2G, 
                                FALL, HEARAID, HEARING, HEARTFAILURE, HYPERTENSION, INCONTINENCE, LALONE, LUNG, MSTAT, OTHERARM, 
                                OTHERCHAIR, OTHERCLIM3G, OTHERDIME, OTHERLIFT, OTHERPUSH, OTHERSIT, OTHERSTOOP, OTHERWALK, PAIN, CESDALL,
                                qBMI, qFAGE, qMAGE, SHLT, SMOKING, STROKE, VOLUNTEER))
  
  ## convert variables to factors;
  predictors[, 1:ncol(predictors)] <- lapply(predictors[,1:ncol(predictors)], factor)
  #str(predictors)
  
  ## Given that the Gradient Boosting algorithm Or R couldn't use factor variables to fit, we will use model.matrix
  ## function to convert everything into numeric binary variables
  
  predictors2<-model.matrix(~.,predictors)[,-1]
  #str(predictors2)
  #colnames(predictors2) #70 coefficients
  
  rm("predictors")
  
  outcome_death <- subset(bsdata, Replicate==b, select=c(time2death, death))
  outcome_death <- dplyr::rename(outcome_death, time = time2death, status = death)
  outcome_death <- as.matrix(outcome_death)
  
  outcome_adl<- subset(bsdata, Replicate==b, select=c( time_adldepdth, status_adldepdth))
  outcome_adl <- dplyr::rename(outcome_adl, time = time_adldepdth, status = status_adldepdth)
  outcome_adl$time[outcome_adl$status==2] <- 15.0278689
  outcome_adl$status[outcome_adl$status==2] <- 0
  outcome_adl <- as.matrix(outcome_adl)
  
  outcome_iadl<- subset(bsdata, Replicate==b, select=c(time_iadldifdth, status_iadldifdth))
  outcome_iadl <- dplyr::rename(outcome_iadl, time = time_iadldifdth, status = status_iadldifdth)
  outcome_iadl$time[outcome_iadl$status==2] <- 15.0278689
  outcome_iadl$status[outcome_iadl$status==2] <- 0
  outcome_iadl <- as.matrix(outcome_iadl)
  
  outcome_walk<- subset(bsdata, Replicate==b, select=c(time_walkdepdth, status_walkdepdth))
  outcome_walk <- dplyr::rename(outcome_walk, time = time_walkdepdth, status = status_walkdepdth)
  outcome_walk$time[outcome_walk$status==2] <- 15.0278689
  outcome_walk$status[outcome_walk$status==2] <- 0
  outcome_walk <- as.matrix(outcome_walk)
  
  
  ####### DEATH: Do LASSO selection using lambda at minimum BIC ####### 
  
  #1-)Fit LASSO model
  lasso_surv <- glmnet(predictors2, outcome_death, family = "cox")
  
  #2-)Calculate BIC=-2logL+k*log(no. uncensored observations)
  
  #no. uncensored observations
  uncens<-with(bsdata, table(death, Replicate))[[2,b]] #row=2 (status=1), column=replicate number
  
  #Formula to compute BIC
  BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens)
  
  #3-) Find minimum BIC among them
  min_BIC <- which.min(BIC)
  
  #4-) Use lambda at the minimum BIC
  coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda
  
  #To get name of non zero coefficients for lambda[min_BIC]
  coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
  #Remove number part from predictor
  coefNamesOnly_minBIC_death=gsub('[[:digit:]]+', '', coefNames_minBIC)
  coefNamesOnly_minBIC_death=unique(coefNamesOnly_minBIC_death)
  coefNamesOnly_minBIC_death<-paste0(coefNamesOnly_minBIC_death, collapse = " ") #To collapse the output into a single string
  
  #Create data with predictors selected using LASSO in original data: replicate=0, VARINMODEL_death
  Replicate<-b
  lasso_temp <- data.frame(Replicate, VARINMODEL_death=coefNamesOnly_minBIC_death, stringsAsFactors=FALSE)
  
  rm(list=setdiff(ls(), c("outcome_adl", "outcome_iadl", "outcome_walk", "predictors2", "bsdata" ,"lasso_temp", "lasso_bs", "b")))
  
  
  ####### ADL: Do LASSO selection using lambda at minimum BIC ####### 
  
  #1-)Fit LASSO model
  lasso_surv <- glmnet(predictors2, outcome_adl, family = "cox")
  
  #2-)Calculate BIC=-2logL+k*log(no. uncensored observations)
  
  #no. uncensored observations
  uncens<-with(bsdata, table(status_adldepdth, Replicate))[[2,b]] #row=2 (status=1), column=replicate number
  
  BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
  
  #3-) Find minimum BIC among them
  min_BIC <- which.min(BIC) 
  
  #4-) Use lambda at the minimum BIC
  coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda
  
  #To get name of non zero coefficients for lambda[min_BIC]
  coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
  #Remove number part from predictor
  coefNamesOnly_minBIC_adl=gsub('[[:digit:]]+', '', coefNames_minBIC)
  coefNamesOnly_minBIC_adl=unique(coefNamesOnly_minBIC_adl) #16 predictors/39
  coefNamesOnly_minBIC_adl<-paste0(coefNamesOnly_minBIC_adl, collapse = " ") #To collapse the output into a single string
  
  lasso_temp<-cbind(lasso_temp, VARINMODEL_adl=coefNamesOnly_minBIC_adl)
  
  rm(list=setdiff(ls(), c("outcome_iadl", "outcome_walk", "predictors2", "bsdata" ,"lasso_temp", "lasso_bs", "b")))
  
  
  ####### IADL: Do LASSO selection using lambda at minimum BIC ####### 
  
  #1-)Fit LASSO model
  lasso_surv <- glmnet(predictors2, outcome_iadl, family = "cox")
  
  #2-)Calculate BIC=-2logL+k*log(no. uncensored observations)
  
  #no. uncensored observations
  uncens<-with(bsdata, table(status_iadldifdth, Replicate))[[2,b]] #row=2 (status=1), column=replicate number
  
  BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
  
  #3-) Find minimum BIC among them
  min_BIC <- which.min(BIC)
  
  #4-) Use lambda at the minimum BIC
  coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda
  
  #To get name of non zero coefficients for lambda[min_BIC]
  coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
  #Remove number part from predictor
  coefNamesOnly_minBIC_iadl=gsub('[[:digit:]]+', '', coefNames_minBIC)
  coefNamesOnly_minBIC_iadl=unique(coefNamesOnly_minBIC_iadl) #18 predictors/39
  coefNamesOnly_minBIC_iadl<-paste0(coefNamesOnly_minBIC_iadl, collapse = " ") #To collapse the output into a single string
  
  lasso_temp<-cbind(lasso_temp, VARINMODEL_iadl=coefNamesOnly_minBIC_iadl)
  
  rm(list=setdiff(ls(), c("outcome_walk", "predictors2", "bsdata" ,"lasso_temp", "lasso_bs", "b")))
  
  ####### WALK: Do LASSO selection using lambda at minimum BIC ####### 
  
  #1-)Fit LASSO model
  lasso_surv <- glmnet(predictors2, outcome_walk, family = "cox")
  
  #2-)Calculate BIC=-2logL+k*log(no. uncensored observations)
  
  #no. uncensored observations
  uncens<-with(bsdata, table(status_walkdepdth, Replicate))[[2,b]] #row=2 (status=1), column=replicate number
  
  BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
  
  #3-) Find minimum BIC among them
  min_BIC <- which.min(BIC) 
  
  #4-) Use lambda at the minimum BIC
  coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda
  
  #To get name of non zero coefficients for lambda[min_BIC]
  coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
  coefNamesOnly_minBIC_walk=gsub('[[:digit:]]+', '', coefNames_minBIC)
  coefNamesOnly_minBIC_walk=unique(coefNamesOnly_minBIC_walk) #13 predictors/39
  coefNamesOnly_minBIC_walk<-paste0(coefNamesOnly_minBIC_walk, collapse = " ") #To collapse the output into a single string
  
  lasso_temp<-cbind(lasso_temp, VARINMODEL_walk=coefNamesOnly_minBIC_walk)
  
  rm(list=setdiff(ls(), c("bsdata" ,"lasso_temp", "lasso_bs")))
  
  lasso_bs<-rbind(lasso_bs, lasso_temp)
  rm("lasso_temp")

}

#Save lasso_temp as csv file
write.csv(lasso_bs, file = "lasso_bs.csv",row.names=FALSE)
