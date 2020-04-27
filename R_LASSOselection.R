setwd("V:/Health and Retirement Study/Grisell/AlexSei/R01eprognosisOutcomes2/Rfiles")

#install.packages("glmnet")

library(glmnet) ##LASSO
mydata <- read.csv('originaldata.csv')

#Select the 39 predictors in full model
predictors <- dplyr::select(mydata, dAGE, SEX, ALCOHOL, ARTHRITIS, CANCER, COGDLRC3G, COGIMRC3G, DIABETES, DRIVE, EDU, EXERCISE, EYE2G, 
                            FALL, HEARAID, HEARING, HEARTFAILURE, HYPERTENSION, INCONTINENCE, LALONE, LUNG, MSTAT, OTHERARM, 
                            OTHERCHAIR, OTHERCLIM3G, OTHERDIME, OTHERLIFT, OTHERPUSH, OTHERSIT, OTHERSTOOP, OTHERWALK, PAIN, CESDALL,
                            qBMI, qFAGE, qMAGE, SHLT, SMOKING, STROKE, VOLUNTEER)
str(predictors)

## convert variables to factors;
predictors[, 1:ncol(predictors)] <- lapply(predictors[,1:ncol(predictors)], factor)
str(predictors)

## Given that the Gradient Boosting algorithm Or R couldn't use factor variables to fit, we will use model.matrix
## function to convert everything into numeric binary variables;

predictors2<-model.matrix(~.,predictors)[,-1]
str(predictors2)
colnames(predictors2) #70 coefficients

#Select the outcomes
outcome_death<- dplyr::select(mydata, time2death, death)
outcome_death <- dplyr::rename(outcome_death, time = time2death, status = death)
outcome_death <- as.matrix(outcome_death)

outcome_adl<- dplyr::select(mydata, time_adldepdth, status_adldepdth)
outcome_adl <- dplyr::rename(outcome_adl, time = time_adldepdth, status = status_adldepdth)
table(outcome_adl$status)
#Change status 2 to censored and time to longest time
outcome_adl$time[outcome_adl$status==2] <- 15.0278689
table(outcome_adl$time[outcome_adl$status==2])
outcome_adl$status[outcome_adl$status==2] <- 0
table(outcome_adl$status)
outcome_adl <- as.matrix(outcome_adl)

outcome_iadl<- dplyr::select(mydata, time_iadldifdth, status_iadldifdth)
outcome_iadl <- dplyr::rename(outcome_iadl, time = time_iadldifdth, status = status_iadldifdth)
table(outcome_iadl$status)
#Change status 2 to censored and time to longest time
outcome_iadl$time[outcome_iadl$status==2] <- 15.0278689
table(outcome_iadl$time[outcome_iadl$status==2])
outcome_iadl$status[outcome_iadl$status==2] <- 0
table(outcome_iadl$status)
outcome_iadl <- as.matrix(outcome_iadl)

outcome_walk<- dplyr::select(mydata, time_walkdepdth, status_walkdepdth)
outcome_walk <- dplyr::rename(outcome_walk, time = time_walkdepdth, status = status_walkdepdth)
table(outcome_walk$status)
#Change status 2 to censored and time to longest time
outcome_walk$time[outcome_walk$status==2] <- 15.0278689
table(outcome_walk$time[outcome_walk$status==2])
outcome_walk$status[outcome_walk$status==2] <- 0
table(outcome_walk$status)
outcome_walk <- as.matrix(outcome_walk)


################################################################################################
####### DEATH: Do LASSO selection using lambda at minimum BIC ####### 

#1-)Fit LASSO model
lasso_surv <- glmnet(predictors2, outcome_death, family = "cox")

#2-)Calculate BIC=-2logL+k*log(no. uncensored observations)

#no. uncensored observations
table(mydata$death) #3768
uncens<-table(mydata$death)[[2]]

BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens)
plot(BIC)

## Note: the term lasso_surv$nulldev*(1-lasso_surv$dev.ratio) does not give only the -2loglike 
##      but the Deviance: 2*(loglike_sat - loglike), where loglike_sat is the log-likelihood for the saturated model.
##      So, this term includes an additional constant 2*loglike_sat.
##      This does not affect our selection since we are not interested in comparing the absolute value of the BIC

#3-) Find minimum BIC among them
min_BIC <- which.min(BIC) #34
min_BIC

#4-) Use lambda at the minimum BIC
lasso_surv$lambda
lasso_surv$lambda[min_BIC]
coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda
coef_minBIC

#To get name of non zero coefficients for lambda[min_BIC]
coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
coefNames_minBIC #54 coefficients
length(coefNames_minBIC)
#Remove number part from predictor
coefNamesOnly_minBIC_death=gsub('[[:digit:]]+', '', coefNames_minBIC)
coefNamesOnly_minBIC_death=unique(coefNamesOnly_minBIC_death) #32 predictors/39
coefNamesOnly_minBIC_death
length(coefNamesOnly_minBIC_death)

rm(list=setdiff(ls(), c("outcome_adl", "outcome_death", "outcome_iadl", "outcome_walk", "predictors2", "mydata","coefNamesOnly_minBIC_death")))

################################################################################################
####### ADL: Do LASSO selection using lambda at minimum BIC ####### 

#1-)Fit LASSO model
lasso_surv <- glmnet(predictors2, outcome_adl, family = "cox")

#2-)Calculate BIC=-2logL+k*log(no. uncensored observations)

#no. uncensored observations
table(mydata$status_adldepdth) #1850
uncens<-table(mydata$status_adldepdth)[[2]]

#df: The number of nonzero coefficients for each value of lambda (=k)

BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
plot(BIC)

#3-) Find minimum BIC among them
min_BIC <- which.min(BIC) #15

#4-) Use lambda at the minimum BIC
coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda

#To get name of non zero coefficients for lambda[min_BIC]
coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
coefNames_minBIC #17 coefficients
length(coefNames_minBIC)
#Remove number part from predictor
coefNamesOnly_minBIC_adl=gsub('[[:digit:]]+', '', coefNames_minBIC)
coefNamesOnly_minBIC_adl=unique(coefNamesOnly_minBIC_adl) #16 predictors/39
coefNamesOnly_minBIC_adl
length(coefNamesOnly_minBIC_adl)

rm(list=setdiff(ls(), c("outcome_death", "outcome_adl", "outcome_iadl", "outcome_walk", "predictors2", "mydata", "coefNamesOnly_minBIC_death", "coefNamesOnly_minBIC_adl")))

################################################################################################
####### IADL: Do LASSO selection using lambda at minimum BIC ####### 

#1-)Fit LASSO model
lasso_surv <- glmnet(predictors2, outcome_iadl, family = "cox")

#2-)Calculate BIC=-2logL+k*log(no. uncensored observations)

#no. uncensored observations
table(mydata$status_iadldifdth) #1937
uncens<-table(mydata$status_iadldifdth)[[2]]

#df: The number of nonzero coefficients for each value of lambda (=k)

BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
plot(BIC)

#3-) Find minimum BIC among them
min_BIC <- which.min(BIC) #17

#4-) Use lambda at the minimum BIC
coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda

#To get name of non zero coefficients for lambda[min_BIC]
coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
coefNames_minBIC #20 coefficients
length(coefNames_minBIC)
#Remove number part from predictor
coefNamesOnly_minBIC_iadl=gsub('[[:digit:]]+', '', coefNames_minBIC)
coefNamesOnly_minBIC_iadl=unique(coefNamesOnly_minBIC_iadl) #18 predictors/39
coefNamesOnly_minBIC_iadl
length(coefNamesOnly_minBIC_iadl)

rm(list=setdiff(ls(), c("outcome_death", "outcome_adl", "outcome_iadl", "outcome_walk", "predictors2", "mydata", "coefNamesOnly_minBIC_death", "coefNamesOnly_minBIC_adl", "coefNamesOnly_minBIC_iadl")))

################################################################################################
####### WALK: Do LASSO selection using lambda at minimum BIC ####### 

#1-)Fit LASSO model
lasso_surv <- glmnet(predictors2, outcome_walk, family = "cox")

#2-)Calculate BIC=-2logL+k*log(no. uncensored observations)

#no. uncensored observations
table(mydata$status_walkdepdth) #1001
uncens<-table(mydata$status_walkdepdth)[[2]]

#df: The number of nonzero coefficients for each value of lambda (=k)

BIC <- lasso_surv$nulldev*(1-lasso_surv$dev.ratio) + lasso_surv$df*log(uncens) 
plot(BIC)

#3-) Find minimum BIC among them
min_BIC <- which.min(BIC) #12

#4-) Use lambda at the minimum BIC
coef_minBIC<-coef(lasso_surv, s=lasso_surv$lambda[min_BIC])  # extract coefficients at a single value of lambda

#To get name of non zero coefficients for lambda[min_BIC]
coefNames_minBIC=coef_minBIC@Dimnames[[1]][which(coef_minBIC!=0)]
coefNames_minBIC #13 coefficients
length(coefNames_minBIC)
#Remove number part from predictor
coefNamesOnly_minBIC_walk=gsub('[[:digit:]]+', '', coefNames_minBIC)
coefNamesOnly_minBIC_walk=unique(coefNamesOnly_minBIC_walk) #13 predictors/39
coefNamesOnly_minBIC_walk
length(coefNamesOnly_minBIC_walk)

rm(list=setdiff(ls(), c("outcome_death", "outcome_adl", "outcome_iadl", "outcome_walk", "predictors2", "mydata", "coefNamesOnly_minBIC_death", "coefNamesOnly_minBIC_adl", "coefNamesOnly_minBIC_iadl", "coefNamesOnly_minBIC_walk")))

################################################################################################
########## Union model: Do LASSO selection using minimum BIC #############

#install.packages("reshape")
library(reshape)

split_death<-as.data.frame(strsplit(coefNamesOnly_minBIC_death, " "))
split_death$outcome<-"death"
split_death2<-melt(split_death, id="outcome" )
split_death2$variable<-NULL

split_adl<-as.data.frame(strsplit(coefNamesOnly_minBIC_adl, " "))
split_adl$outcome<-"adl"
split_adl2<-melt(split_adl, id="outcome" )
split_adl2$variable<-NULL

split_iadl<-as.data.frame(strsplit(coefNamesOnly_minBIC_iadl, " "))
split_iadl$outcome<-"iadl"
split_iadl2<-melt(split_iadl, id="outcome" )
split_iadl2$variable<-NULL

split_walk<-as.data.frame(strsplit(coefNamesOnly_minBIC_walk, " "))
split_walk$outcome<-"walk"
split_walk2<-melt(split_walk, id="outcome" )
split_walk2$variable<-NULL

union_lasso_minBIC<-rbind(split_death2, split_adl2, split_iadl2, split_walk2)
write.csv(union_lasso_minBIC, file = "union_lasso_minBIC.csv",row.names=FALSE)

union_lasso_minBIC2<-as.data.frame(table(union_lasso_minBIC$value))
write.csv(union_lasso_minBIC2, file = "union_lasso_minBIC2.csv",row.names=FALSE)

rm(list=setdiff(ls(), c("outcome_death", "outcome_adl", "outcome_iadl", "outcome_walk", "predictors2", "mydata", "coefNamesOnly_minBIC_death", "coefNamesOnly_minBIC_adl", "coefNamesOnly_minBIC_iadl", "coefNamesOnly_minBIC_walk","union_lasso_minBIC")))

