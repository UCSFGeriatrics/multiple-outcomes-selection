# Title: A Novel Method for Identifying a Parsimonious and Accurate Predictive Model for Multiple Clinical Outcomes
## L. Grisell Diaz-Ramirez MS,a,b Sei J. Lee MD,a,b Alexander K. Smith MD,a,b Siqi Gan MS,a,b W. John Boscardin PhDa,b
### a Division of Geriatrics, University of California, San Francisco
### 490 Illinois Street, Floor 08, Box 1265, San Francisco, CA 94143, United States
### b San Francisco Veterans Affairs (VA) Medical Center
### 4150 Clement Street, 181G, San Francisco, CA 94121, United States

Description of data and SAS and R codes for reproducing the results of this article:
File name: originaldata
File format: .csv
Description: HRS data with 39 predictors and 4 outcomes of 5,531 respondents
File name: R_LASSOselection
File format: .R
Description: R code to perform LASSO Selection based on Optimal λ at Minimum BIC for Individual Outcome and Union methods
File name: SAS_ LassoIndUnionNumPredCstat_BS500
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for LASSO Individual and Union methods using 500 bootstrap samples
File name: R_Fig4.nBIC_BICvsNumPred
File format: .R
Description: R code for Fig. 4: Selection with the baBIC method and Individual Outcome Methods in the Case-study Data

File name: R_Fig7.NumPred
File format: .R
Description: R code for Fig. 7: Comparison of Mean Number of Predictors Across Case-study Bootstrap Data and Simulations with Case-study Censoring and 25% Censoring
File name: R_Fig8.Cstat
File format: .R
Description: R code for Fig. 8: Comparison of Mean Harrell’s C-statistic Across Case-study Bootstrap Data and Simulations with Case-study Censoring and 25% Censoring
File name: 1.SAS_BICbackwardIndOutcomeCR
File format: .sas
Description: SAS code to perform BIC backward elimination by Outcome using HRS original data set. It uses Cox regression for Death, and Competing-risk regression for rest of outcomes.
File name: 2.SAS_BICbackwardIndOutcomeCox
File format: .sas
Description: SAS code to perform BIC backward elimination by Outcome using HRS original data set. It uses Cox regression for 4 outcomes and Wolbers et. al (2009) adaptation to the Competing-risk settings.
File name: 3.SAS_baBICbackwardCR
File format: .sas
Description: SAS code to perform best average BIC (baBIC) backward elimination using HRS original data set. It uses Cox regression for Death, and Competing-risk regression for rest of outcomes. baBIC=absolute(BICk-BICbest)/absolute(BICfull-BICbest): BICfull and BICbest are the BICs of the full and best individual models.
File name: 4.SAS_baBICbackwardCox
File format: .sas
Description: SAS code to perform best average BIC (baBIC) backward elimination using HRS original data set. It uses Cox regression for 4 outcomes and Wolbers et. al (2009) adaptation to the Competing-risk settings. baBIC=absolute(BICk-BICbest)/absolute(BICfull-BICbest): BICfull and BICbest are the BICs of the full and best individual models.
File name: 5a.SAS_BetaCoefficientsSce1_2_3
File format: .sas
Description: Compute Beta coefficients used in Scenarios 1, 2, and 3 of the simulation study
File name: 5b.R_CleanBetaCoefficientsSce1_2_3
File format: .R
Description: Produce cleaner format of Beta coefficients used in Scenarios 1, 2, and 3 of the simulation study
File name: 6.GenerateSimulationsSce1_2_3
File format: .R
Description: R code to generate simulated data sets (training/test) for Scenarios 1, 2, and 3 with case-study censoring and 25% censoring
File name: 7a.SAS_SimSce1corigTrain
File format: .sas
Description: SAS code to import training simulated data sets under Scenario 1 (baBIC method) with case-study censoring from R and obtain some general statistics
File name: 7b.SAS_SimSce1corigTest
File format: .sas
Description: SAS code to import test simulated data sets under Scenario 1 (baBIC method) with case-study censoring from R and obtain some general statistics
File name: 8a.SAS_SimSce1c25Train
File format: .sas
Description: SAS code to import training simulated data sets under Scenario 1 (baBIC method) with 25% censoring from R and obtain some general statistics
File name: 8b.SAS_SimSce1c25Test
File format: .sas
Description: SAS code to import test simulated data sets under Scenario 1 (baBIC method) with 25% censoring from R and obtain some general statistics
File name: 9a.SAS_SimSce2corigTrain
File format: .sas
Description: SAS code to import training simulated data sets under Scenario 2 (Individual Outcome method) with case-study censoring from R and obtain some general statistics
File name: 9b.SAS_SimSce2corigTest
File format: .sas
Description: SAS code to import test simulated data sets under Scenario 2 (Individual Outcome method) with case-study censoring from R and obtain some general statistics
File name: 10a.SAS_SimSce2c25Train
File format: .sas
Description: SAS code to import training simulated data sets under Scenario 2 (Individual Outcome method) with 25% censoring from R and obtain some general statistics
File name: 10b.SAS_SimSce2c25Test
File format: .sas
Description: SAS code to import test simulated data sets under Scenario 2 (Individual Outcome method) with 25% censoring from R and obtain some general statistics
File name: 11a.SAS_SimSce3corigTrain
File format: .sas
Description: SAS code to import training simulated data sets under Scenario 3 (Full method) with case-study censoring from R and obtain some general statistics
File name: 11b.SAS_SimSce3corigTest
File format: .sas
Description: SAS code to import test simulated data sets under Scenario 3 (Full method) with case-study censoring from R and obtain some general statistics
File name: 12a.SAS_SimSce3c25Train
File format: .sas
Description: SAS code to import training simulated data sets under Scenario 3 (Full method) with 25% censoring from R and obtain some general statistics
File name: 12b.SAS_SimSce3c25Test
File format: .sas
Description: SAS code to import test simulated data sets under Scenario 3 (Full method) with 25% censoring from R and obtain some general statistics
File name: 13.SAS_BICbackwardIndOutcomeSimSce1corig
File format: .sas
Description: SAS code to perform BIC backward elimination for individual outcomes for simulations of Scenario 1 with case-study censoring and training data.
File name: 14.SAS_BICbackwardIndOutcomeSimSce1c25
File format: .sas
Description: SAS code to perform BIC backward elimination for individual outcomes for simulations of Scenario 1 with 25% censoring and training data.
File name: 15.SAS_baBICbackwardSimSce1corig
File format: .sas
Description: SAS code to perform baBIC backward elimination for simulations of Scenario 1 with case-study censoring and training data
File name: 16.SAS_baBICbackwardSimSce1c25
File format: .sas
Description: SAS code to perform baBIC backward elimination for simulations of Scenario 1 with 25% censoring and training data
File name: 17.SAS_BICbackwardIndOutcomeSimSce2corig
File format: .sas
Description: SAS code to perform BIC backward elimination for individual outcomes for simulations of Scenario 2 with case-study censoring and training data.
File name: 18.SAS_BICbackwardIndOutcomeSimSce2c25
File format: .sas
Description: SAS code to perform BIC backward elimination for individual outcomes for simulations of Scenario 2 with 25% censoring and training data.
File name: 19.SAS_baBICbackwardSimSce2corig
File format: .sas
Description: SAS code to perform baBIC backward elimination for simulations of Scenario 2 with case-study censoring and training data
File name: 20.SAS_baBICbackwardSimSce2c25
File format: .sas
Description: SAS code to perform baBIC backward elimination for simulations of Scenario 2 with 25% censoring and training data
File name: 21.SAS_BICbackwardIndOutcomeSimSce3corig
File format: .sas
Description: SAS code to perform BIC backward elimination for individual outcomes for simulations of Scenario 3 with case-study censoring and training data.
File name: 22.SAS_BICbackwardIndOutcomeSimSce3c25
File format: .sas
Description: SAS code to perform BIC backward elimination for individual outcomes for simulations of Scenario 3 with 25% censoring and training data.
File name: 23.SAS_baBICbackwardSimSce3corig
File format: .sas
Description: SAS code to perform baBIC backward elimination for simulations of Scenario 3 with case-study censoring and training data
File name: 24.SAS_baBICbackwardSimSce3c25
File format: .sas
Description: SAS code to perform baBIC backward elimination for simulations of Scenario 3 with 25% censoring and training data
File name: 25.SAS_ BICIndNumPredCstatTest_SimSce1corig
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersection, and Full methods in simulations of Scenario 1 with case-study censoring and testing data
File name: 26.SAS_ BICIndNumPredCstatTest_SimSce1c25
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersection, and Full methods in simulations of Scenario 1 with 25% censoring and testing data
File name: 27.SAS_baBICNumPredCstatTest_SimSce1corig
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in simulations of Scenario 1 with case-study censoring and testing data
File name: 28.SAS_baBICNumPredCstatTest_SimSce1c25
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in simulations of Scenario 1 with 25% censoring and testing data
File name: 29.SAS_ BICIndNumPredCstatTest_SimSce2corig
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersection, and Full methods in simulations of Scenario 2 with case-study censoring and testing data
File name: 30.SAS_ BICIndNumPredCstatTest_SimSce2c25
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersection, and Full methods in simulations of Scenario 2 with 25% censoring and testing data
File name: 31.SAS_baBICNumPredCstatTest_SimSce2corig
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in simulations of Scenario 2 with case-study censoring and testing data
File name: 32.SAS_baBICNumPredCstatTest_SimSce2c25
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in simulations of Scenario 2 with 25% censoring and testing data
File name: 33.SAS_ BICIndNumPredCstatTest_SimSce3corig
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersection, and Full methods in simulations of Scenario 3 with case-study censoring and testing data
File name: 34.SAS_ BICIndNumPredCstatTest_SimSce3c25
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersection, and Full methods in simulations of Scenario 3 with 25% censoring and testing data
File name: 35.SAS_baBICNumPredCstatTest_SimSce3corig
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in simulations of Scenario 3 with case-study censoring and testing data
File name: 36.SAS_baBICNumPredCstatTest_SimSce3c25
File format: .sas
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in simulations of Scenario 3 with 25% censoring and testing data
File name: 37. SAS_Gen500bs
File format: .sas
Description: Generation of 500 bootstrap samples from HRS case-study data 
File name: 38.SAS_BICbackwardIndOutcome500bs
File format: .sas
Description: Perform BIC backward elimination for individual outcomes in 500 bootstrap samples
File name: 39.SAS_BICIndFullNumPredCstat_BS500
File format: .sas
Description: Compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersection, and Full methods in 500 bootstrap samples
File name: 40.SAS_baBICbackward500bs
File format: .sas
Description: Perform baBIC backward elimination in 500 bootstrap samples
File name: 41.SAS_baBICNumPredCstat_BS500
File format: .sas
Description: Compute summary statistics for number of predictors and C-statistic for baBIC method in 500 bootstrap samples
File name: 42a.SAS_CumIncSce1corig
File format: .sas
Description: Compute Cumulative Incidence for simulations of Scenario 1 with case-study censoring and training data
File name: 42b.R_CumIncSce1corig
File format: .R
Description: Figure of Cumulative Incidence for simulations of Scenario 1 with case-study censoring and training data
File name: 43a.SAS_CumIncSce1c25
File format: .sas
Description: Compute Cumulative Incidence for simulations of Scenario 1 with 25% censoring and training data 
File name: 43b.R_CumIncSce1c25
File format: .R
Description: Figure of Cumulative Incidence for simulations of Scenario 1 with 25% censoring and training data
File name: 44a.SAS_CumIncSce2corig
File format: .sas
Description: Compute Cumulative Incidence for simulations of Scenario 2 with case-study censoring and training data
File name: 44b.R_CumIncSce2corig
File format: .R
Description: Figure of Cumulative Incidence for simulations of Scenario 2 with case-study censoring and training data
File name: 45a.SAS_CumIncSce2c25
File format: .sas
Description: Compute Cumulative Incidence for simulations of Scenario 2 with 25% censoring and training data
File name: 45b.R_CumIncSce2c25
File format: .R
Description: Figure of Cumulative Incidence for simulations of Scenario 2 with 25% censoring and training data
File name: 46a.SAS_CumIncSce3corig
File format: .sas
Description: Compute Cumulative Incidence for simulations of Scenario 3 with case-study censoring and training data
File name: 46b.R_CumIncSce3corig
File format: .R
Description: Figure of Cumulative Incidence for simulations of Scenario 3 with case-study censoring and training data
File name: 47a.SAS_CumIncSce3c25
File format: .sas
Description: Compute Cumulative Incidence for simulations of Scenario 3 with 25% censoring and training data
File name: 47b.R_CumIncSce3c25
File format: .R
Description: Figure of Cumulative Incidence for simulations of Scenario 3 with 25% censoring and training data

