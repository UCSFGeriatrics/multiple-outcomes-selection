# multiple-outcomes-selection
This respository corresponds to the following manuscript:
## A Novel Method for Identifying a Parsimonious and Accurate Predictive Model for Multiple Clinical Outcomes

L. Grisell Diaz-Ramirez MS,a,b Sei J. Lee MD,a,b Alexander K. Smith MD,a,b Siqi Gan MS,a,b W. John Boscardin PhDa,b

a Division of Geriatrics, University of California, San Francisco
3333 California St., Suite 380, Box 1265, San Francisco, CA 94143, United States

b San Francisco Veterans Affairs (VA) Medical Center
4150 Clement Street, 181G, San Francisco, CA 94121, United States


### Description of data and SAS and R codes for reproducing the results of this article:
#### File name: originaldata
Link to File: [.csv](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/originaldata.csv) <br>
Description: HRS data with 39 predictors and 4 outcomes of 5,531 respondents

#### File name: R_LASSOselection
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/R_LASSOselection.R) <br>
Description: R code to perform LASSO Selection based on Optimal λ at Minimum BIC for Individual Outcome and Union methods

#### File name: SAS_ LassoIndUnionNumPredCstat_BS500
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/SAS_%20LassoIndUnionNumPredCstat_BS500.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/SAS_%20LassoIndUnionNumPredCstat_BS500.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for LASSO Individual and Union methods using 500 bootstrap samples

#### File name: R_Fig4.nBIC_BICvsNumPred
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/R_Fig4.nBIC_BICvsNumPred.R) <br>
Description: R code for Fig. 4: Selection with the baBIC method and Individual Outcome Methods in the Case-study Data

#### File name: R_Fig7.NumPred
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/R_Fig7.NumPred.R) <br>
Description: R code for Fig. 7: Comparison of Number of Predictors with 95% Confidence Intervals Using Case-study Bootstrap Data and Simulations with Correlated and Uncorrelated Outcomes

#### File name: R_Fig8.Cstat
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/R_Fig8.Cstat.R) <br>
Description: R code for Fig. 8: Comparison of Harrell’s C-statistic Using Case-study Bootstrap Data and Simulations with Correlated and Uncorrelated Outcomes

#### File name: 1.SAS_BICbackwardIndOutcomeCR
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/1.SAS_BICbackwardIndOutcomeCR.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/1.SAS_BICbackwardIndOutcomeCR.sas)
Description: SAS code to perform BIC backward elimination by Outcome using HRS original dataset. It uses Cox regression for Death, and Competing-risk regression for rest of outcomes.

#### File name: 2.SAS_BICbackwardIndOutcomeCox
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/2.SAS_BICbackwardIndOutcomeCox.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/2.SAS_BICbackwardIndOutcomeCox.sas) <br>
Description: SAS code to perform BIC backward elimination by Outcome using HRS original dataset. It uses Cox regression for 4 outcomes and Wolbers et. al (2009) adaptation to the Competing-risk settings.

#### File name: 3.SAS_baBICbackwardCR
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/3.SAS_baBICbackwardCR.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/3.SAS_baBICbackwardCR.sas)
Description: SAS code to perform best average BIC (baBIC) backward elimination using HRS original dataset. It uses Cox regression for Death, and Competing-risk regression for rest of outcomes. baBIC=absolute(BICk-BICbest)/absolute(BICfull-BICbest): BICfull and BICbest are the BICs of the full and best individual models.

#### File name: 4.SAS_baBICbackwardCox
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/4.SAS_baBICbackwardCox.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/4.SAS_baBICbackwardCox.sas)  <br>
Description: SAS code to perform best average BIC (baBIC) backward elimination using HRS original dataset. It uses Cox regression for 4 outcomes and Wolbers et. al (2009) adaptation to the Competing-risk settings. baBIC=absolute(BICk-BICbest)/absolute(BICfull-BICbest): BICfull and BICbest are the BICs of the full and best individual models.

#### File name: 5a.SAS_BetaCoefficientsSce1_2_3
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/5a.SAS_BetaCoefficientsSce1_2_3.txt) <br>
Description: Compute Beta coefficients used in Scenarios 1, 2, and 3 of the simulation study

#### File name: 5b.R_CleanBetaCoefficientsSce1_2_3
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/5b.R_CleanBetaCoefficientsSce1_2_3.R) <br>
Description: Produce cleaner format of Beta coefficients used in Scenarios 1, 2, and 3 of the simulation study

#### File name: 6a.SAS_SimSce1CorrTrain
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/6a.SAS_SimSce1CorrTrain.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/6a.SAS_SimSce1CorrTrain.sas) <br>
Description: SAS code to generate simulated training datasets with correlated outcomes and predictors from baBIC model of HRS case-study data. Compute some statistics (Scenario 1, correlated outcomes)

#### File name: 6b.SAS_SimSce1CorrTest
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/6b.SAS_SimSce1CorrTest.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/6b.SAS_SimSce1CorrTest.sas) <br>
Description: SAS code to generate simulated testing datasets with correlated outcomes and predictors from baBIC model of HRS case-study data. Compute some statistics (Scenario 1, correlated outcomes)

#### File name: 7a.SAS_SimSce1UncorrTrain
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/7a.SAS_SimSce1UncorrTrain.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/7a.SAS_SimSce1UncorrTrain.sas)<br>
Description: SAS code to generate simulated training datasets with uncorrelated outcomes and predictors from baBIC model of HRS case-study data. Compute some statistics (Scenario 1, uncorrelated outcomes)

#### File name: 7b.SAS_SimSce1UncorrTest
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/7b.SAS_SimSce1UncorrTest.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/7b.SAS_SimSce1UncorrTest.sas) <br>
Description: SAS code to generate simulated testing datasets with uncorrelated outcomes and predictors from baBIC model of HRS case-study data. Compute some statistics (Scenario 1, uncorrelated outcomes)

#### File name: 8a.SAS_SimSce2CorrTrain
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/8a.SAS_SimSce2CorrTrain.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/8a.SAS_SimSce2CorrTrain.sas)<br>
Description: SAS code to generate simulated training datasets with correlated outcomes and predictors from Best Individual models of HRS case-study data. Compute some statistics (Scenario 2, correlated outcomes)

#### File name: 8b.SAS_SimSce2CorrTest
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/8b.SAS_SimSce2CorrTest.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/8b.SAS_SimSce2CorrTest.sas) <br>
Description: SAS code to generate simulated testing datasets with correlated outcomes and predictors from Best Individual models of HRS case-study data. Compute some statistics (Scenario 2, correlated outcomes)

#### File name: 9a.SAS_SimSce2UncorrTrain
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/9a.SAS_SimSce2UncorrTrain.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/9a.SAS_SimSce2UncorrTrain.sas) <br>
Description: SAS code to generate simulated training datasets with uncorrelated outcomes and predictors from Best Individual models of HRS case-study data. Compute some statistics (Scenario 2, uncorrelated outcomes)

#### File name: 9b.SAS_SimSce2UncorrTest
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/9b.SAS_SimSce2UncorrTest.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/9b.SAS_SimSce2UncorrTest.sas) <br>
Description: SAS code to generate simulated testing datasets with uncorrelated outcomes and predictors from Best Individual models of HRS case-study data. Compute some statistics (Scenario 2, uncorrelated outcomes)

#### File name: 10a.SAS_SimSce3CorrTrain
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/10a.SAS_SimSce3CorrTrain.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/10a.SAS_SimSce3CorrTrain.sas) <br>
Description: SAS code to generate simulated training datasets with correlated outcomes and predictors from Full model of HRS case-study data. Compute some statistics (Scenario 3, correlated outcomes)

#### File name: 10b.SAS_SimSce3CorrTest
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/10b.SAS_SimSce3CorrTest.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/10b.SAS_SimSce3CorrTest.sas) <br>
Description: SAS code to generate simulated testing datasets with correlated outcomes and predictors from Full model of HRS case-study data. Compute some statistics (Scenario 3, correlated outcomes)

#### File name: 11a.SAS_SimSce3UncorrTrain
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/11a.SAS_SimSce3UncorrTrain.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/11a.SAS_SimSce3UncorrTrain.sas) <br>
Description: SAS code to generate simulated training datasets with uncorrelated outcomes and predictors from Full model of HRS case-study data. Compute some statistics (Scenario 3, uncorrelated outcomes)

#### File name: 11b.SAS_SimSce3UncorrTest
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/11b.SAS_SimSce3UncorrTest.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/11b.SAS_SimSce3UncorrTest.sas) <br>
Description: SAS code to generate simulated testing datasets with uncorrelated outcomes and predictors from Full model of HRS case-study data. Compute some statistics (Scenario 3, uncorrelated outcomes)

#### File name: 12.SAS_BICbackwardIndOutcomeSimSce1Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/12.SAS_BICbackwardIndOutcomeSimSce1Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/12.SAS_BICbackwardIndOutcomeSimSce1Corr.sas)
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 1 simulated training correlated data.

#### File name: 13.SAS_BICbackwardIndOutcomeSimSce1Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/13.SAS_BICbackwardIndOutcomeSimSce1Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/13.SAS_BICbackwardIndOutcomeSimSce1Uncorr.sas) <br>
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 1 simulated training uncorrelated data.

#### File name: 14.SAS_baBICbackwardSimSce1Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/14.SAS_baBICbackwardSimSce1Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/14.SAS_baBICbackwardSimSce1Corr.sas) <br>
Description: SAS code to perform baBIC backward elimination for Scenario 1 simulated training correlated data

#### File name: 15.SAS_baBICbackwardSimSce1Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/15.SAS_baBICbackwardSimSce1Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/15.SAS_baBICbackwardSimSce1Uncorr.sas) <br>
Description: SAS code to perform baBIC backward elimination for Scenario 1 simulated training uncorrelated data

#### File name: 16.SAS_BICbackwardIndOutcomeSimSce2Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/16.SAS_BICbackwardIndOutcomeSimSce2Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/16.SAS_BICbackwardIndOutcomeSimSce2Corr.sas) <br>
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 2 simulated training correlated data

#### File name: 17.SAS_BICbackwardIndOutcomeSimSce2Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/17.SAS_BICbackwardIndOutcomeSimSce2Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/17.SAS_BICbackwardIndOutcomeSimSce2Uncorr.sas) <br>
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 2 simulated training uncorrelated data

#### File name: 18.SAS_baBICbackwardSimSce2Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/18.SAS_baBICbackwardSimSce2Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/18.SAS_baBICbackwardSimSce2Corr.sas) <br>
Description: SAS code to perform baBIC backward elimination for Scenario 2 simulated training correlated data

#### File name: 19.SAS_baBICbackwardSimSce2Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/19.SAS_baBICbackwardSimSce2Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/19.SAS_baBICbackwardSimSce2Uncorr.sas) <br>
Description: SAS code to perform baBIC backward elimination for Scenario 2 simulated training uncorrelated data

#### File name: 20.SAS_BICbackwardIndOutcomeSimSce3Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/20.SAS_BICbackwardIndOutcomeSimSce3Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/20.SAS_BICbackwardIndOutcomeSimSce3Corr.sas)<br>
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 3 simulated training correlated data

#### File name: 21.SAS_BICbackwardIndOutcomeSimSce3Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/21.SAS_BICbackwardIndOutcomeSimSce3Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/21.SAS_BICbackwardIndOutcomeSimSce3Uncorr.sas) <br>
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 3 simulated training uncorrelated data

#### File name: 22.SAS_baBICbackwardSimSce3Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/22.SAS_baBICbackwardSimSce3Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/22.SAS_baBICbackwardSimSce3Corr.sas) <br>
Description: SAS code to perform baBIC backward elimination for Scenario 3 simulated training correlated data

#### File name: 23.SAS_baBICbackwardSimSce3Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/23.SAS_baBICbackwardSimSce3Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/23.SAS_baBICbackwardSimSce3Uncorr.sas) <br>
Description: SAS code to perform baBIC backward elimination for Scenario 3 simulated training uncorrelated data

#### File name: 24.SAS_BICIndFullNumPredCstatTest_SimSce1Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/24.SAS_BICIndFullNumPredCstatTest_SimSce1Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/24.SAS_BICIndFullNumPredCstatTest_SimSce1Corr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in Scenario 1 simulated testing correlated data

#### File name: 25.SAS_BICIndFullNumPredCstatTest_SimSce1Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/25.SAS_BICIndFullNumPredCstatTest_SimSce1Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/25.SAS_BICIndFullNumPredCstatTest_SimSce1Uncorr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in Scenario 1 simulated testing uncorrelated data

#### File name: 26.SAS_baBICNumPredCstatTest_SimSce1Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/26.SAS_baBICNumPredCstatTest_SimSce1Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/26.SAS_baBICNumPredCstatTest_SimSce1Corr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in Scenario 1 simulated testing correlated data

#### File name: 27.SAS_baBICNumPredCstatTest_SimSce1Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/27.SAS_baBICNumPredCstatTest_SimSce1Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/27.SAS_baBICNumPredCstatTest_SimSce1Uncorr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in Scenario 1 simulated testing uncorrelated data

#### File name: 28.SAS_BICIndFullNumPredCstatTest_SimSce2Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/28.SAS_BICIndFullNumPredCstatTest_SimSce2Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/28.SAS_BICIndFullNumPredCstatTest_SimSce2Corr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in Scenario 2 simulated testing correlated data

#### File name: 29.SAS_BICIndFullNumPredCstatTest_SimSce2Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/29.SAS_BICIndFullNumPredCstatTest_SimSce2Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/29.SAS_BICIndFullNumPredCstatTest_SimSce2Uncorr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in Scenario 2 simulated testing uncorrelated data

#### File name: 30.SAS_baBICNumPredCstatTest_SimSce2Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/30.SAS_baBICNumPredCstatTest_SimSce2Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/30.SAS_baBICNumPredCstatTest_SimSce2Corr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in Scenario 2 simulated testing correlated data

#### File name: 31.SAS_baBICNumPredCstatTest_SimSce2Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/31.SAS_baBICNumPredCstatTest_SimSce2Uncorr.txt) | [.sas]( https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/31.SAS_baBICNumPredCstatTest_SimSce2Uncorr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in Scenario 2 simulated testing uncorrelated data

#### File name: 32.SAS_BICIndFullNumPredCstatTest_SimSce3Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/32.SAS_BICIndFullNumPredCstatTest_SimSce3Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/32.SAS_BICIndFullNumPredCstatTest_SimSce3Corr.sas)
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in Scenario 3 simulated testing correlated data

#### File name: 33.SAS_BICIndFullNumPredCstatTest_SimSce3Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/33.SAS_BICIndFullNumPredCstatTest_SimSce3Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/33.SAS_BICIndFullNumPredCstatTest_SimSce3Uncorr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in Scenario 3 simulated testing uncorrelated data

#### File name: 34.SAS_baBICNumPredCstatTest_SimSce3Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/34.SAS_baBICNumPredCstatTest_SimSce3Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/34.SAS_baBICNumPredCstatTest_SimSce3Corr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in Scenario 3 simulated testing correlated data

#### File name: 35.SAS_baBICNumPredCstatTest_SimSce3Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/35.SAS_baBICNumPredCstatTest_SimSce3Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/35.SAS_baBICNumPredCstatTest_SimSce3Uncorr.sas) <br>
Description: SAS code to compute summary statistics for number of predictors and C-statistic for baBIC method in Scenario 3 simulated testing uncorrelated data

#### File name: 36. SAS_Gen500bs
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/36.SAS_Gen500bs.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/36.SAS_Gen500bs.sas) <br>
Description: Generation of 500 bootstrap samples from HRS case-study data 

#### File name: 37.SAS_BICbackwardIndOutcome500bs
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/37.SAS_BICbackwardIndOutcome500bs.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/37.SAS_BICbackwardIndOutcome500bs.sas)<br>
Description: Perform BIC backward elimination for individual outcomes in 500 bootstrap samples

#### File name: 38.SAS_BICIndFullNumPredCstat_BS500
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/38.SAS_BICIndFullNumPredCstat_BS500.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/38.SAS_BICIndFullNumPredCstat_BS500.sas) <br>
Description: Compute summary statistics for number of predictors and C-statistic for Individual, Union, Intersect, and Full methods in 500 bootstrap samples

#### File name: 39.SAS_baBICbackward500bs
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/39.SAS_baBICbackward500bs.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/39.SAS_baBICbackward500bs.sas) <br>
Description: Perform baBIC backward elimination in 500 bootstrap samples

#### File name: 40.SAS_baBICNumPredCstat_BS500
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/40.SAS_baBICNumPredCstat_BS500.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/40.SAS_baBICNumPredCstat_BS500.sas) <br>
Description: Compute summary statistics for number of predictors and C-statistic for baBIC method in 500 bootstrap samples

#### File name: 41a.SAS_CumIncSce1Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/41a.SAS_CumIncSce1Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/41a.SAS_CumIncSce1Corr.sas)<br>
Description: Compute Cumulative Incidence for Scenario 1 simulated training correlated data

#### File name: 41b.R_CumIncSce1Corr
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/41b.R_CumIncSce1Corr.R) <br>
Description: Plot Compute Cumulative Incidence for Scenario 1 simulated training correlated data

#### File name: 42a.SAS_CumIncSce1Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/42a.SAS_CumIncSce1Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/42a.SAS_CumIncSce1Uncorr.sas) <br>
Description: Compute Cumulative Incidence for Scenario 1 simulated training uncorrelated data

#### File name: 42b.R_CumIncSce1Uncorr
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/42b.R_CumIncSce1Uncorr.R) <br>
Description: Plot Cumulative Incidence for Scenario 1 simulated training uncorrelated data

#### File name: 43a.SAS_CumIncSce2Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/43a.SAS_CumIncSce2Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/43a.SAS_CumIncSce2Corr.sas) <br>
Description: Compute Cumulative Incidence for Scenario 2 simulated training correlated data

#### File name: 43b.R_CumIncSce2Corr
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/43b.R_CumIncSce2Corr.R) <br>
Description: Plot Cumulative Incidence for Scenario 2 simulated training correlated data

#### File name: 44a.SAS_CumIncSce2Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/44a.SAS_CumIncSce2Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/44a.SAS_CumIncSce2Uncorr.sas) <br>
Description: Compute Cumulative Incidence for Scenario 2 simulated training uncorrelated data

#### File name: 44b.R_CumIncSce2Uncorr
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/44b.R_CumIncSce2Uncorr.R) <br>
Description: Plot Cumulative Incidence for Scenario 2 simulated training uncorrelated data

#### File name: 45a.SAS_CumIncSce3Corr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/45a.SAS_CumIncSce3Corr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/45a.SAS_CumIncSce3Corr.sas) <br>
Description: Compute Cumulative Incidence for Scenario 3 simulated training correlated data

#### File name: 45b.R_CumIncSce3Corr
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/45b.R_CumIncSce3Corr.R) <br>
Description: Plot Cumulative Incidence for Scenario 3 simulated training correlated data

#### File name: 46a.SAS_CumIncSce3Uncorr
Link to File: [.txt](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/46a.SAS_CumIncSce3Uncorr.txt) | [.sas](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/46a.SAS_CumIncSce3Uncorr.sas) <br>
Description: Compute Cumulative Incidence for Scenario 3 simulated training uncorrelated data

#### File name: 46b.R_CumIncSce3Uncorr
Link to File: [.R](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/46b.R_CumIncSce3Uncorr.R) <br>
Description: Plot Cumulative Incidence for Scenario 3 simulated training uncorrelated data


