# multiple-outcomes-selection
This respository corresponds to the following manuscript:
## A Novel Method for Identifying a Parsimonious and Accurate Predictive Model for Multiple Clinical Outcomes
L. Grisell Diaz-Ramirez MS,a,b Sei J. Lee MD,a,b Alexander K. Smith MD,a,b Siqi Gan MS,a,b W. John Boscardin PhDa,b

a Division of Geriatrics, University of California, San Francisco
3333 California St., Suite 380, Box 1265, San Francisco, CA 94143, United States

b San Francisco Veterans Affairs (VA) Medical Center
4150 Clement Street, 181G, San Francisco, CA 94121, United States

### Description of data and SAS and R codes for reproducing the results of this article:
#### [File name: originaldata](https://github.com/UCSFGeriatrics/multiple-outcomes-selection/blob/master/originaldata.csv)
File format: .csv
Description: HRS data with 39 predictors and 4 outcomes of 5,531 respondents
#### File name: R_LASSOselection
File format: .R
Description: R code to perform LASSO Selection based on Optimal λ at Minimum BIC
#### File name: 1.SAS_BICbackwardIndOutcomeCR
File format: .txt
Description: SAS code to perform BIC backward elimination by Outcome using HRS original dataset. It uses Cox regression for Death, and Competing-risk regression for rest of outcomes.
#### File name: 2.SAS_BICbackwardIndOutcomeCox
File format: .txt
Description: SAS code to perform BIC backward elimination by Outcome using HRS original dataset. It uses Cox regression for 4 outcomes and Wolbers et. al (2009) adaptation to the Competing-risk settings.
#### File name: 3.SAS_baBICbackwardCR
File format: .txt
Description: SAS code to perform best average BIC (baBIC) backward elimination using HRS original dataset. It uses Cox regression for Death, and Competing-risk regression for rest of outcomes. baBIC=absolute(BICk-BICbest)/absolute(BICfull-BICbest): BICfull and BICbest are the BICs of the full and best individual models.
#### File name: 4.SAS_baBICbackwardCox
File format: .txt
Description: SAS code to perform best average BIC (baBIC) backward elimination using HRS original dataset. It uses Cox regression for 4 outcomes and Wolbers et. al (2009) adaptation to the Competing-risk settings. baBIC=absolute(BICk-BICbest)/absolute(BICfull-BICbest): BICfull and BICbest are the BICs of the full and best individual models.
#### File name: 5.SAS_SimSce2Corr
File format: .txt
Description: SAS code to generate simulated datasets with correlated outcomes and predictors from Best Individual models and compute some statistics (Scenario 2, correlated outcomes)
#### File name: 6.SAS_SimSce1Corr
File format: .txt
Description: SAS code to generate simulated datasets with correlated outcomes and predictors from baBIC model and compute some statistics (Scenario 1, correlated outcomes)
#### File name: 7.SAS_SimSce2Uncorr
File format: .txt
Description: SAS code to generate simulated datasets with uncorrelated outcomes and predictors from Best Individual models and compute some statistics (Scenario 2, uncorrelated outcomes)
#### File name: 8.SAS_SimSce1Uncorr
File format: .txt
Description: SAS code to generate simulated datasets with uncorrelated outcomes and predictors from baBIC model and compute some statistics (Scenario 1, uncorrelated outcomes)
#### File name: 9.SAS_BICbackwardIndOutcomeSimSce1Corr
File format: .txt
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 1 simulated correlated data and to obtain Union model.
File name: 10.SAS_baBICbackwardSimSce1Corr
File format: .txt
Description: SAS code to perform BIC backward elimination using baBIC for Scenario 1 simulated correlated data
File name: 11.SAS_BICbackwardIndOutcomeSimSce1Uncorr
File format: .txt
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 1 simulated uncorrelated data and to obtain Union model.
#### File name: 12.SAS_baBICbackwardSimSce1Uncorr
File format: .txt
Description: SAS code to perform BIC backward elimination using baBIC for Scenario 1 simulated uncorrelated data
#### File name: 13.SAS_BICbackwardIndOutcomeSimSce2Corr
File format: .txt
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 2 simulated correlated data and to obtain Union model.
#### File name: 14.SAS_baBICbackwardSimSce2Corr
File format: .txt
Description: SAS code to perform BIC backward elimination using baBIC for Scenario 2 simulated correlated data
#### File name: 15.SAS_BICbackwardIndOutcomeSimSce2Uncorr
File format: .txt
Description: SAS code to perform BIC backward elimination for individual outcomes for Scenario 2 simulated uncorrelated data and to obtain Union model.
#### File name: 16.SAS_baBICbackwardSimSce2Uncorr
File format: .txt
Description: SAS code to perform BIC backward elimination using baBIC for Scenario 2 simulated uncorrelated data

