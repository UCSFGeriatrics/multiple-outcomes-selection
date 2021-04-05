# multiple-outcomes-selection
This respository corresponds to manuscript "A Novel Method for Identifying a Parsimonious and Accurate Predictive Model for Multiple Clinical Outcomes" authored by L. Grisell Diaz-Ramirez MS,a,b Sei J. Lee MD,a,b Alexander K. Smith MD,a,b Siqi Gan MS,a,b W. John Boscardin PhDa,b

A Novel Method for Identifying a Parsimonious and Accurate Predictive Model for Multiple Clinical Outcomes,
Computer Methods and Programs in Biomedicine,

Authors: L. Grisell Diaz-Ramirez, Sei J. Lee, Alexander K. Smith, Siqi Gan, W. John Boscardin,

Volume 204,
2021,
106073,
ISSN 0169-2607,
[https://doi.org/10.1016/j.cmpb.2021.106073.](https://www.sciencedirect.com/science/article/pii/S0169260721001486)

Abstract: 

Background and Objective
Most methods for developing clinical prognostic models focus on identifying parsimonious and accurate models to predict a single outcome; however, patients and providers often want to predict multiple outcomes simultaneously. As an example, for older adults one is often interested in predicting nursing home admission as well as mortality. We propose and evaluate a novel predictor-selection computing method for multiple outcomes and provide the code for its implementation.

Methods
Our proposed algorithm selected the best subset of common predictors based on the minimum average normalized Bayesian Information Criterion (BIC) across outcomes: the Best Average BIC (baBIC) method. We compared the predictive accuracy (Harrell's C-statistic) and parsimony (number of predictors) of the model obtained using the baBIC method with: 1) a subset of common predictors obtained from the union of optimal models for each outcome (Union method), 2) a subset obtained from the intersection of optimal models for each outcome (Intersection method), and 3) a model with no variable selection (Full method). We used a case-study data from the Health and Retirement Study (HRS) to demonstrate our method and conducted a simulation study to investigate performance.

Results
In the case-study data and simulations, the average Harrell's C-statistics across outcomes of the models obtained with the baBIC and Union methods were comparable. Despite the similar discrimination, the baBIC method produced more parsimonious models than the Union method. In contrast, the models selected with the Intersection method were the most parsimonious, but with worst predictive accuracy, and the opposite was true in the Full method. In the simulations, the baBIC method performed well by identifying many of the predictors selected in the baBIC model of the case-study data most of the time and excluding those not selected in the majority of the simulations.

Conclusions

Our method identified a common subset of variables to predict multiple clinical outcomes with superior balance between parsimony and predictive accuracy to current methods.

Keywords: backward elimination; Bayesian Information Criterion; prognostic models; survival analysis; variable selection

link: https://authors.elsevier.com/a/1cs6FcV4L9bGE
