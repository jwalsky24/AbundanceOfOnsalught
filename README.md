# AbundanceOfOnsalught

Investigation Into Predictors Of
Birth Weight Among Human Babies
Via Linear Regression Analysis

Files included in the repository:

babies23.data <- This file containts the original raw data used for the analysis.
BabyWeightExtremes.R <- This R code isolates the extreme values documented for baby weight and gestation period.
bootstrapping.R <- The bootstrapping algorithm code.
data_cleaning.R <- The initial code to clean the data: replace 9,99,999 for unknowns with NA, take out columns of data that do not add to analysis, rename columns that have same name, change units of measurements to metric system.
modeling.R <- This R file contains the code to model the data, explores the possible cominations of covariates. 
MSE.R <- This R file conducts the Mean Square Error calculations as well as the cross validation analysis.
PreliminaryExploration.pdf <- PDF of the initial basic exploration of the raw data, contains plots and test results.
