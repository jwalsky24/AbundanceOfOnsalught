##MODEL SELECTION & CROSS VALIDATION

#Create two data sets (train + validation)
full_data <- babydata #contains 1236 observations
train_data <- full_data[1:866,] #training data set, unordered. 866 observations (70%).
cv_data <- full_data[867:1236,] #cross-validation data set, unordered. 369 observations (30%).

#Modelling
full_model <- lm(wt ~ ., data = train_data)
summary(full_model)
install.packages(ggfortify)
library(ggfortify)
autoplot(full_model)

#keeping only the covariates with  a significant p-value
lm_1 <- lm(wt ~ id + date + gestation + ht + dwt + number, data = train_data)
summary(lm_1)
autoplot(lm_1)
#testing the normality of the residuals
shapiro.test(lm_1$residuals)
#ncv test
install.packages("carData")
install.packages("car")
library(carData)
library(car)
ncvTest(lm_1)
#p value of 0.35, thus we don't have enough evidence to reject the null hypothesis of constant error variance

#using sqrt transformation makes the date covariate less significant thus we can remove it
lm_1_sqrt <- lm(sqrt(wt) ~ id + gestation + ht + dwt + number, data = train_data)
summary(lm_1_sqrt)
autoplot(lm_1_sqrt)
ncvTest(lm_1_sqrt)
#strong evidence that the error variance changes with the level of the response (fitted values)
shapiro.test(lm_1_sqrt$residuals)
#residuals seem to be nromally distributed
#testing for error independence
durbinWatsonTest(lm_1_sqrt)
#not enough evidence to prove that the errors are not independent

#AIC test for out best models
AIC(full_model)
#9374.377
AIC(lm_1)
#10731.33
AIC(lm_1_sqrt)
#3975.544

#looking for extreme residuals
hist(resid(lm_1_sqrt))
hist(resid(lm_1))

#we can remove the outliers "261" and "979"
#removing the influencial value #261
train_data_rm <- train_data[-c(261,979),]
lm_2 <- lm(wt ~ id + gestation + ht + dwt + number, data = train_data_rm)
summary(lm_2)
autoplot(lm_2)
#same tests as above
shapiro.test(lm_1$residuals)
ncvTest(lm_2)
durbinWatsonTest(lm_2)
AIC(lm_2)


#if we decide not to remove the outliers "261" and "979" I would choose lm_1 as the sqrt transformation seems to make change the result of the
#ncvTest(lm_1_sqrt), providing strong evidence that the error variance changes with the level of the response (fitted values)
final_model <- lm_1

#removing the outliers seems to make everything look and behave better which would give us the following model
final_model_rm <- lm_2

#Calculate MSEs
mse_final_model <- mean(final_model$residuals^2)
mse_final_model
mse_final_model_rm <- mean(final_model_rm$residuals^2)
mse_final_model_rm

#cross-validation prediction (out-of-sample error) on cross-validation set for both final models (with and without outliers included)
cv_final_model <- predict(final_model, test_data)
cv_final_model #out-of-sample error for final model incl. outliers
cv_final_model_rm <- predict(final_model_rm, test_data)
cv_final_model_rm #out-of-sample error for final model excl. outliers

#the codes reference :taacher's codes
#model 1 
set.seed(3245)  

# something to store lots of regression coefficients
bootResults <- array(dim = c(1000, 2))  


for(i in 1:1000){
  
  # resample our data with replacement
  bootData <-  train_data [sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM1 <- lm(sqrt(wt) ~ id, data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM1)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(id)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(id)')

for(i in 1:1000){
  # resample our data with replacement
  bootData <-  train_data[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM<- lm(sqrt(wt) ~ gestation , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(gestation)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(gestation)')

for(i in 1:1000){
  # resample our data with replacement
  bootData <- train_data[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(sqrt(wt) ~ ht , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(ht)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(ht)')

for(i in 1:1000){
  # resample our data with replacement
  bootData <- train_data [sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative 
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)reality
  bootLM <- lm(sqrt(wt) ~ dwt , data = bootData)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(dwt)')
geom_line(hist(bootResults[,1]))
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(dwt)')

for(i in 1:1000){
  # resample our data with replacement
  bootData <- train_data[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(sqrt(wt) ~ number , data = bootData)
  
  # store the coefs
  bootResults5[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(number)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(number)')

#model 2
set.seed(3245)  

# something to store lots of regression coefficients
bootResults <- array(dim = c(1000, 2))  


for(i in 1:1000){
  
  # resample our data with replacement
  bootData <-  train_data_rm[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM1 <- lm(sqrt(wt) ~ id , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM1)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(id)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(id)')

for(i in 1:1000){
  # resample our data with replacement
  bootData <- train_data_rm[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM<- lm(wt ~ gestation , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(gestation)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(gestation)')

for(i in 1:1000){
  # resample our data with replacement
  bootData <-train_data_rm[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ ht , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(ht)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(ht)')

for(i in 1:1000){
  # resample our data with replacement
  bootData <- train_data_rm[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ dwt , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(dwt)')
geom_line(hist(bootResults[,1]))
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(dwt)')

for(i in 1:1000){
  # resample our data with replacement
  bootData <- train_data_rm[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ number , data = bootData)
  
  # store the coefs
  bootResults5[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(number)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(number)')

