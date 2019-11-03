#Modelling
full_model <- lm(wt ~ ., data = babydata)
summary(full_model)
install.packages(ggfortify)
library(ggfortify)
autoplot(full_model)

#keeping only the covariates with  a significant p-value
lm_1 <- lm(wt ~ id + date + gestation + ht + dwt + number, data = babydata)
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
lm_1_sqrt <- lm(sqrt(wt) ~ id + gestation + ht + dwt + number, data = babydata)
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
babydata_rm <- babydata[-c(261,979),]
lm_2 <- lm(wt ~ id + gestation + ht + dwt + number, data = babydata_rm)
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
final_model_rm <- lm_2babydata$ed[17]=6
babydata$ed[51]=6
babydata$ed[153]=6
babydata$ed[378]=6
babydata$ed[705]=6
babydata$ed[902]=6
babydata$ed[1004]=6
babydata$ded[905]=6
babydata$ded[477]=6
babydata$ded[517]=6
babydata$ded[403]=6
babydata$ded[157]=6
rm(dht_cm)
rm(dwt_g)
rm(ht_cm)
rm(i)
rm(mwt_g)
rm(wt_g)

#Modelling
full_model <- lm(wt ~ ., data = babydata)
summary(full_model)
install.packages(ggfortify)
library(ggfortify)
autoplot(full_model)

#keeping only the covariates with  a significant p-value
lm_1 <- lm(wt ~ id + date + gestation + ht + dwt + number, data = babydata)
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
lm_1_sqrt <- lm(sqrt(wt) ~ id + gestation + ht + dwt + number, data = babydata)
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
babydata_rm <- babydata[-c(261,979),]
lm_2 <- lm(wt ~ id + gestation + ht + dwt + number, data = babydata_rm)
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

set.seed(3245)  

# something to store lots of regression coefficients
bootResults <- array(dim = c(1000, 2))  


for(i in 1:1000){
  
  # resample our data with replacement
  bootData <- babydata_rm [sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM1 <- lm(wt ~ id , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM1)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(id)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(id)')

for(i in 1:1000){
# resample our data with replacement
bootData <- babydata_rm [sample(1:1000, 1000, replace = T),]

# fit the model under this alternative reality
bootLM<- lm(wt ~ gestation , data = bootData)

# store the coefs
bootResults[i, ] <- coef(bootLM)

}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(gestation)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(gestation)')

for(i in 1:1000){
# resample our data with replacement
bootData <- babydata_rm [sample(1:1000, 1000, replace = T),]

# fit the model under this alternative reality
bootLM <- lm(wt ~ ht , data = bootData)

# store the coefs
bootResults[i, ] <- coef(bootLM)

}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution')

for(i in 1:1000){
# resample our data with replacement
bootData <- babydata_rm [sample(1:1000, 1000, replace = T),]

# fit the model under this alternative reality
bootLM <- lm(wt ~ dwt , data = bootData)

# store the coefs
bootResults[i, ] <- coef(bootLM)

}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(dwt)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(dwt)')

for(i in 1:1000){
# resample our data with replacement
bootData <- babydata_rm [sample(1:1000, 1000, replace = T),]

# fit the model under this alternative reality
bootLM <- lm(wt ~ number , data = bootData)

# store the coefs
bootResults5[i, ] <- coef(bootLM)

}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(number)')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(number)')
