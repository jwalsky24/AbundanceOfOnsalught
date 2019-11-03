#Modelling

full_model <- lm(wt ~ ., data = babydata)
summary(full_model)
install.packages(ggfortify)
library(ggfortify)
autoplot(full_model)

#keeping only the covariates with  a significant p-value, date also becomes not significant
lm_1 <- lm(wt ~ gestation + ht + dwt + number, data = babydata)
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
#p value of 0.57, thus we don't have enough evidence to reject the null hypothesis of constant error variance

#using sqrt transformation makes the date covariate less significant thus we can remove it
lm_1_sqrt <- lm(sqrt(wt) ~ gestation + ht + dwt + number, data = babydata)
summary(lm_1_sqrt)
autoplot(lm_1_sqrt)
ncvTest(lm_1_sqrt)
#strong evidence that the error variance changes with the level of the response (fitted values)
shapiro.test(lm_1_sqrt$residuals)
#some evidence that the residuals might not be normally distributed
#testing for error independence
durbinWatsonTest(lm_1_sqrt)
#not enough evidence to prove that the errors are not independent

#AIC test for out best models
AIC(full_model)
#9305.99
AIC(lm_1)
#10740.85
AIC(lm_1_sqrt)
#3985.143

#looking for extreme residuals
hist(resid(lm_1_sqrt))
hist(resid(lm_1))

#we can remove the outliers "261" and "979"
#removing the influencial value #261
babydata_rm <- babydata[-c(261),]
lm_2 <- lm(wt ~ gestation + ht + dwt + number, data = babydata_rm)
summary(lm_2)
autoplot(lm_2)
#same tests as above
shapiro.test(lm_1$residuals)
ncvTest(lm_2)
durbinWatsonTest(lm_2)
AIC(lm_2)

#looking into first order interactions 
lm_int_full <- lm(wt ~ (.)^2, data = babydata)
summary(lm_int_full)
autoplot(lm_int_full)
AIC(lm_int_full)
shapiro.test(lm_int_full$residuals)
ncvTest(lm_int_full)
durbinWatsonTest(lm_int_full)

#looking at the high significance interactions and solo covariate
lm_int_1 <- lm(wt ~ gestation + ht + number + ded + ded:dwt + race:age  + gestation:parity, data = babydata)
summary(lm_int_1)
autoplot(lm_int_1)
AIC(lm_int_1)
shapiro.test(lm_int_1$residuals)
ncvTest(lm_int_1)
durbinWatsonTest(lm_int_1)


lm_int_2 <- lm(wt ~ gestation + number + ded + ded:dwt + race:age + ht:ded + gestation:dage, data = babydata)
summary(lm_int_2)
autoplot(lm_int_2)
AIC(lm_int_2)
shapiro.test(lm_int_2$residuals)
ncvTest(lm_int_2)
durbinWatsonTest(lm_int_2)

#out of the models that take into consideration the interacting terms I prefer lm_int_2, but we could use lm_int_1 just as well
lm_int_final <- lm_int_2
#we can remove entry 261 from these models as well in order to get better results


#if we decide not to remove the outliers "261" and "979" I would choose lm_1 as the sqrt transformation seems to make change the result of the
#ncvTest(lm_1_sqrt), providing strong evidence that the error variance changes with the level of the response (fitted values)
final_model <- lm_1

#removing the outliers seems to make everything look and behave better which would give us the following model
final_model_rm <- lm_2
