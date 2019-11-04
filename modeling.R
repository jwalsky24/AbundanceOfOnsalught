#Modelling

# Fit model with all possible variables
full_model <- lm(wt ~ ., data = babydata)
summary(full_model)

# Plots residuals vs fitted, also Q-Q plot
install.packages("ggfortify")
library(ggfortify)
autoplot(full_model)
 
# "Slimmer" model keeping only the covariates with  a significant p-value
lm_1 <- lm(wt ~ gestation + ht + dwt + number, data = babydata)
summary(lm_1)

# same plots as before
autoplot(lm_1)

#testing the normality of the residuals
shapiro.test(lm_1$residuals)

#ncv test
install.packages("carData")
install.packages("car")
library(carData)
library(car)
ncvTest(lm_1)# we don't have enough evidence to reject the null hypothesis of constant error variance


# Using sqrt transformation is worth checking, but ultimately it makes 
# the covariates less significant thus we can remove it
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
# 9439.451
AIC(lm_1)
# 9439.677
AIC(lm_1_sqrt)
# 3501.942

# looking for extreme residuals
hist(resid(lm_1_sqrt))
hist(resid(lm_1))

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

#out of the models that take into consideration the interacting terms we prefer lm_int_2
final_model <- lm_int_2

summary(final_model)

