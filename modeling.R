# Modeling

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

# Same plots as before
autoplot(lm_1)

# Statistical test for normality of residuals
shapiro.test(lm_1$residuals)
# We fail to reject the null hypothesis of normality

# Non-Constant Error Variance Test
install.packages("carData")
install.packages("car")
library(carData)
library(car)
ncvTest(lm_1)
# We fail to reject the null hypothesis of constant error variance


# Using sqrt transformation is worth checking, but ultimately it makes 
# the covariates less significant thus we can remove it
lm_1_sqrt <- lm(sqrt(wt) ~ gestation + ht + dwt + number, data = babydata)
summary(lm_1_sqrt)
autoplot(lm_1_sqrt)
ncvTest(lm_1_sqrt)
# Strong evidence that the error variance changes with the level of the response (fitted values)

shapiro.test(lm_1_sqrt$residuals)
# Some evidence that the residuals might not be normally distributed

# Testing for error independence
durbinWatsonTest(lm_1_sqrt)
# We fail to reject the null hypothesis of independence 

#AIC test for out best models
AIC(full_model)
# 9439.451
AIC(lm_1)
# 9439.677
AIC(lm_1_sqrt)
# 3501.942

# Output histograms to inspect shape, also for extreme residuals
hist(resid(lm_1_sqrt))
hist(resid(lm_1))

# Removing the influencial value from row 261
babydata_rm <- babydata[-c(261),]
lm_2 <- lm(wt ~ gestation + ht + dwt + number, data = babydata_rm)
summary(lm_2)
autoplot(lm_2)

# Same assumptions tests as above
shapiro.test(lm_1$residuals)
ncvTest(lm_2)
durbinWatsonTest(lm_2)
AIC(lm_2)

# Looking into first order interactions 
lm_int_full <- lm(wt ~ (.)^2, data = babydata)
summary(lm_int_full)
autoplot(lm_int_full)
AIC(lm_int_full)

# Same assumptions tests as above
shapiro.test(lm_int_full$residuals)
ncvTest(lm_int_full)
durbinWatsonTest(lm_int_full)

# Looking at the high significance interactions and solo covariate
lm_int_1 <- lm(wt ~ gestation + ht + number + ded + ded:dwt + race:age  + gestation:parity, data = babydata)
summary(lm_int_1)
autoplot(lm_int_1)
AIC(lm_int_1)
shapiro.test(lm_int_1$residuals)
ncvTest(lm_int_1)
durbinWatsonTest(lm_int_1)

# Another possible model
lm_int_2 <- lm(wt ~ gestation + number + ded + ded:dwt + race:age + ht:ded + gestation:dage, data = babydata)
summary(lm_int_2)
autoplot(lm_int_2)
AIC(lm_int_2)
shapiro.test(lm_int_2$residuals)
ncvTest(lm_int_2)
durbinWatsonTest(lm_int_2)

# Out of the models we've considered, we prefer lm_int_2
final_model <- lm_int_2
summary(final_model)



