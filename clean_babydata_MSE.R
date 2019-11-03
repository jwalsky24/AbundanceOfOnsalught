babydata$pluralty<-NULL
babydata$outcome<-NULL
babydata$sex<-NULL
names(babydata)[10]<-"mwt"
for (i in babydata$pluralty){
  if (i != 5){
    print("Multiple fetus!")
  }
}
for (i in babydata$outcome){
  if (i != 1){
    print("Dead baby!")
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$gestation[i]==999){
    babydata$gestation[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$wt[i]==999){
    babydata$wt[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$mwt[i]==999){
    babydata$mwt[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$dwt[i]==999){
    babydata$dwt[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$parity[i]==99){
    babydata$parity[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$race[i]==99){
    babydata$race[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$age[i]==99){
    babydata$age[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$ht[i]==99){
    babydata$ht[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$drace[i]==99){
    babydata$drace[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$dage[i]==99){
    babydata$dage[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$dht[i]==99){
    babydata$dht[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$inc[i]==99){
    babydata$inc[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$inc[i]==98){
    babydata$inc[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$time[i]==99){
    babydata$time[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$time[i]==98){
    babydata$time[i]=NA
  }
}
babydata$time[921]=NA
rm(i)
for (i in 1:1236){ 
  if(babydata$number[i]==99){
    babydata$number[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$number[i]==98){
    babydata$number[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$sex[i]==9){
    babydata$sex[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$ed[i]==9){
    babydata$ed[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$smoke[i]==9){
    babydata$smoke[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(babydata$ded[i]==9){
    babydata$ded[i]=NA
  }
}
rm(i)
for (i in 1:1236){ 
  if(is.na(babydata$wt[i])==FALSE){
    wt_g = babydata$wt[i]*28.34952
    babydata$wt[i]= wt_g
  }
}
rm(i)
for (i in 1:1236){ 
  if(is.na(babydata$mwt[i])==FALSE){
    mwt_g = babydata$mwt[i]*0.45359237
    babydata$mwt[i]= mwt_g
  }
}
rm(i)
for (i in 1:1236){ 
  if(is.na(babydata$dwt[i])==FALSE){
    dwt_g = babydata$dwt[i]*0.45359237
    babydata$dwt[i]= dwt_g
  }
}
rm(i)
for (i in 1:1236){ 
  if(is.na(babydata$ht[i])==FALSE){
    ht_cm = babydata$ht[i]*2.54
    babydata$ht[i]= ht_cm
  }
}
rm(i)
for (i in 1:1236){ 
  if(is.na(babydata$dht[i])==FALSE){
    dht_cm = babydata$dht[i]*2.54
    babydata$dht[i]= dht_cm
  }
}
babydata$ed[17]=6
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
