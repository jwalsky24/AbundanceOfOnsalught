#the codes reference :teacher's slides
#model 1 
set.seed(3245)  

# something to store lots of regression coefficients
bootResults <- array(dim = c(1000, 2))  


for(i in 1:1000){
  # resample our data with replacement
  bootData <-  babydata[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM<- lm(wt ~ gestation , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(gestation)',xlab = 'intercept')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(gestation)',xlab='slope')
# our best guesses of parameters
round(c(mean(bootResults[,1]), mean(bootResults[,2])),4)
# the CIs for these
round(rbind(quantile(bootResults[,1], probs = c(0.025, 0.975)),
      quantile(bootResults[,2], probs = c(0.025, 0.975))),4)

for(i in 1:1000){
  # resample our data with replacement
  bootData <- babydata[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ ht , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(ht)',xlab = 'intercept')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(ht)',xlab='slope')
# our best guesses of parameters
round(c(mean(bootResults[,1]), mean(bootResults[,2])),4)
# the CIs for these
round(rbind(quantile(bootResults[,1], probs = c(0.025, 0.975)),
      quantile(bootResults[,2], probs = c(0.025, 0.975))),4)

for(i in 1:1000){
  # resample our data with replacement
  bootData <- babydata[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ number , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(number)',xlab= 'intercept')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(number)',xlab='slope')
# our best guesses of parameters
round(c(mean(bootResults[,1]), mean(bootResults[,2])),4)
# the CIs for these
round(rbind(quantile(bootResults[,1], probs = c(0.025, 0.975)),
      quantile(bootResults[,2], probs = c(0.025, 0.975))),4)

for(i in 1:1000){
  # resample our data with replacement
  bootData <- babydata[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ ded , data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(ded)',xlab= 'intercept')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(ded)',xlab='slope')
# our best guesses of parameters
round(c(mean(bootResults[,1]), mean(bootResults[,2])),4)
# the CIs for these
round(rbind(quantile(bootResults[,1], probs = c(0.025, 0.975)),
      quantile(bootResults[,2], probs = c(0.025, 0.975))),4)

for(i in 1:1000){
  # resample our data with replacement
  bootData <- babydata[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ ded:dwt, data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(ded:dwt)',xlab= 'intercept')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(ded:dwt)',xlab='slope')
# our best guesses of parameters
round(c(mean(bootResults[,1]), mean(bootResults[,2])),4)
# the CIs for these
round(rbind(quantile(bootResults[,1], probs = c(0.025, 0.975)),
      quantile(bootResults[,2], probs = c(0.025, 0.975))),4)


for(i in 1:1000){
  # resample our data with replacement
  bootData <- babydata[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ race:age, data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(race:age)',xlab= 'intercept')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(race:age)',xlab='slope')
# our best guesses of parameters
round(c(mean(bootResults[,1]), mean(bootResults[,2])),4)
# the CIs for these
round(rbind(quantile(bootResults[,1], probs = c(0.025, 0.975)),
      quantile(bootResults[,2], probs = c(0.025, 0.975))),4)



for(i in 1:1000){
  # resample our data with replacement
  bootData <- babydata[sample(1:1000, 1000, replace = T),]
  
  # fit the model under this alternative reality
  bootLM <- lm(wt ~ gestation:parity, data = bootData)
  
  # store the coefs
  bootResults[i, ] <- coef(bootLM)
  
}
hist(bootResults[,1], col = "slateblue4", main = 'intercept distribution(gestation:parity)',xlab= 'intercept')
hist(bootResults[,2], col = "slateblue4", main = 'slope distribution(gestation:parity)',xlab='slope')
# our best guesses of parameters
round(c(mean(bootResults[,1]), mean(bootResults[,2])),4)
# the CIs for these
round(rbind(quantile(bootResults[,1], probs = c(0.025, 0.975)),
      quantile(bootResults[,2], probs = c(0.025, 0.975))),4)

