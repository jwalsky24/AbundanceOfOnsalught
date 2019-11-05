#MT5762- BabyWeights
#Code to look at extreme values of gestation and weight

#Extreme long and short gestation periods, generates a vector of indexes corresponding
#to the babies with the extreme gestation periods
extreme_long <- NULL
extreme_short <- NULL
for(i in 1:length(babydata$gestation)){
  if(!is.na(babydata$gestation[i])){
    if(babydata$gestation[i] < 230){
      extreme_short <- c(extreme_short, i)
    }
    else if(babydata$gestation[i] > 310){
      extreme_long <- c(extreme_long, i)
    }
  }
}

#Extreme thin and fat babies, generates a vector of indexes corresponding
#to the babies with the extreme weights
extreme_thin <- NULL
extreme_fat <- NULL
for(i in 1:length(babydata$wt)){
  if(!is.na(babydata$wt[i])){
    if(babydata$wt[i] < 2000){
      extreme_thin <- c(extreme_thin, i)
    }
    else if(babydata$wt[i] > 4500){
      extreme_fat <- c(extreme_fat, i)
    }
  }
}
