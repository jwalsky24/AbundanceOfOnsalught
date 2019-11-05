#Code to clean the given dataset

#Read in the data
babydata <- read.table("babies23.data", header = T)
str(babydata)

#Check the columns pluralty, outcome and sex for outliers
for (i in babydata$pluralty){
  if (i != 5){
    print("Multiple fetus!")
  }
}
rm(i)
for (i in babydata$outcome){
  if (i != 1){
    print("Dead baby!")
  }
}
rm(i)
for (i in babydata$sex){
  if (i != 1){
    print("It's a girl!")
  }
}
rm(i)

#Delete the columns pluralty, outcome and sex, because all babies are singular feti, alive and male
babydata$pluralty<-NULL
babydata$outcome<-NULL
babydata$sex<-NULL

#Delte the column id, because it is not relevant for our analysis
babydata$id<-NULL

#Change the name of the column of mothers' weights, else we have two columns with the same name
names(babydata)[10]<-"mwt"

#Set the placeholders to NA when information is unknown or not asked 
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
babydata$time[921]=NA
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

#Change ounces, pounds and inches to gramms, kgs and cms
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

#Merge levels 6 and 7 for educational level of mother and educational level of father
babydata$ed[17] <- 6
babydata$ed[51] <- 6
babydata$ed[153] <- 6
babydata$ed[378] <- 6
babydata$ed[705] <- 6
babydata$ed[902] <- 6
babydata$ed[1004] <- 6
babydata$ded[905] <- 6
babydata$ded[477] <- 6
babydata$ded[517] <- 6
babydata$ded[403] <- 6
babydata$ded[157] <- 6
rm(dht_cm, dwt_g, ht_cm, i, mwt_g, wt_g)

# Keep copy of original data
babydata_orig <- babydata
# Remove NAs prior to linear regression
babydata <- na.omit(babydata)
# examine data one last time
str(babydata)

