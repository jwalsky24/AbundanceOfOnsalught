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
babydata$pluralty<-NULL
babydata$outcome<-NULL
babydata$sex<-NULL
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