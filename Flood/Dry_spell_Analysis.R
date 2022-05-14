library(ncdf4)
library(dplyr)
library(rio)
rm(list = ls())

setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")
#source("C:/Users/Yacou/Desktop/ACMAD_Git/Script/Dry_spell_Function.R")
source("C:/Users/Yacou/Desktop/ACMAD_Git/Script/Dry_spell_3.R")

k=1981
A=c(7,8.9)
for (k in 1981:2010) {
  print(k)
  if(k==1981){
    Data<-rio::import(paste("Data/CHIRPS/CSV_Format/",k,".csv",sep=""))
    Data$Year<-format(Data$T,"%Y")
    Data$Month=as.numeric(format(Data$T,"%m"))
    Data$prcp[is.na(Data$prcp)]=0
    Data= filter(Data,Month %in% A)
    
    Null= (sum(is.na(Data$prcp))/nrow(Data))*100
    
    Data_Dry_Speel<-Data%>%
      group_by(X,Y,Year)%>%
      summarise(Dry_Spell=CDD_function(prcp,2.5,Nb=5))
    
    Data<-merge(Data,Data_Dry_Speel,by=c("X","Y","Year"))   
    
  
  }else{
    Data1<-rio::import(paste("Data/CHIRPS/CSV_Format/",k,".csv",sep=""))
    Data1$Year<-format(Data1$T,"%Y")
    Data1$Month=as.numeric(format(Data1$T,"%m"))
    Data1$prcp[is.na(Data1$prcp)]=0
    Data1= filter(Data1,Month %in% A)
    
    Data_Dry_Speel1<-Data1%>%
      group_by(X,Y,Year)%>%
      summarise(Dry_Spell=CDD_function(prcp,2.5,Nb=10))
    
  
   
    Data_Dry_Speel<-rbind(Data_Dry_Speel,Data_Dry_Speel1)
  }
}


Data_Dry<-Data_Dry_Speel%>%
  group_by(X,Y,Year)%>%
  summarise(Mean=mean(Dry_Spell))




rio::export(Data_Dry,"Data/Dry_Spell_lengh_Max_1983_2010.csv")

#Perc95th<-Data_Dry_Speel%>%
#  group_by(X,Y)%>%
 # summarise(Perc95=quantile(Perc95,0.95,na.rm=T))

#rio::export(Perc95th,"Data/Extreme_95th_Perc_1983_2010_CHIRPS.csv")

