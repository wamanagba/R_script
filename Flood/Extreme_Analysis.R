library(ncdf4)
library(dplyr)
library(rio)
rm(list = ls())

setwd("C:\\Users\\Yacou\\Desktop\\Flood_Risk")

k=1983
for (k in 1983:2021) {
  print(k)
  if(k==1983){
    Data<-rio::import(paste("CHIRPS/CSV_Format/",k,".csv",sep=""))
    Data$Year<-format(Data$T,"%Y")
    Data_95th<-Data%>%
      group_by(X,Y,Year)%>%
      summarise(Perc95=quantile(prcp,1,na.rm=T))
    
    Data<-merge(Data,Data_95th,by=c("X","Y","Year"))
    
  
  }else{
    Data1<-rio::import(paste("CHIRPS/CSV_Format/",k,".csv",sep=""))
    Data1$Year<-format(Data1$T,"%Y")
    Data_95th1<-Data1%>%
      group_by(X,Y,Year)%>%
      summarise(Perc95=quantile(prcp,1,na.rm=T))
    
  
   
    Data_95th<-rbind(Data_95th,Data_95th1)
  }
}
rio::export(Data_95th,"Data/Extreme_Perc_1983_2021.csv")

Perc95th<-Data_95th%>%
  group_by(X,Y)%>%
  summarise(Perc95=quantile(Perc95,0.95,na.rm=T))

rio::export(Perc95th,"Data/Extreme_95th_Perc_1983_2021_CHIRPS.csv")

