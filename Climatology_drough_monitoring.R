library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())

setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")

k=1981
dd=data.frame()
for (k in 1981:2010) {
  Data1<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
  
  Data1$Day<-as.numeric(format(Data1$T,"%d"))
  Data1$Month<-format(Data1$T,"%b")
  Data1=filter(Data1,Month %in% c("Jun", "Jul"))
  dd<-rbind( dd, Data1)

}
#DDD=dd


Data_Cum2<-dd%>%
  group_by(X,Y,Day,Month)%>%
  summarise(Cum=mean(rain,na.rm=T))

Data_Cum$Cum2=(Data_Cum$Cum/30)
rio::export(Data_Cum,paste("Products/Drought_Monitoring/Data/climatologyJun_Jul.csv"))

Data_Cum<-rbind( Data_Cum, Data_Cum1)

Annual_Total<-Data_Cum%>%
  group_by(X,Y)%>%
  summarise(Mean=mean(Cum,na.rm=T))

#rio::export(Annual_Total,"Data/Annual_Total_Mean_JFM.csv")

