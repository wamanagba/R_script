library(ncdf4)
library(dplyr)
library(rio)
library(tidync)
rm(list = ls())

setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")

k=1981

for (k in 1981:1983) {
  Data_NC<-nc_open(paste("Data/CPC-UNIFIED/",k,".nc",sep=""))
  
  Data<-tidync(paste("Data/CPC-UNIFIED/",k,".nc",sep=""))%>%hyper_tibble(na.rm = F)
  Date=seq(as.Date(paste(k,"-01-01",sep="")),as.Date(paste(k,"-12-31",sep="")),by="days")
  X<-length(ncvar_get(Data_NC,"X"))
  Y<-length(ncvar_get(Data_NC,"Y"))
  
  Data$T<-rep(Date,X*Y)

 dir.create("Data/CPC-UNIFIED/CSV_Format/",recursive = T,showWarnings = F)
 rio::export(Data,paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))  
}


