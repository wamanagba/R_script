library(ncdf4)
library(dplyr)
library(rio)
library(tidync)
rm(list = ls())

setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")

k=2022

for (k in 1981:2010) {
  Data_NC<-nc_open(paste("CPC-UNIFIED-2022/",k,".nc",sep=""))
  
  Data<-tidync(paste("CPC-UNIFIED-2022/",k,".nc",sep=""))%>%hyper_tibble(na.rm = F)
  Date=seq(as.Date(paste(k,"-01-01",sep="")),as.Date(paste(k,"-6-4",sep="")),by="days")
  X<-length(ncvar_get(Data_NC,"X"))
  Y<-length(ncvar_get(Data_NC,"Y"))
  Date_All=sort(rep(Date,X*Y),decreasing = F)
  Data$T<-Date_All

 dir.create("Data/CPC-UNIFIED/CSV_Format/",recursive = T,showWarnings = F)
 rio::export(Data,paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))  
}


