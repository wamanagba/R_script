library(ncdf4)
library(dplyr)
library(rio)
library(tidync)
rm(list = ls())

setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")

k=1981

for (k in 1981:2021) {
  Data_NC<-nc_open(paste("Data/CHIRPS/",k,".nc",sep=""))
  
  Data<-tidync(paste("Data/CHIRPS/",k,".nc",sep=""))%>%hyper_tibble(na.rm = F)
  Date=seq(as.Date(paste(k,"-01-01",sep="")),as.Date(paste(k,"-12-31",sep="")),by="days")
  X<-length(ncvar_get(Data_NC,"X"))
  Y<-length(ncvar_get(Data_NC,"Y"))
  
  Data$T<-rep(Date,X*Y)

 dir.create("Data/CHIRPS/CSV_Format/",recursive = T,showWarnings = F)
 rio::export(Data,paste("Data/CHIRPS/CSV_Format/",k,".csv",sep=""))  
}


