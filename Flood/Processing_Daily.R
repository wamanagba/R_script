library(ncdf4)
library(dplyr)
library(rio)
library(tidync)
rm(list = ls())

setwd("C:\\Users\\Yacou\\Desktop\\Flood_Risk")

k=1983

for (k in 1983:2021) {
  Data_NC<-nc_open(paste("CHIRPS/",k,".nc",sep=""))
  
  Data<-tidync(paste("CHIRPS/",k,".nc",sep=""))%>%hyper_tibble(na.rm = F)
  Date=seq(as.Date(paste(k,"-01-01",sep="")),as.Date(paste(k,"-12-31",sep="")),by="days")
  X<-length(ncvar_get(Data_NC,"X"))
  Y<-length(ncvar_get(Data_NC,"Y"))
  
  Data$T<-rep(Date,X*Y)

 dir.create("CHIRPS/CSV_Format/",recursive = T,showWarnings = F)
 rio::export(Data,paste("CHIRPS/CSV_Format/",k,".csv",sep=""))  
}


