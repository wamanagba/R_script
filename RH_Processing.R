library(rio)
library(ncdf4)
library(dplyr)

setwd("C:/Users/Yacou/Desktop/ACMAD_Git")

#Take Data
Year=2022
Month="Apr"
First_Day=01
Last_Day=10
P=700
download.file(paste("http://iridl.ldeo.columbia.edu/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.rhum/T/(",First_Day,"%20",Month,"%20",Year,")/(",Last_Day,"%20",Month,"%20",Year,")/RANGEEDGES/X/-25/0.2/60/GRID/Y/-40/0.2/40/GRID/P/(",P,")/VALUES/%5BT%5Daverage/data.nc",sep=""),mode="wb",paste("Data/RH_",Month,"_",Year,"_",First_Day,"_",Last_Day,"_",P,".nc",sep=""))


Data<-ncdf4::nc_open(paste("Data/RH_",Month,"_",Year,"_",First_Day,"_",Last_Day,"_",P,".nc",sep=""))
#print(Data)
Rh<-ncvar_get(Data,"rhum")

X<-ncvar_get(Data,"X")
X[1]
Y<-ncvar_get(Data,"Y")
nc_close(Data)

for (i in 1:length(X)) {
     if(i==1){
     Data<-data.frame(Longitude=X[i],Latitude=Y, RH=Rh[i,])
     }else{
      Data1<-data.frame(Longitude=X[i],Latitude=Y, RH=Rh[i,])
      Data<-rbind(Data,Data1)
     }
}

rio::export(Data,paste("Data/RH_",Month,"_",Year,"_",First_Day,"_",Last_Day,"_",P,".csv",sep=""))

