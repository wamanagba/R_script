
library(rio)
library(ncdf4)
library(dplyr)
library(raster)

###########   RH_level_700 et RH_level_850: Dekadal Values  #######


setwd("C:/Users/Yacou/Desktop/ACMAD_Git")

#Take Data
Year=2022
Month="Apr"
First_Day=01
Last_Day=10
P=700



download.file(paste("http://iridl.ldeo.columbia.edu/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.rhum/T/(",First_Day,"%20",Month,"%20",Year,")/(",Last_Day,"%20",Month,"%20",Year,")/RANGEEDGES/X/-25/0.2/60/GRID/Y/-40/0.2/40/GRID/P/(",P,")/VALUES/%5BT%5Daverage/data.nc",sep=""),mode="wb",paste("Data/RH_",Month,"_",Year,"_",First_Day,"_",Last_Day,"_",P,".nc",sep=""))

#http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.ARC2/.daily/.est_prcp/X/-20/0.5/70/GRID/Y/-40/0.5/40/GRID/T/(%20%2011-20%20Jul%20%201983%20)/(%20%2011-20%20Jul%20%201984%20)/(%20%2011-20%20Jul%20%201985%20)/(%20%2011-20%20Jul%20%201986%20)/(%20%2011-20%20Jul%20%201987%20)/(%20%2011-20%20Jul%20%201988%20)/(%20%2011-20%20Jul%20%201989%20)/(%20%2011-20%20Jul%20%201990%20)/(%20%2011-20%20Jul%20%201991%20)/(%20%2011-20%20Jul%20%201992%20)/(%20%2011-20%20Jul%20%201993%20)/(%20%2011-20%20Jul%20%201994%20)/(%20%2011-20%20Jul%20%201995%20)/(%20%2011-20%20Jul%20%201996%20)/(%20%2011-20%20Jul%20%201997%20)/(%20%2011-20%20Jul%20%201998)/(%20%2011-20%20Jul%20%201999%20)/(%20%2011-20%20Jul%20%202000%20)/(%20%2011-20%20Jul%20%202001%20)/(%20%2011-20%20Jul%20%202002%20)/(%20%2011-20%20Jul%20%202003%20)/(%20%2011-20%20Jul%20%202004%20)/(%20%2011-20%20Jul%20%202005%20)/(%20%2011-20%20Jul%20%202006)/(%20%2011-20%20Jul%20%202007%20)/(%20%2011-20%20Jul%20%202008%20)/(%20%2011-20%20Jul%20%202009%20)/(%20%2011-20%20Jul%20%202010%20)/(%20%2011-20%20Jul%20%202011%20)/(%20%2011-20%20Jul%20%202012%20)/(%20%2011-20%20Jul%20%202013%20)/(%20%2011-20%20Jul%20%202014%20)/(%20%2011-20%20Jul%20%202015%20)/(%20%2011-20%20Jul%20%202016%20)/(%20%2011-20%20Jul%20%202017)/(%20%2011-20%20Jul%20%202018)/(%20%2011-20%20Jul%20%202019)/(%20%2011-20%20Jul%20%202020)/(%20%2011-20%20Jul%20%202021)/RANGE/%5BT%5Daverage/10/mul/data.nc























Download_RH= function(Year,Month,First_Day,Last_Day,P){

download.file(paste("http://iridl.ldeo.columbia.edu/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.rhum/T/(",First_Day,"%20",Month,"%20",Year,")/(",Last_Day,"%20",Month,"%20",Year,")/RANGEEDGES/X/-25/0.2/60/GRID/Y/-40/0.2/40/GRID/P/(",P,")/VALUES/%5BT%5Daverage/data.nc",sep=""),mode="wb",paste("Data/RH_",Month,"_",Year,"_",First_Day,"_",Last_Day,"_",P,".nc",sep=""))


Data<-ncdf4::nc_open(paste("Data/RH_",Month,"_",Year,"_",First_Day,"_",Last_Day,"_",P,".nc",sep=""))
print(Data)
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
}


Download_RH(Year=2022,
         Month="Apr",
         First_Day=01,
         Last_Day=10,
         P=850)







Year1=1991
Year2=2020
Month="Apr"
First_Day=01
Last_Day=10
P=850
download.file(paste("https://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.rhum/dekadalAverage/T/(1%20Jan%202002)/(31%20Dec%202002)/RANGE/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.rhum/dekadalAverage/T/(01%20Apr%201991)/(10%20Apr%202020)/RANGE/T/name/npts/NewIntegerGRID/replaceGRID/T/36/splitstreamgrid/%5BT2%5Daverage/T/2/index/T/dekadaledgesgrid/partialgrid/2/1/roll/pop/replaceGRID/T/(days%20since%202002-01-01)/streamgridunitconvert/T/T/dekadaledgesgrid/first/secondtolast/subgrid//calendar//365/def/gridS/365/store/modulus/pop/periodic/setgridtype/partialgrid/replaceGRID/%5BX/Y%5DREORDER/2/1/roll/pop//fullname/(rhum%202002-2011%20clim)/def//long_name/(rhum%202002-2011%20clim)/def/X/-25/0.2/60/GRID/Y/-40/0.2/40/GRID/P/(850)/VALUES/data.nc",sep = ""),mode = "wb",paste("Data/RH_Clim",Month,"_",Year1,"_",First_Day,"_",Year2,"_",Last_Day,"_",P,".nc",sep=""))

Data<-ncdf4::nc_open(paste("Data/RH_Clim",Month,"_",Year1,"_",First_Day,"_",Year2,"_",Last_Day,"_",P,".nc",sep=""))
#print(Data)         paste("Data/RH_Clim",Month,"_",Year1,"_",First_Day,"_",Year2,"_",Last_Day,"_",P,".nc",sep="")
#nc.brick <- raster::brick('C:\\Users\\Yacou\\Desktop\\ACMAD_Git\\Data\\RH_ClimApr_1991_1_2020_10_700.nc')
#nc.df <- as.data.frame(nc.brick[[2]], xy=T)

Rh<-ncvar_get(Data,"rhum")

X<-ncvar_get(Data,"X")

Y<-ncvar_get(Data,"Y")


nc_close(Data)
Rh[1,1,]
for (i in 1:length(X)) {
  for (j in 1:length(Y)) {

  if(i==1){
    Data<-data.frame(Longitude=X[i],Latitude=Y[j],Year=seq(1991,), RH=Rh[i,])
  }else{
    Data1<-data.frame(Longitude=X[i],Latitude=Y, RH=Rh[i,])
    Data<-rbind(Data,Data1)
  }
  }
}

rio::export(Data,paste("Data/RH_clim",Month,"_",Year1,"_",First_Day,"_",Year2,"_",Last_Day,"_",P,".csv",sep=""))
