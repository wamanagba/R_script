library("rio")
library(dplyr)
rm(list = ls())
Data_Source="ARC2"
#Importation of station list
setwd('C:/Users/Yacou/Desktop/ACMAD_Git')
Stations<-rio::import("Synoptic_Station_All.csv")
Station<-filter(Stations,Country=="Burkina")
station= Station$Station[1]
CounTry='Burkina'
Data=rio::import(paste('Data/',Data_Source,"/",CounTry,'/',station,'.csv',sep=""))

A=c(7,8,9)
Data$Precipitation<-replace(Data$Precipitation,is.na(Data$Precipitation),0)

Data$year = as.numeric(format(Data$Date,'%Y'))
Data$Month=as.numeric(format(Data$Date,"%m"))
#Data1$prcp[is.na(Data1$prcp)]=0
Data= filter(Data,Month %in% A)
#Data_Station<-filter(Data,Station==station)
#Data_Station$year = as.numeric(format(Data_Station$Date,'%Y'))

All <- data.frame(Country = character(),
                  Station = character(),
                  #Lon=double(),
                  #Lat=double(),
                  #Date=character(),
                  #Precipitation=double(),
                  year=double(),
                  Max_spell=double(),
                 stringsAsFactors = FALSE)

Year<-seq(1983,2020,1)
k=2000
for(k in Year){
  Dat<-filter(Data,year==k)
  Dat$spell<-0
  spell=0
  for(i in 1:length(Dat$Precipitation)){
    if(Dat$Precipitation[i]<=2.5){
      spell=spell+1
      Dat$spell[i]=spell
    }
    else{
      spell=0
    }
  }
  sp<-Dat%>%
    group_by(Country,Station,year)%>%
    summarise(Max_Spell=max(spell))
              
  All<-do.call("rbind",list(All,sp))
}
DD<-All


