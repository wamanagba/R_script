

rm(list = ls())
library(rio)
library(dplyr)
library(ggplot2)
library(rio)
library(dplyr)
library(ncdf4)
library(RColorBrewer)
library(metR)
library(ggplot2)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(raster)
library(sp)
library(ggdark)
library(showtext)
library(ggrepel)
library(Hmisc)
#http://154.66.220.45:8080/thredds/fileServer/ACMAD/CDD/climatedataservice/Synoptic_Daily_CPC_Unified_Data/Niger/ZINDER.csv
setwd("C:/Users/Yacou/Desktop/ACMAD_Git")
#Importation of station list
Station<-rio::import("Synoptic_Station_All.csv")

Station<-filter(Station,Country %in% c("Burkina") & Station=="GAOUA")
source("C:/Users/Yacou/Desktop/ACMAD_Git/Script/Dry_spell_Function.R")

Data_Source="ARC2"

data <- data.frame(Lon = double(),
                 Lat = double(),
                 Country=character(),
                 Station=character(),
                 Year = double(),
                 Dry_Spell=double(),
                 stringsAsFactors = FALSE)
i=1
k=2000
for (k in 1983:2021) {
  print(k)
  for (i in 1:length(Station$Station)) {
    print(i);print(k);print(Station$Station[i]);print(Station$Country[i])
    station= Station$Station[i]
    CounTry=Station$Country[i]
    
    
    Data=rio::import(paste('Data/',Data_Source,"/",CounTry,'/',station,'.csv',sep=""))
    Data$Precipitation[is.na(Data$Precipitation)]=0
    Data$Year<-format(Data$Date,"%Y")
    Data$Month=as.numeric(format(Data$Date,"%m"))
    Data= filter(Data,Year==k & Month %in% c(6,7,8))
    
    if(nrow(Data)==0){
      print(paste("Station",station, "had 0 ROW"))
    }
    else{
      
      
      Dry_spell<-Data%>%
        group_by(Lon,Lat,Country,Station, Year)%>%
        summarise(Dry_Spell=find_dryspells(x=Precipitation,dates=Date,2.5,nb=5))
      
      data<-rbind(data,Dry_spell)}
  }
  
  }



rio::export(data,"Data/Dry_spell_Africa.csv")



d1=filter(data,Station=="OUAGADOUGOU")
x11();hist(d1$Dry_Spell)

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
Data=data

MinLon=6
MaxLon=9
MinLat=8
MaxLat=12


Raster_file<-rasterFromXYZ(Data[c("Lon","Lat","Dry_Spell")])


Raster_file_1=disaggregate(Raster_file,20,method='bilinear')

rr<-raster::mask(Raster_file_1,Africa)

Data <- as.data.frame(rasterToPoints(rr ))

mybreaks <- c(0,20,40,60,80,100,120,Inf)

#Function to return the desired number of colors










