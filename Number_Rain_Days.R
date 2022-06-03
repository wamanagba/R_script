################################################################################
#
#  Number Rain day
#
################################################################################
library(rio)
library(dplyr)

setwd('C:/Users/Yacou/Desktop/ACMAD_Git')
Stations<-rio::import("Synoptic_Station_All.csv")

# Importing the Niger stations
CounTry= 'Burkina'
Niger_station= Station<-filter(Stations,Country==CounTry)

i=1
for (i in 1:length(Niger_station$Station)) {
  station= Niger_station$Station[i]
  data=rio::import(paste('Data/ARC2/',CounTry,'/',station,'.csv',sep=""))
  data$Precipitation= ifelse(data$Precipitation<2.5,0,1)
  data$Year = as.numeric(format(data$Date,'%Y'))
  
  
  # Calculation of annual precipitation sum for each station
  Data_numberDay = data%>%
    group_by(Country,Station,Year) %>%
    summarise(sum_Precipitation = sum(Precipitation,na.rm = T))
  
  # Create a folder to put the
  dir.create(paste('Data/',CounTry,'/sum_Precipitation',sep = ""),recursive = T, showWarnings = F)
  rio::export(Data_numberDay,paste('Data/',CounTry,'/sum_Precipitation/Data_numberDay_',station,'.csv',sep = ""))
  
}


## #############################################################################
## 
##  Cumulative precipitation
##
################################################################################
i=1
for (i in 1:length(Niger_station$Station)) {
  station= Niger_station$Station[i]
  data_S=rio::import(paste('data/',CounTry,'/',station,'.csv',sep=""))
  #data_$Precipitation= ifelse(data_$Precipitation<2.5,0,1)
  data_S$Year = as.numeric(format(data_S$Date,'%Y'))
  
  # Calculation of annual precipitation sum for each station
  data_S_sum_Precipitation = data_S%>%
    group_by(Country,Station,Year) %>%
    summarise(sum_Precipitation = sum(Precipitation,na.rm = T))
  
  # Create a folder to put the
  dir.create(paste('Data/',CounTry,'/sum_Precipitation',sep = ""),recursive = T, showWarnings = F)
  rio::export(data_S_sum_Precipitation,paste('Data/',CounTry,'/sum_Precipitation/data_S_numberDay_',station,'.csv',sep = ""))
  
}

########################## Extrem precipitation by year ##############

#rm(list=ls())


for (i in 1:length(Niger_station$Station)) {
  station= Niger_station$Station[i]
  data=rio::import(paste('data/',CounTry,'/',station,'.csv',sep=""))
  #data_$Precipitation= ifelse(data_$Precipitation<2.5,0,1)
  data$Year = as.numeric(format(data$Date,'%Y'))
  
  # Calculation of annual precipitation sum for each station
  data_Extrem_Precipitation = data%>%
    group_by(Country,Station,Year) %>%
    summarise(Extreme_Precipitation = max(Precipitation,na.rm = T))
  
  # Create a folder to put the
  dir.create(paste('Data/',CounTry,'/Extrem_Precipitation',sep = ""),recursive = T, showWarnings = F)
  rio::export(data_Extrem_Precipitation,paste('Data/',CounTry,'/Extrem_Precipitation/data_S_numberDay_',station,'.csv',sep = ""))
}