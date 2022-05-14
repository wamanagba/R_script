################################################################################
#What does the script do
#
#
#ACMAD/Copyright
################################################################################
rm(list = ls())
library(rio)
library(dplyr)
library(ggplot2)
#http://154.66.220.45:8080/thredds/fileServer/ACMAD/CDD/climatedataservice/Synoptic_Daily_CPC_Unified_Data/Niger/ZINDER.csv
setwd("~/Bureau/ACMAD_Git/")
#Importation of station list
Station<-rio::import("Synoptic_Station_All.csv")
 
Station<-filter(Station,Country %in% c("Burkina","Niger"))
Data_Source="ARC2"

<<<<<<< HEAD
Station<-filter(Station,Country=="Burkina" )

i=1
=======
>>>>>>> 098d00f9ac55258a3a88397031c7d8c0e43a21ef
for (i in 1:length(Station$Station)) {
  print(i)
  
  List<-gsub(".csv","",list.files(path =paste("Data/",Data_Source,"/",Station$Country[i],"/",sep=""),".csv"))
  
  if(Station$Station[i] %in% List){
    
    print("Yet Downloaded!!!!")
    
  }else{
  
  dir.create(paste("Data/",Data_Source,"/",Station$Country[i],sep=""),recursive = T,showWarnings = F)
  
<<<<<<< HEAD
  download.file(url = paste("http://154.66.220.45:8080/thredds/fileServer/ACMAD/CDD/climatedataservice/Synoptic_Daily_CPC_Unified_Data/",Station$Country[i],"/",Station$Station[i],".csv",sep=""),destfile = paste("Data/",Station$Country[i],"/",Station$Station[i],".csv",sep=""),mode = "wb")
=======
  download.file(url = paste("http://154.66.220.45:8080/thredds/fileServer/ACMAD/CDD/climatedataservice/Synoptic_Daily_ARC2_Data/",Station$Country[i],"/",Station$Station[i],".csv",sep=""),destfile = paste("Data/",Data_Source,"/",Station$Country[i],"/",Station$Station[i],".csv",sep=""))
>>>>>>> 098d00f9ac55258a3a88397031c7d8c0e43a21ef
  }
}


<<<<<<< HEAD
    
=======




################################################################################
#
#  Number Rain day
#
###############################################################################
i=1
for (i in 1:length(Station$Station)) {
  station= Station$Station[i]
  CounTry=Station$Country[i]
  data=rio::import(paste('Data/',Data_Source,"/",CounTry,'/',station,'.csv',sep=""))
  data$Precipitation= ifelse(data$Precipitation<2.5,0,1)
  data$Year = as.numeric(format(data$Date,'%Y'))
  
  # Calculation of Number Rain day for each station
  Data_numberDay = data%>%
    group_by(Country,Station,Year) %>%
    summarise(Number_Rain_day = sum(Precipitation,na.rm = T))
  
  #Compute the Mean
  
  NumberDay_Mean<-filter(Data_numberDay,Year<2022)%>%
    group_by(Station)%>%
    summarise(Mean=mean(Number_Rain_day))
  
  #Merge two Data Frame
  
  Data_numberDay<-merge(Data_numberDay,NumberDay_Mean,by=c("Station"))
  
  #Anomaly=Number_Rain_day-Mean ()
  
  Data_numberDay$Anomaly<-Data_numberDay$Number_Rain_day- Data_numberDay$Mean
  
  Data_numberDay$AnoPosit<-ifelse(Data_numberDay$Anomaly<0,0,Data_numberDay$Anomaly)
  
  Data_numberDay$AnoNeg<-ifelse(Data_numberDay$Anomaly>0,0,Data_numberDay$Anomaly)
  
  Data_numberDay<-filter(Data_numberDay,Year<2022)
  
  
  
  
  # Create a folder each station
  dir.create(paste('Data/',Data_Source,"/",CounTry,'/Number_Rain_day',sep = ""),recursive = T, showWarnings = F)
  
  rio::export(Data_numberDay,paste('Data/',Data_Source,"/",CounTry,'/Number_Rain_day/',station,'.csv',sep = ""))
  
  
  last_graph <- ggplot2::ggplot(data=Data_numberDay, mapping=ggplot2::aes(x=Year, y=Anomaly)) + ggplot2::geom_bar(position="dodge", stat="identity", fill="red") + ggplot2::geom_bar(mapping=ggplot2::aes(y=AnoPosit), fill="blue", stat="identity", position="dodge") + ggplot2::geom_smooth(mapping=ggplot2::aes(y=Anomaly), method="loess", formula="y ~ x", colour="black", se=FALSE) + theme_grey() + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, size=20, colour="black"), axis.title=ggplot2::element_text(face="bold"), axis.text=ggplot2::element_text(face="bold"), axis.title.x=ggplot2::element_text(size=15.0, face="bold"), axis.title.y=ggplot2::element_text(size=15.0), plot.title=ggplot2::element_text(size=20.0, face="bold"), axis.text.y=ggplot2::element_text(size=20, face="bold", colour="black")) + ggplot2::labs(title=paste("Anomaly of Annual Number of rainy days for ",station,sep = ""), subtitle="", caption="") + ggplot2::xlab(label="Years") + ggplot2::scale_x_continuous(breaks=seq(by=1, to=2021, from=1983)) + ggplot2::ylab(label="Number of rainy days") + ggplot2::scale_y_continuous(breaks=seq(by=5, to=50, from=-50))
  
  # Create a folder to save a figures
  
  dir.create(paste('Products/',Data_Source,"/",CounTry,'/Number_Rain_day',sep = ""),recursive = T, showWarnings = F)
  
  ggplot2::ggsave(filename=paste('Products/',Data_Source,"/",CounTry,'/Number_Rain_day/',station,'_Anomaly.jpeg',sep = ""), width=14,height=8,limitsize = FALSE ,plot=last_graph)
  
}


## #############################################################################
## 
##  Cumulative precipitation
##
################################################################################
i=1
for (i in 1:length(Station$Station)) {
  station= Station$Station[i]
  CounTry= Station$Country[i]
  data_S=rio::import(paste('Data/',Data_Source,"/",CounTry,'/',station,'.csv',sep=""))
  #data_$Precipitation= ifelse(data_$Precipitation<2.5,0,1)
  data_S$Year = as.numeric(format(data_S$Date,'%Y'))
  
  # Calculation of annual precipitation sum for each station
  Sum_Precipitation = data_S%>%
    group_by(Country,Station,Year) %>%
    summarise(Total_Precipitation = sum(Precipitation,na.rm = T))
  
  #Compute the Mean
  
  Precipitation_Mean<-filter(Sum_Precipitation,Year<2022)%>%
    group_by(Station)%>%
    summarise(Mean=mean(Total_Precipitation))
  
  #Merge two Data Frame
  
  Sum_Precipitation<-merge(Sum_Precipitation,Precipitation_Mean,by=c("Station"))
  
  #Anomaly=Number_Rain_day-Mean ()
  
  Sum_Precipitation$Anomaly<-Sum_Precipitation$Total_Precipitation- Sum_Precipitation$Mean
  
  Sum_Precipitation$AnoPosit<-ifelse(Sum_Precipitation$Anomaly<0,0,Sum_Precipitation$Anomaly)
  
  Sum_Precipitation$AnoNeg<-ifelse(Sum_Precipitation$Anomaly>0,0,Sum_Precipitation$Anomaly)
  
  Sum_Precipitation<-filter(Sum_Precipitation,Year<2022)
  
  
  # Create a folder for the  data
  dir.create(paste('Data/',Data_Source,"/",CounTry,'/Sum_Precipitation',sep = ""),recursive = T, showWarnings = F)
  rio::export(Sum_Precipitation,paste('Data/',Data_Source,"/",CounTry,'/Sum_Precipitation/',station,'.csv',sep = ""))
  
  
  
  
  last_graph <- ggplot2::ggplot(data=Sum_Precipitation, mapping=ggplot2::aes(x=Year, y=Anomaly)) + ggplot2::geom_bar(position="dodge", stat="identity", fill="red") + ggplot2::geom_bar(mapping=ggplot2::aes(y=AnoPosit), fill="blue", stat="identity", position="dodge") + ggplot2::geom_smooth(mapping=ggplot2::aes(y=Anomaly), method="loess", formula="y ~ x", colour="black", se=FALSE) + theme_grey() + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, size=15, colour="black"), axis.title=ggplot2::element_text(face="bold"), axis.text=ggplot2::element_text(face="bold"), axis.title.x=ggplot2::element_text(size=9.0, face="bold"), axis.title.y=ggplot2::element_text(size=9.0), plot.title=ggplot2::element_text(size=13.0, face="bold"), axis.text.y=ggplot2::element_text(size=13, face="bold", colour="black")) + ggplot2::labs(title=paste("Anomaly of Annual Total Rainfall for ",station), subtitle="", caption="") + ggplot2::xlab(label="Years") + ggplot2::scale_x_continuous(breaks=seq(by=1, to=2021, from=1983)) + ggplot2::ylab(label="Rainfall Anomaly (mm)") + ggplot2::scale_y_continuous(breaks=seq(by=40, to=800, from=-800))
  
  # Create a folder to save a figures
  
  dir.create(paste('Products/',Data_Source,"/",CounTry,'/Total_Precipitations',sep = ""),recursive = T, showWarnings = F)
  ggplot2::ggsave(filename=paste('Products/',Data_Source,"/",CounTry,'/Total_Precipitations/',station,'_Anomaly.jpeg',sep = ""), width=14,height=8,limitsize = FALSE ,plot=last_graph)
  
}

###############################################################################
##                                                                          ###  
##                 Extrem precipitation by year                             ###
##                                                                          ###
###############################################################################
#rm(list=ls())


for (i in 1:length(Station$Station)) {
  station= Station$Station[i]
  CounTry=Station$Country[i]
  data=rio::import(paste('Data/',Data_Source,"/",CounTry,'/',station,'.csv',sep=""))
  #data_$Precipitation= ifelse(data_$Precipitation<2.5,0,1)
  data$Year = as.numeric(format(data$Date,'%Y'))
  
  # Calculation of annual precipitation sum for each station
  data_Extrem_Precipitation = data%>%
    group_by(Country,Station,Year) %>%
    summarise(Extreme_Precipitation = max(Precipitation,na.rm = T))
  
  
  #Compute the Mean
  
  Extrem_Precipitation_mean<-filter(data_Extrem_Precipitation,Year<2022)%>%
    group_by(Station)%>%
    summarise(Mean=mean(Extreme_Precipitation))
  
  #Merge two Data Frame
  
  data_Extrem_Precipitation<-merge(data_Extrem_Precipitation,Extrem_Precipitation_mean,by=c("Station"))
  
  #Anomaly=Number_Rain_day-Mean ()
  
  data_Extrem_Precipitation$Anomaly<-data_Extrem_Precipitation$Extreme_Precipitation- data_Extrem_Precipitation$Mean
  
  data_Extrem_Precipitation$AnoPosit<-ifelse(data_Extrem_Precipitation$Anomaly<0,0,data_Extrem_Precipitation$Anomaly)
  
  data_Extrem_Precipitation$AnoNeg<-ifelse(data_Extrem_Precipitation$Anomaly>0,0,data_Extrem_Precipitation$Anomaly)
  
  data_Extrem_Precipitation<-filter(data_Extrem_Precipitation,Year<2022)
  
  
  # Create a folder to save the data
  dir.create(paste('Data/',Data_Source,"/",CounTry,'/Extrem_Precipitation',sep = ""),recursive = T, showWarnings = F)
  rio::export(data_Extrem_Precipitation,paste('Data/',Data_Source,"/",CounTry,'/Extrem_Precipitation/',station,'.csv',sep = ""))

  last_graph <- ggplot2::ggplot(data=data_Extrem_Precipitation, mapping=ggplot2::aes(x=Year, y=Anomaly)) + ggplot2::geom_bar(position="dodge", stat="identity", fill="red") + ggplot2::geom_bar(mapping=ggplot2::aes(y=AnoPosit), fill="blue", stat="identity", position="dodge") + ggplot2::geom_smooth(mapping=ggplot2::aes(y=Anomaly), method="loess", formula="y ~ x", colour="black", se=FALSE) + theme_grey() + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, size=15, colour="black"), axis.title=ggplot2::element_text(face="bold"), axis.text=ggplot2::element_text(face="bold"), axis.title.x=ggplot2::element_text(size=9.0, face="bold"), axis.title.y=ggplot2::element_text(size=9.0), plot.title=ggplot2::element_text(size=13.0, face="bold"), axis.text.y=ggplot2::element_text(size=13, face="bold", colour="black")) + ggplot2::labs(title=paste("Anomaly of Annual Extrem Precipitation for ",station), subtitle="", caption="") + ggplot2::xlab(label="Years") + ggplot2::scale_x_continuous(breaks=seq(by=1, to=2021, from=1983)) + ggplot2::ylab(label="Extrem Precipitation Anomaly (mm)") + ggplot2::scale_y_continuous(breaks=seq(by=5, to=100, from=-100))
  
  # Create a folder to save a figures
  
  dir.create(paste('Products/',Data_Source,"/",CounTry,'/Extrem_Precipitations',sep = ""),recursive = T, showWarnings = F)
  ggplot2::ggsave(filename=paste('Products/',Data_Source,"/",CounTry,'/Extrem_Precipitations/',station,'_Anomaly.jpeg',sep = ""), width=14,height=8,limitsize = FALSE ,plot=last_graph)
  
  
  }
>>>>>>> 098d00f9ac55258a3a88397031c7d8c0e43a21ef

