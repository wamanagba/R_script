



library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())
setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")
Data_Source="CPC-UNIFIED"

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40


k=2000
cpt=7
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
  
  
  for (cpt in 1:12) {
    
  cpt1=(cpt+1)%%12
  if(cpt1==0) cpt1=12
  cpt2=(cpt+2)%%12
  if(cpt2==0) cpt2=12
  season=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")
  

  dd=data.frame()
  
  
  
    
  Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
  Data$Month=format(Data$T,"%b")
  Data$Year=format(Data$T,"%Y")
  
  Data$rain[is.na(Data$rain)]=0
  Data$count=ifelse(Data$rain<2.5,0,1)
    
  NumberDay=Data%>%
    group_by(X,Y,Year)%>%
    summarise(NbRainDay=sum(count))
  Mask=rio::import(paste("Data/CPC-UNIFIED/Mask/mask_ ",season," .csv",sep = ""))
  Mask=dplyr::select(Mask,-"Mean")
  Data_F=merge(NumberDay,Mask,by=c("X","Y"))
  DATA_F=filter(Data_F,mask>=1)
  dir.create(paste("Data/CPC-UNIFIED/DataBase/Number_RainDays/",season,sep = ""),recursive = T,showWarnings = F)
  rio::export(DATA_F,paste("Data/CPC-UNIFIED/DataBase/Number_RainDays/",season,"/",k,".csv",sep = ""))
  
  
  
  }
