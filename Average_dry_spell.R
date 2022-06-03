
library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())
source("C:/Users/Yacou/Desktop/ACMAD_Git/Script/Dry_spell_3.R")
Data_Source="CPC-UNIFIED"
setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")
Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
#Data_Source="CPC-UNIFIED"
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40
cpt=1


 for (cpt in c(1,4,7,10)) {
  
  cpt1=(cpt+1)%%12
  if(cpt1==0) cpt1=12
  cpt2=(cpt+2)%%12
  if(cpt2==0) cpt2=12
  
  A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
  
  season=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")
  
  dd=data.frame()
  
  
  k=2000
  i=0
  for (k in 1981:2010) {
    print(k);print(season)
    i=i+1
    Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
    Data$Month=format(Data$T,"%b")
    Data$rain[is.na(Data$rain)]=0
    Data= filter(Data,Month %in% A[c(cpt,cpt1,cpt2)])

    
    mean_dry=Data%>%
      group_by(X,Y)%>%
      summarise(MeanT=CDD_function(rain,t=2.5,Nb=10))
    
    dd=rbind(dd,mean_dry)
  }
  
  data=dd%>%
    group_by(X,Y)%>%
    summarise(Mean=mean(MeanT))

  dir.create(paste("Data/Dry_Speel_Data/Average_dry_speel",sep = ""),recursive = T,showWarnings = F)
  rio::export(data,paste("Data/Dry_Speel_Data/Average_dry_speel/",season,".csv",sep = ""))
  
 }