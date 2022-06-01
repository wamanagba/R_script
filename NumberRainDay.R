
library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())

cpt1=(cpt+1)%%12
if(cpt1==0) cpt1=12
cpt2=(cpt+2)%%12
if(cpt2==0) cpt2=12

season=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")

dd=data.frame()



for (k in 1981:1982) {
  print(k);
  print(season)
  
  Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
  Data$Month=(format(Data$T,"%b"))
  Data$rain[is.na(Data$rain)]=0
  Data= filter(Data,Month %in% A[c(cpt,cpt1,cpt2)])
  
  Data$count=ifelse(Data$rain<2.5,0,1)
  
  NumberDay=Data%>%
    group_by(X,Y)%>%
    summarise(Number=sum(count))
  
  #dd=rbind(dd,Sum)
  
  
  
}