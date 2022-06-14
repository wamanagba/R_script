
library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())

setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")

k=1981
cpt=1
for (cpt in 1:12) {
  
cpt1=(cpt+1)%%12
if(cpt1==0) cpt1=12
cpt2=(cpt+2)%%12
if(cpt2==0) cpt2=12

season=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")

dd=data.frame()

for (k in 1981:2010) {
  print(k);
  print(season)
  
  Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
  #Data$Year<-format(Data$T,"%Y")
  Data$Month=(format(Data$T,"%b"))
  Data$rain[is.na(Data$rain)]=0
  Data= filter(Data,Month %in% A[c(cpt,cpt1,cpt2)])
  
  Sum=Data%>%
    group_by(X,Y)%>%
    summarise(prcp=sum(rain))
  
  dd=rbind(dd,Sum)
}

Annual_Total<-dd%>%
  group_by(X,Y)%>%
  summarise(Mean=mean(prcp,na.rm=T))

Annual_Total$mask=ifelse(Annual_Total$Mean>=100,1,0)

dir.create(paste("Data/CPC-UNIFIED/Mask/",sep = ""),recursive = T,showWarnings = F)
rio::export(Annual_Total, paste("Data/CPC-UNIFIED/Mask/mask_",season,".csv"))

}
