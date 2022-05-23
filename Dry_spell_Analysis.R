library(ncdf4)
library(dplyr)
library(rio)
rm(list = ls())

setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")
#source("C:/Users/Yacou/Desktop/ACMAD_Git/Script/Dry_spell_Function.R")
source("C:/Users/Yacou/Desktop/ACMAD_Git/Script/Dry_spell_3.R")

k=1981
cpt=1
# A=c(7,8.9)
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
for(cpt in 1:12){
  cpt1=(cpt+1)%%12
  if(cpt1==0) cpt1=12
  cpt2=(cpt+2)%%12
  if(cpt2==0) cpt2=12
for (k in 1981:2010) {
  print(k)
  if(k==1981){
    Data<-rio::import(paste("Data/CHIRPS/CSV_Format/",k,".csv",sep=""))
    Data$Year<-format(Data$T,"%Y")
    # Data$Month=as.numeric(format(Data$T,"%m"))
    Data$Month1=(format(Data$T,"%b"))
    Data$prcp[is.na(Data$prcp)]=0

    Data= filter(Data,Month1 %in% A[c(cpt,cpt1,cpt2)])
    D1= Data
    # Null= (sum(is.na(Data$prcp))/nrow(Data))*100
    
    Data_Dry_Speel<-Data%>%
      group_by(X,Y,Year)%>%
      summarise(Dry_Spell=CDD_function(prcp,2.5,Nb=10))
    
    Data<-merge(Data,Data_Dry_Speel,by=c("X","Y","Year"))   
    
  
  }else{
    Data1<-rio::import(paste("Data/CHIRPS/CSV_Format/",k,".csv",sep=""))
    Data1$Year<-format(Data1$T,"%Y")
    # Data1$Month=as.numeric(format(Data1$T,"%m"))
    Data1$Month1=(format(Data1$T,"%b"))
    Data1$prcp[is.na(Data1$prcp)]=0
    # Data1= filter(Data1,Month %in% A)
    Data1= filter(Data1,Month1 %in% A[c(cpt,cpt1,cpt2)])
    D1=rbind(D1,Data1)
    
    Data_Dry_Speel1<-Data1%>%
      group_by(X,Y,Year)%>%
      summarise(Dry_Spell=CDD_function(prcp,2.5,Nb=10))
    
  
   
    Data_Dry_Speel<-rbind(Data_Dry_Speel,Data_Dry_Speel1)
  }
}


Data_Dry<-Data_Dry_Speel%>%
  group_by(X,Y)%>%
  summarise(Mean=mean(Dry_Spell))

Clima<-D1%>%
  group_by(X,Y)%>%
 summarise(clim=sum(prcp))

Clima$binary= ifelse(Clima$clim>100,1,0)

Data_final=merge(Clima,Data_Dry,by=c("X","Y"))
Data_final$spell=Data_final$binary*Data_final$Mean

dir.create(paste("Data/Dry_Speel_Data/",sep = ""),recursive = T, showWarnings = F)
#rio::export(Data_Dry,"Data/Dry_Spell_10Day_1983_2010.csv")
rio::export(Data_final,paste("Data/Dry_Speel_Data/Dry_Spell_10Day_1981_2010_",substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),".csv",sep = ""))
}
