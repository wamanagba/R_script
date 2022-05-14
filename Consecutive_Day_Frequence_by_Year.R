library("rio")
library(dplyr)
rm(list = ls())

#Importation of station list
Station<-rio::import("Synoptic_Station_All.csv")
Station<-filter(Station,Country=="Burundi")
Data<-rio::import("/home/acmad/Desktop/ACMAD_Git/Data/ARC2/Burundi/BUJUMBURA.csv")
#Station<-rio::import("E:/profil/Station_Burundi/Station_cordinates_Burundi.xls")
Data$Precipitation<-replace(Data$Precipitation,is.na(Data$Precipitation),0)
for(s in 2:length(Station$Station)){
   Data_Station<-filter(Data,Station=="BUJUMBURA")
  
  
}
Data_Station$year = as.numeric(format(Data_Station$Date,'%Y'))
#First Station
var<-Station$Station[2]
Data_Station<-filter(Data,Station==var)
Data_Station$year = as.numeric(format(Data_Station$Date,'%Y'))

#Fist Year
Dat<-filter(Data_Station,year==1983)
Dat$spell<-0
spell=0
for(i in 1:length(Dat$data)){
  if(Dat$data[i]<=2.5){
    spell=spell+1
    Dat$spell[i]=spell
  }
  else{
    spell=0
  }
 
}

Year<-seq(1984,2020,1)
All<-Dat
for(k in Year){
  print("Yes")
  Dat<-filter(Data_Station,year==k)
  Dat$spell<-0
  spell=0
  for(i in 1:length(Dat$data)){
    if(Dat$data[i]<=2.5){
      spell=spell+1
      Dat$spell[i]=spell
    }
    else{
      spell=0
    }
  }
  All<-do.call("rbind",list(All,Dat))
}
Burundi<-All

for(s in 2:length(Station$Station)){
  var=Station$Station[s]
  Data_Station<-filter(Data,Station==var)
  
  #Fist Year
  Dat<-filter(Data_Station,year==1983)
  Dat$spell<-0
  spell=0
  for(i in 1:length(Dat$data)){
    if(Dat$data[i]<=2.5){
      spell=spell+1
      Dat$spell[i]=spell
    }
    else{
      spell=0
    }
    
  }
  
  Year<-seq(1984,2020,1)
  All<-Dat
  for(k in Year){
    print("Yes")
    Dat<-filter(Data_Station,year==k)
    Dat$spell<-0
    spell=0
    for(i in 1:length(Dat$data)){
      if(Dat$data[i]<=2.5){
        spell=spell+1
        Dat$spell[i]=spell
      }
      else{
        spell=0
      }
    }
    All<-do.call("rbind",list(All,Dat))
  }
  Burundi<-do.call("rbind",list(Burundi,All))
}
#######################Frequency of 10 consecutive days without precipitatio

Burundi$Count<-ifelse(Burundi$spell>=10,1,0)


Frequency<-Burundi%>%
         group_by(Station,Country,year)%>%
         summarise(Total=sum(Count))
