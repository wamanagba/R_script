# 
# 
# setwd('C:/Users/Yacou/Desktop/ACMAD_Git')
# Stations<-rio::import("Synoptic_Station_All.csv")
# 
# # Importing the Niger stations
# CounTry= 'Burkina'
# Niger_station= Station<-filter(Stations,Country==CounTry)
# 
# i=1
# 
#   station= Niger_station$Station[i]
#   data=rio::import(paste('Data/ARC2/',CounTry,'/',station,'.csv',sep=""))
#   
#   data$Year = as.numeric(format(data$Date,'%Y'))
#   data$Month = format(data$Date,'%b')
#   data= filter(data,Month == "Aug")
#   data= filter(data,Year == 2021)
#   
# 
# 
# 
# data$Precipitation[is.na(data$Precipitation)]=0
# prcp=data$Precipitation
# 
# Nb=10
#  t=2.5

CDD_function=function(prcp,t,Nb) {
  
  prcp[length(prcp)+1]=10
  prcp[is.na(prcp)]=0
  spell <- ifelse(prcp > 1, 0, 0)
  SPELL=0
  for(i in 1:length(prcp)){
    if(prcp[i]<=t){
      SPELL=SPELL+1
      spell[i]=SPELL
    }
    else{
      SPELL=0
    }
  }
  
aa=as.data.frame(spell)

All=aa
CDD<-All  

n=length(CDD$spell)-1
for (i in 2:n) {
  if(All$spell[i+1]==0 & All$spell[i-1]>0){
    #print(All$spell[i])
  }else{
    CDD$spell[i]=0}
}


   CDD=cbind(CDD,aa$spell)
   names(CDD)[2]="Spell2"
   CDD$spell[1]=0
   CDD$Count<-ifelse(CDD$spell>=Nb,1,0)
   
   # Plus long sequence seche
   S=max(CDD$spell)
   
   # Le nombre desequences seche superieur a 10 jours
   S10=sum(CDD$Count)
   
   # Le nombre moyen de jour de sequence seche
   NbMoyen=filter(CDD,spell!=0)
   mean_speel=mean(NbMoyen$spell)
   return(mean_speel)
}

#CDD_function(prcp = prcp,t=2.5,Nb=10)


# 
# 
# 
# 
# 
# All <- data.frame(#Country = character(),
#                   #Station = character(),
#                   #Lon=double(),
#                   #Lat=double(),
#                   #Date=character(),
#                   #Precipitation=double(),
#                   year=double(),
#                   Max_spell=double(),
#                   stringsAsFactors = FALSE)
# 
# Year<-seq(1983,2021,1)
# k=2000
# for(k in Year){
#   Dat<-filter(Data,year==k)
#   max=CDD_function(prcp = Dat$Precipitation,t=2.5,Nb=10)
#   All=rbind(All,data.frame(k,max))
# }
# 
# d1=rio::import("Data/Mangodara2.csv")
# colnames(d1)[1]=c('year')
# colnames(All)[1]=c('year')
# d2=merge(All,d1,by="year")
# d2$diff=d2$max-d2$V3         
