################################################################################
#Optimal Script for Profiles and Onset_Date
#Ibrahim DAN DIJE, Intern at ACMAD
#02-06-2022
#This script is used for the station tha have the start from Jan to Jul
################################################################################

#########################Libraries##############################################
library(rio)
library(zoo)
library(reshape2)
library(dplyr)
library(ggplot2)  #ggplot() fortify()
library(rgdal)    #readOGR() spTransform()
library(raster)   #intersect()
library(ggsn)     #north2() scalebar()
library(rworldmap)#getMap()
library(rgeos)
#########################Language###############################################

Sys.setlocale("LC_TIME", 'english')

#########################Delete all environment data############################
rm(list = ls())

#Give Targed Year
Years<-c(2021)


for (Analogues_Years in Analogues) {
 
################################################################################
Year_Legend<-c("Mean","75% Mean","125% Mean",Analogues_Years)
#########################Gives the path#########################################

#Specify the working directory

setwd("D:/LRF_Products_Package/")

#The name of the folder where the data are stored

Data_Folder="Data"

#The name of the folder where the profiles will be saved: The folder will be created automatically

Profile_Folder="Western_Africa"

#Data Source
Source="CPC/Unified"

#Giev the file that contains the Station list

Station<-rio::import("Synoptic_Stations_January_Start.csv")
## Make sure that your Station list file contains at least the follwing column
## Station  Country

i=1

#Additional Array: Don't modify

Date_Legend<-c("")
Color_Legend<-c("")
Analogue_Onset<-c()
Max_Cum<-c()

nbelm=0 

################################################Don't need to modify anything here##################################
for (i in 1:length(Station$Station)){
  
  print(paste(Station$Country[i],": ",Station$Station[i]))
  
  Analogue_Onset<-c()
  
  #Read Data
  #Make sure that the columns of your data contain the following at least
  #Station Date Precipitation
  #Data<-rio::import(file=paste(Data_Folder,"/",Station$Station[i],".csv",sep=""))
  if(Station$Data_Source[i]=="CPC_Unified"){
  Data<-rio::import(paste("D:/Onset_Methods/CPC_Unified/",Station$Station[i],".csv",sep=""))
    
  }
  if(Station$Data_Source[i]=="ARC2"){
    
    Data<-rio::import(paste("D:/Onset_Methods/ARC2/Data/",Station$Station[i],".csv",sep=""))
    
  }
  
  Data<-Data %>% distinct(Date, .keep_all = TRUE)
  
  #Last_Date<-Data$Date[length(Data$Date)]
  Last_Date<-as.Date("2021-06-14")
  istation<-Station$Station[i]
  
  Data$year<-format(Data$Date,format="%Y")
  
  Data$month<-format(Data$Date,format="%m")
  
  Data$day<-format(Data$Date,format="%d")
  
  Data$Precipitation<-replace(Data$Precipitation,is.na(Data$Precipitation),0)
  
  Data$Precipitation<-replace(Data$Precipitation,Data$Precipitation>300,0)
  
  Data$Precipitation<-replace(Data$Precipitation,Data$Precipitation>200,0)
  
  dd.instat <- round(acast(Data,  month+day~year, value.var="Precipitation"),2)
  
  #Condition
  Instat <- dd.instat
  #Cumulative sum of the daily mean
  Clim.Mean <- rowMeans(Instat, na.rm=T)
  Clim.Cumsum <- cumsum(Clim.Mean)
  #75% cumulative sum
  Clim.Cumsum.75 <- Clim.Cumsum*0.75
  
  #125% cumulative sum
  Clim.Cumsum.125<- Clim.Cumsum*1.25
  
  Max_Cum<-c(Clim.Cumsum,Clim.Cumsum.75,Clim.Cumsum.125)
  
  if(max(Clim.Cumsum)>200){
    print("Yes")
    Data_1<-as.data.frame(dd.instat)
    Data_1$Cum_Mean<-Clim.Cumsum
    Data_1$doy<-seq(1,366)
    MeanOnset<-filter(Data_1,Cum_Mean>=50)
    Start_Cum<-MeanOnset$doy[1]-30
    
    
    Date_Mean<-format(as.Date(MeanOnset$doy[1],origin="2016-12-31"),"%d-%b")
    
    Data_75<-data.frame(Cum= Clim.Cumsum.75)
    Data_75$doy<-seq(1,366)
    Data_75<-filter(Data_75,Cum>=50)
    Onset_75=format(as.Date(Data_75$doy[1],origin="2016-12-31"),"%d-%b")
    
    Data_125<-data.frame(Cum=Clim.Cumsum.125)
    Data_125$doy<-seq(1,366)
    Data_125<-filter(Data_125,Cum>=50)
    Onset_125=format(as.Date(Data_125$doy[1],origin="2016-12-31"),"%d-%b")
    Date_Legend<-c(Date_Mean,Onset_125,Onset_75)
    if(Start_Cum<0){
      Start_Cum<-0
    }
    
    if(Start_Cum>=0){
      Instat[1:Start_Cum,]=0
      
      for (Year in Analogues_Years) {
        
        l<-length(seq(as.Date(paste(Year,"-01-01",sep="")),Last_Date,by="days"))
        
        #Cum_Year<-cumsum(na.fill(Instat[,as.character(Year)],0))
        
        Cum_Year<-cumsum(na.fill(Instat[1:l+1,as.character(Year)],0))
        
        Onset_Year<-data.frame(Cum=Cum_Year)
        Onset_Year$doy<-seq(1,l)
        Onset_Year<-filter(Onset_Year,Cum>=50)
        Onset_Year_Date=format(as.Date(Onset_Year$doy[1],origin="2016-12-31"),"%d-%b")
        Onset_Year_Date_Numeric<-Onset_Year$doy[1]
        #length(Date_Legend)
        Date_Legend[length(Date_Legend)+1]<-Onset_Year_Date
        Analogue_Onset[length(Analogue_Onset)+1]<-Onset_Year_Date_Numeric
        Max_Cum[length(Max_Cum)+1]<-max(Cum_Year)
      }
    }
   
    for (KK in 1:length(Analogue_Onset)) {
      if(KK==1){
        Analogues_Years_Onset<-data.frame(Year=Analogues_Years[KK],Doy=Analogue_Onset[KK])
      }
      else{
        Analogues_Years_Onset1<-data.frame(Year=Analogues_Years[KK],Doy=Analogue_Onset[KK])
        Analogues_Years_Onset<-rbind(Analogues_Years_Onset,Analogues_Years_Onset1)
      }

    }
   
    if(nbelm==0){
        Onset<-data.frame(Station=Station$Station[i],Lon=Station$longitude[i],Lat=Station$latitude[i],Onset75=Data_75$doy[1],MeanOnset=MeanOnset$doy[1],Onset125=Data_125$doy[1],Year=Analogues_Years_Onset[Analogues_Years_Onset$Year==Analogues_Years,]$Year,OnsetAnalogue1D=Analogues_Years_Onset[Analogues_Years_Onset$Year==Analogues_Years,]$Doy)
      nbelm=nbelm+1
    }
    else{
      Onset1<-data.frame(Station=Station$Station[i],Lon=Station$longitude[i],Lat=Station$latitude[i],Onset75=Data_75$doy[1],MeanOnset=MeanOnset$doy[1],Onset125=Data_125$doy[1],Year=Analogues_Years_Onset[Analogues_Years_Onset$Year==Analogues_Years,]$Year,OnsetAnalogue1D=Analogues_Years_Onset[Analogues_Years_Onset$Year==Analogues_Years,]$Doy)
      Onset<-rbind(Onset,Onset1)
    }
  }
}

dir.create("Onset",recursive = T,showWarnings = F)

rio::export(Onset,paste("Onset/Onset_Date_CPC_UNIFIED_ARC2.csv",sep=""))

#################################State of Onset#################################

################################################################################
# 
# Onset Date Processing: Forecast and Observed Onset Date
# 
#
################################################################################



Onset_Started<-Onset

Onset_Started<-filter(Onset,!is.na(OnsetAnalogue1D))

####################################### Started Onset Status ###################

Onset_Started$Tardive<-ifelse(Onset_Started$OnsetAnalogue1D>=Onset_Started$Onset75,1,0)

Onset_Started$Normal_Tardive<-ifelse(Onset_Started$OnsetAnalogue1D<Onset_Started$Onset75 & Onset_Started$OnsetAnalogue1D>Onset_Started$MeanOnset,2,0)

Onset_Started$Normal_Precos<-ifelse(Onset_Started$OnsetAnalogue1D<=Onset_Started$MeanOnset  & Onset_Started$OnsetAnalogue1D>Onset_Started$Onset125,3,0)

Onset_Started$Precos<-ifelse(Onset_Started$OnsetAnalogue1D<=Onset_Started$Onset125,4,0)

Onset_Started$Situation<-rowSums(Onset_Started[,c("Precos","Normal_Precos","Normal_Tardive","Tardive")])

rio::export(Onset_Started,paste("Onset/Onset_Started_",Year,".csv",sep=""))


################################################################################



Data<-Onset_Started


l<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

Africa=readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp")

Legend_Title="Observed start of the Agriculture\nSeason departure from Average."

Map_Title=toupper(paste("START OF THE AGRICULTURE SEASON FROM JANUARY TO 14 JUNE IN ",Year,"\nOVER SUB-SAHARAN AFRICA.",sep=""))

with_world <- ggplot(d=Data,aes(x = Lon, y = Lat,colour=as.factor(Situation)))+
  geom_polygon(data = Africa, aes(x = long, y = lat, group = group),fill = "white", colour = "black",size = 1) + geom_point(size=6,aes(colour=as.factor(Situation)))+scale_color_manual(values=c("red","orange","green","#1fc600"),labels=c(paste("LATE",sep=""),paste("NEAR AVERAGE TO LATE",sep=""),paste("NEAR AVERAGE TO EARLY",sep=""),paste("EARLY",sep="")))+theme_light(base_size = 30)+theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),legend.key.size =unit(0.1,"cm"),legend.text = element_text(size=35,face="bold"),legend.title = element_text(size=35,face="bold",family = ""),axis.text = element_text(size=15,face="bold"),plot.title = ggplot2::element_text(size=35,hjust =0.5,face = "bold"),plot.subtitle = element_text(size=25,face = "bold"),axis.title.x=element_text(face = "bold") )+labs(title = Map_Title)+coord_quickmap(clip = "off") +   xlab("") +ylab("")+guides(colour=guide_legend(title=Legend_Title))+annotation_custom(l, xmin = 60, xmax = 65, ymin = 35, ymax =40)

with_world<-with_world+theme(plot.margin=margin(0,0,0,0),legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(0, 0, 0, 0),legend.text = element_text(size=20,face = "bold"),legend.title  = element_text(size=25,face = "bold"),axis.text.x = element_text(size=20,face = "bold"),axis.text.y = element_text(size=20,face = "bold"))

with_world<-with_world+ metR::scale_x_longitude(limits = c(-25, 65),breaks = seq(-25, 65,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10)) 

dir.create("Onset_Map/",recursive = T,showWarnings = F)

ggplot2::ggsave(plot=with_world ,paste("Onset_Map/Onset_",Year,"_Sub_Saharan_Africa.jpeg",sep=""),width = 25,height = 18,limitsize = FALSE,unit=c("in"),dpi=100)
with_world<-magick::image_trim(magick::image_read(paste("Onset_Map/Onset_",Year,"_Sub_Saharan_Africa.jpeg",sep="")))
magick::image_write(with_world, path = paste("Onset_Map/Onset_",Year,"_Sub_Saharan_Africa.jpeg",sep=""), format = "jpeg", quality = 100)

#ggplot2::ggsave(plot=with_world ,paste("Onset_Map/Onset_",Year,"_Sub_Saharan_Africa.tiff",sep=""),width = 15,height = 10,limitsize = FALSE,unit=c("in"),device='tiff')

}