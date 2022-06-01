library(rio)
library(dplyr)
library(ggplot2)
library(rio)
library(dplyr)
library(ncdf4)
library(RColorBrewer)
library(metR)
library(ggplot2)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(raster)
library(sp)
library(ggdark)
library(showtext)
library(ggrepel)
library(Hmisc)
rm(list = ls())

options(download.file.extra = '--no-check-certificate')
options(timeout=600)
options(warn=-1)
rm(list=ls())
#Climatology
#https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.Monthly/.extREALTIME/.rain/T/(Jan%202021)/(Sep%202021)/RANGEEDGES/Y/-40/0.5/40/GRID/X/55/0.5/-25/GRID/T/9/runningAverage/270/mul/ngridtable+table-+skipanyNaN+3+-table+.html
#############################################################################################################################################################################
setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
Data_Source="CHIRPS"
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40

##############################Legend############################################
###Read Data########################

cpt=1
A=c(7,8.9)
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
#for(cpt in 1:12){
  cpt1=(cpt+1)%%12
  if(cpt1==0) cpt1=12
  cpt2=(cpt+2)%%12
  if(cpt2==0) cpt2=12


season=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")
Dat=rio::import(paste("Data/Dry_Speel_Data/Dry_Spell_10Day_2000_",season,".csv",sep = ""))
#Dat=rio::import(paste("Data/Dry_Speel_Data/climatologie/clim_",season,".csv",sep=""))
#Dat<-rio::import(paste("Data/Dry_Speel_Data/Dry_Spell_10Day_1981_2010_Yacou",season,".csv",sep = ""))
Data=Dat

Data=filter(Data,binary==1)


Raster_file<-rasterFromXYZ(Data[c("X","Y","Mean")])

Raster_file_1=disaggregate(Raster_file,8,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

Data <- as.data.frame(rasterToPoints(rr ))
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
#mybreaks <- c(-Inf,100,400,600,1200,1500,Inf)
mybreaks <- c(-Inf,1,2,3,4,5,Inf)
#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("#89522a","#8cb02c","darkviolet","#37fdf8","#2ccac6","blue"))(6)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c("< 100 mm: Off Season Area","100-400 mm: Arid Zone","400-600 mm: Semi-Arid Zone","600-1200 mm: Sub-Humid Zone","1200-1500 mm: Moist Sub-Humid Zone",">1500 mm:  Humid Zone"))
  labels[1:x]
}
################################################################################

Title<-paste("Average number of Dry spell over 10 days")#","\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =Mean),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))

last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/climatologie/Afrique/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/climatologie/Afrique/","dry.jpeg",sep=""),
     width = 14,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()
#}


