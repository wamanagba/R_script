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
Dat<-rio::import(paste("Data/Dry_Spell_lengh_Max_10_1983_2010.csv",sep=""))
Data=Dat
Data=Data%>%
  group_by(X,Y)%>%
  summarise(Mean2=mean(Mean))

Data_2010=filter(Dat,Year==2010)
anomaly=merge(Data,Data_2010,by=c("X","Y"))
anomaly$Anomaly=anomaly$Mean2-anomaly$Mean
colnames(anomaly)[6]="Mean";colnames(anomaly)[5]="Mean1"
Data=anomaly
Raster_file<-rasterFromXYZ(Data[c("X","Y","Mean")])

Raster_file_1=disaggregate(Raster_file,8,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

Data <- as.data.frame(rasterToPoints(rr ))
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
mybreaks <- c(-2,-1,5,0,1,2,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("#f0ff00","#ffce00","#ff9a00","#ff5a00","#ff0000","darkred"))(7)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<-as.character(c(-2,-1,0,1,2))
  #labels<- as.character(seq(1,5))
  labels[1:x]
}
################################################################################

Title<-paste("Rainfall average yearly tatal Map over Nigeria","\nRef: 1983-2021","\nData Source: ",Data_Source,sep="")

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =Mean),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,2.5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,2.5))

last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/essais/Afrique/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/essais/Afrique/","Anomly2_Spell ","_",Data_Source,".jpeg",sep=""),
     width = 14,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()



