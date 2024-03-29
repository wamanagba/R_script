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
library(tidync)
rm(list = ls())
setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")
Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
season="May"
Data_source="NCEP-NCAR"

# setwd("~/Desktop/ACMAD_Git/")
k=2022

MinLon=-20;MaxLon=55
MinLat=-40;MaxLat=40
Name="May_2022_HR"
Name="May_2022_Prep"
  
  Data=rio::import(paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))


  Raster_file<-rasterFromXYZ(Data[c("X","Y","est_prcp")])
  
  Raster_file_1=disaggregate(Raster_file,10,method='bilinear')
  
  rr = raster::mask(Raster_file_1 ,Africa)
  
  Data <- as.data.frame(rasterToPoints(rr ))
  
  mybreaks <- c(20,100,200,300,400,500,Inf)
  
  #Function to return the desired number of colors
  
  mycolors<- function(x) {
    colors<-colorRampPalette(c("#8cb02c","#37fdf8","blue","#89522a","black","red"))(6)
    colors[1:x]
  }
  
  #Function to create labels for legend
  
  breaklabel <- function(x){
    labels<- as.character(c(20,100,200,300,400,500))
    labels[1:x]
  }
  ################################################################################
  
  #Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_source,"\n Season:",season,sep="")
  Title<-toupper(paste("Precipitation mensuelle from May \nData Source: ",Data_source,sep=""))
  
  #Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)
  
  l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =est_prcp),breaks= mybreaks, show.legend = TRUE) +
    scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
  
  last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
  
  #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
  
  last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
  
  last<-last+labs(title = Title,x="",y="")
  dir.create(paste("Products/malaria/",sep=""),recursive = T,showWarnings = F)
  
  jpeg(filename = paste("Products/malaria/",Name,".jpeg",sep=""),
       width = 14,
       height =14,
       units = "in",
       res=300)
  print(last)
  dev.off()

   

  

