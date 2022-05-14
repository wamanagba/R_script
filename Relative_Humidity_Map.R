
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
rm(list = ls())
setwd("C:/Users/Yacou/Desktop/ACMAD/DEKAD/")

#Shape file

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 

# Precipitation Data: Lon, Lat, Cum
Data<-import("Data/RH_level_850_April_decad1.csv")



########################Masking the Continent###################################

data<-data.frame(Data$Longitude,Data$Latitude,Data$`Relative humidity`)


coords = cbind(Data$Longitude,Data$Latitude)


sp = SpatialPoints(coords,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

# make spatial Data frame

RR_Perc = SpatialPointsDataFrame(coords = sp,data)

RR_Perc_spdf<-RR_Perc[!is.na(over(RR_Perc, as(Africa, "SpatialPolygons"))), ]

RR_Perc_df<-as.data.frame(RR_Perc_spdf)



##############################Legend############################################
mybreaks <- c(-Inf,20,40,60,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("red","orange","yellow","gray"))(4)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c("<20",40,60,">60"))
  #labels<- as.character(seq(1,5))
  labels[1:x]
}
################################################################################



Title<-paste("Title",sep="")

Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=RR_Perc_df, aes(Data.Longitude,Data.Latitude,z = Data..Relative.humidity.),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(4), name="", drop=FALSE, guide = guide_legend(reverse = F))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = "NA",color = "black",size = 1.1)+ theme(text = element_text(family = "Montserrat"),legend.position = c(.09, .09),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=20,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
last<-last+  annotation_custom(Im, xmin = 50, xmax = 60, ymin =30, ymax = 40) +coord_cartesian(clip = "off")
last=last + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank())
last<-last+labs(title = Title,x="",y="")+metR::scale_x_longitude(limits = c(-25,60 ),breaks = seq(-25,60,10)) + metR:: scale_y_latitude(limits = c(-40,40),breaks = seq(-40,40,10))

jpeg(filename = paste("Products/Relative_Humidity.jpeg",sep=""),
     width = 11,
     height =12,
     units = "in",
     res=300)
print(last)
dev.off()
