<<<<<<< HEAD
library(dplyr)
library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(rgeos)
rm(list = ls())

#Please set working directory

setwd("Give the Path")

#Red the Shape file

Africa=readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp")

#Read the Station Data (Lon,Lat) at least

Station<-rio::import("Synoptic_Station_All.csv")

#Creation the object
with_world <- ggplot(d=Station,aes(x =longitude, y = latitude))+geom_polygon(data = Africa, aes(x = long, y = lat, group = group),fill = "#DCDCDC", colour = "black",size = 1)+geom_point(size=3.5)+theme_light(base_size = 10)+theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),legend.key.size =unit(0.1,"cm"),axis.text = element_text(size=15,face="bold"),plot.title = ggplot2::element_text(size=20,hjust =0.5,face = "bold"),plot.subtitle = element_text(size=25,face = "bold"),axis.title.x=element_text(face = "bold") )+labs(title = paste("Title:",sep=""))+coord_quickmap(clip = "off") +   xlab("") +ylab("")

#Lon-Lat Scale
with_world<-with_world+ metR::scale_x_longitude(limits = c(-25, 65),breaks = seq(-25, 65,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10)) 

#Save the picture in png format
ggplot2::ggsave(plot=with_world ,paste("Africa_Map.png",sep=""),width = 15,height = 10,limitsize = FALSE,unit=c("in"),device='png')
=======
library(dplyr)
library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(rgeos)
rm(list = ls())

#Please set working directory

setwd("Give the Path")

#Red the Shape file

Africa=readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp")

#Read the Station Data (Lon,Lat) at least

Station<-rio::import("Synoptic_Station_All.csv")

#Creation the object
with_world <- ggplot(d=Station,aes(x =longitude, y = latitude))+geom_polygon(data = Africa, aes(x = long, y = lat, group = group),fill = "#DCDCDC", colour = "black",size = 1)+geom_point(size=3.5)+theme_light(base_size = 10)+theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),legend.key.size =unit(0.1,"cm"),axis.text = element_text(size=15,face="bold"),plot.title = ggplot2::element_text(size=20,hjust =0.5,face = "bold"),plot.subtitle = element_text(size=25,face = "bold"),axis.title.x=element_text(face = "bold") )+labs(title = paste("Title:",sep=""))+coord_quickmap(clip = "off") +   xlab("") +ylab("")

#Lon-Lat Scale
with_world<-with_world+ metR::scale_x_longitude(limits = c(-25, 65),breaks = seq(-25, 65,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10)) 

#Save the picture in png format
ggplot2::ggsave(plot=with_world ,paste("Africa_Map.png",sep=""),width = 15,height = 10,limitsize = FALSE,unit=c("in"),device='png')
>>>>>>> 098d00f9ac55258a3a88397031c7d8c0e43a21ef
