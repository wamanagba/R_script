library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(rgeos)
rm(list = ls())

##Have the map for Africa

world_clip_f <-readOGR("Data\\NGA_adm/NGA_adm1.shp") 


##Import the Data

Data<-rio::import(paste("Data/Data_by_State.csv",sep=""))

(with_world <- ggplot(d=Data,aes(x = Lon, y = Lat))+
       geom_polygon(data = world_clip_f, 
                    aes(x = long, y = lat, group = group),
                    fill = "#DCDCDC", colour = "black",size = 1) + 
       geom_point(size=0.0,colour="black")+geom_text(label=round(Data$indice,2),size=18,colour="blue", hjust = "righ",vjust="top",fontface="bold", family = "serif",nudge_x = 0.20,nudge_y =0.20)+theme_light(base_size = 60)+theme(plot.title = ggplot2::element_text(size=80,face = "bold",family = "serif"),plot.subtitle = element_text(size=80,face = "bold",family = "serif"),axis.title.x=element_text(face = "bold",family = "serif") )+labs(title = paste("Vulnerability Index Over Nigeria",sep=""))
     +coord_quickmap() +  # Prevents stretching when resizing
       # Remove ugly grey background
       xlab("") +
     ylab("")+guides(colour=guide_legend(title="")))
    ggplot2::ggsave(plot=with_world ,paste("Products/Graphs/Maps.jpeg",sep=""),width = 40,height = 40,limitsize = FALSE,unit=c("in"),dpi=300)
    