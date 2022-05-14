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
setwd("~/Mening/Meningetis_Bulletin/")

Date=gsub("-","",Sys.Date())
Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
#Week 1

#Temperature
Raster_file_Tmp<-raster::raster(paste("Data/Weekly_Mean_1_20220321.nc",sep=""),varname="t")
#Raster_Mask = mask(Raster_file, Africa)

#Raster_file_1=disaggregate(Raster_file_Tmp,6,method='bilinear')

rr = mask(Raster_file_Tmp,Africa)

Data_Temp <- as.data.frame(rasterToPoints(rr ))

Data_Temp$Temperature<-Data_Temp$Temperature-273.15

#RH
Raster_file_RH<-raster::raster(paste("Data/Weekly_Mean_1_20220321.nc",sep=""),varname="r")

#Raster_Mask = mask(Raster_file, Africa)

#Raster_file_1=disaggregate(Raster_file_RH,6,method='bilinear')

rr = mask(Raster_file_RH ,Africa)

Data_RH<- as.data.frame(rasterToPoints(rr ))

#Dust

Raster_file_Dust<-raster::raster(paste("Data/Dust_Data_20220321.nc",sep=""),varname="SCONC_DUST")

Raster_file_Dust <- resample(Raster_file_Dust,Raster_file_RH, method = "bilinear")

#Raster_file_1=disaggregate(Raster_file_Dust,6,method='bilinear')

rr = mask(Raster_file_Dust,Africa)

Data_Dust <- as.data.frame(rasterToPoints(rr))


Data_Dust$Dust.Surface.Concentration<-Data_Dust$Dust.Surface.Concentration*1000000000

Data<-merge(Data_RH,Data_Temp,by=c("x","y"))

Data<-merge(Data,Data_Dust,by=c("x","y"))

########################################################################################
Data$Vigilance<-4
Data$mask_rh<-NA
Data$mask_tmp<-NA
Data$mask_tmp<-NA
Data$mask_dust<-NA

# masque RH
Data$mask_rh<-ifelse(Data$Relative.humidity<=20,1,Data$mask_rh)

Data$mask_rh<-ifelse(Data$Relative.humidity>20 & Data$Relative.humidity<=40,2,Data$mask_rh)

Data$mask_rh<-ifelse(Data$Relative.humidity>40 & Data$Relative.humidity<=60,3,Data$mask_rh)

Data$mask_rh<-ifelse(Data$Relative.humidity>60,4,Data$mask_rh)


     
#masque temp

Data$mask_tmp<-ifelse(Data$Temperature>=27,1,Data$mask_tmp)

Data$mask_tmp<-ifelse(Data$Temperature>27 & Data$Temperature<30,2,Data$mask_tmp)

Data$mask_tmp<-ifelse(Data$Temperature<27,3,Data$mask_tmp)

#masque DUST

Data$mask_dust<-ifelse(Data$Dust.Surface.Concentration>=400,1,Data$mask_dust)

Data$mask_dust<-ifelse(Data$Dust.Surface.Concentration>150 & Data$Dust.Surface.Concentration<400,2,Data$mask_dust)

Data$mask_dust<-ifelse(Data$Dust.Surface.Concentration<=150,3,Data$mask_dust)

#definition of mask of each vigilance level


 
Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==1 & Data$mask_dust==1,1,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==2 & Data$mask_dust==1,1,Data$Vigilance)
Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==1 & Data$mask_dust==2,2,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==2 & Data$mask_dust==2,2,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==2 & Data$mask_dust==1,1,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==1 & Data$mask_dust==2,2,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==1 & Data$mask_dust==2,2,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==2 & Data$mask_dust==2,2,Data$Vigilance)


Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==1 & Data$mask_dust==1,2,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==2 & Data$mask_dust==1,2,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==2 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==1 & Data$mask_dust==3,3,Data$Vigilance)


Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==1 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==1 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==1 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==2 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==3 & Data$mask_tmp==1 & Data$mask_dust==1,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==3 & Data$mask_tmp==1 & Data$mask_dust==2,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==3 & Data$mask_tmp==2 & Data$mask_dust==2,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==3 & Data$mask_tmp==1 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==2 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==3 & Data$mask_dust==1,2,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==3 & Data$mask_dust==2,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==2 & Data$mask_tmp==3 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==3 & Data$mask_tmp==3 & Data$mask_dust==2,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==1 & Data$mask_tmp==3 & Data$mask_dust==2,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==3 & Data$mask_tmp==3 & Data$mask_dust==3,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$mask_rh==4,4,Data$Vigilance)
Data$Vigilance<-ifelse(Data$mask_rh==4 & Data$mask_tmp==1 & Data$mask_dust==1,3,Data$Vigilance)
Data$Vigilance<-ifelse(Data$mask_rh==4 & Data$mask_tmp==2 & Data$mask_dust==1,3,Data$Vigilance)
Data$Vigilance<-ifelse(Data$mask_rh==4 & Data$mask_tmp==1 & Data$mask_dust==2,3,Data$Vigilance)

Data$Vigilance<-ifelse(Data$x<(-18) ,4,Data$Vigilance)
Data$Vigilance<-ifelse(Data$x>40 ,4,Data$Vigilance)
Data$Vigilance<-ifelse(Data$y>25 ,4,Data$Vigilance)

# Data$Vigilance<-ifelse(Data$Temperature >=27 & Data$Relative.humidity<=20 & Data$Dust.Surface.Concentration>=400,1,Data$Vigilance)
# 
# Data$Vigilance<-ifelse(Data$Temperature >=27 & Data$Temperature<=30 & Data$Relative.humidity<=20 & Data$Dust.Surface.Concentration>=400,2,Data$Vigilance)
# Data$Vigilance<-ifelse(Data$Temperature >=27 & Data$Relative.humidity>=20 & Data$Relative.humidity<=40 & Data$Dust.Surface.Concentration>=400,2,Data$Vigilance)
# Data$Vigilance<-ifelse(Data$Temperature >=30 & Data$Relative.humidity<=20 & Data$Dust.Surface.Concentration<400 & Data$Dust.Surface.Concentration>=150,2,Data$Vigilance)
# Data$Vigilance<-ifelse(Data$Temperature >=27 & Data$Relative.humidity>=20 & Data$Relative.humidity<=40 & Data$Dust.Surface.Concentration<400 & Data$Dust.Surface.Concentration>=150,2,Data$Vigilance)
# 
# 
# Data$Vigilance<-ifelse(Data$Temperature >=27 & Data$Relative.humidity<=20 & Data$Dust.Surface.Concentration<150,3,Data$Vigilance)
# Data$Vigilance<-ifelse(Data$Temperature >=27 & Data$Temperature <=30 & Data$Relative.humidity>=40 & Data$Relative.humidity<=60 & Data$Dust.Surface.Concentration<150,3,Data$Vigilance)
# Data$Vigilance<-ifelse(Data$Temperature >=27 & Data$Temperature <=30 & Data$Relative.humidity>60 & Data$Dust.Surface.Concentration<=150,3,Data$Vigilance)
# 
# # Data$Vigilance<-ifelse(Data$Vigilance==4,3,Data$Vigilance)
# Data$Vigilance<-ifelse(Data$x<(-18) ,4,Data$Vigilance)
# Data$Vigilance<-ifelse(Data$x>40 ,4,Data$Vigilance)
# 
# Data$Vigilance<-ifelse(Data$y>25 ,4,Data$Vigilance)

mybreaks <- c(1,2,3,4)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("red","orange","#fdee00"))(3)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c("","",""))
  #labels<- as.character(seq(1,5))
  labels[1:x]
}
################################################################################

Week_1<-paste(gsub("-","",Sys.Date()+8),"-",gsub("-","",Sys.Date()+14),sep="")

Title<-paste("VIGILANCE MAP FOR MENINGITIS OUTBREAKS IN AFRICA\nissued on ","Date"," for week: ","Week_1",sep="")

Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z = Vigilance),breaks= mybreaks, show.legend = F) +
  scale_fill_manual(palette=mycolors, values=breaklabel(3), name="", drop=FALSE, guide = guide_legend(reverse = F))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = "NA",color = "black",size = 1.1)+ theme(text = element_text(family = "Montserrat"),legend.position = c(.09, .09),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=20,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
last<-last+  annotation_custom(Im, xmin = 50, xmax = 60, ymin =30, ymax = 40) +coord_cartesian(clip = "off")
last=last + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank())
last<-last+labs(title = Title,x="",y="")+metR::scale_x_longitude(limits = c(-25,60 ),breaks = seq(-25,60,10)) + metR:: scale_y_latitude(limits = c(-40,40),breaks = seq(-40,40,10))

jpeg(filename = paste("Products/","/Vigilence_Map_","Week_1",".jpeg",sep=""),
     width = 11,
     height =12,
     units = "in",
     res=300)
print(last)
dev.off()
