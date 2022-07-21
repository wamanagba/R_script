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
library(mondate)
options(download.file.extra = '--no-check-certificate')
rm(list=ls())

setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")
#dir.create("Results",recursive = T,showWarnings = F)

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 

#Give the month abbr and name

# Month="Mar"
# Month_name="March"
#Year=2022
Stat_day=15
End_day=5
Stat_Month="Jun"
End_Month="Jun"
End_Year=2022
Year=2022
Name=paste(Stat_day,Stat_Month,"_",End_day,End_Month,"_",Year,sep = "")
#Monthly Cumulative

#download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.ARC2/.daily/.est_prcp/T/%28",Stat_day,"%20",Stat_Month,"%20",End_Year,"%29%28",End_day,"%20",End_Month,"%20",Year,"%29RANGEEDGES/data.nc",sep=""),mode="wb",paste("Data/Drought_Monitoring/weekly_",Name,"_",Year,".nc",sep=""))
download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/X/%2820W%29%2855E%29RANGEEDGES/T/%280000%20",Stat_day,"%20",Stat_Month,"%20",End_Year,"%29%280000%20",End_day,"%20",End_Month,"%20",Year,"%29RANGEEDGES/Y/%2840S%29%2840N%29RANGEEDGES/data.nc",sep=""),mode="wb",paste("Data/Drought_Monitoring/weekly_",Name,"_",Year,".nc",sep=""))

Data_Cum<-raster::raster(x = paste("Data/Drought_Monitoring/weekly_",Name,"_",Year,".nc",sep=""))


Data_interpolted_Cum<-raster::disaggregate(Data_Cum,8,method="bilinear")

Data_Masked_Cum<-raster::mask(Data_interpolted_Cum,Africa)

Data_df_Cum<- as.data.frame(rasterToPoints(Data_Masked_Cum))

names(Data_df_Cum)[3]="Precipitation"

# #Monthly Climatology
# #http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(Jun%201981)/(Jun%202010)/RANGEEDGES/Y/-40/0.5/40/GRID/X/25/0.5/55/GRID/T/12/STEP/%5BT%5Daverage/31/mul/data.nc
# 
# download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(",Month,"%201981)/(",Month,"%202010)/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/12/STEP/%5BT%5Daverage/31/mul/data.nc",sep=""),mode="wb", paste("Data/Drought_Monitoring/Climatology_",Month_name,"_",Year,".nc",sep=""))

Data_Clim=rio::import("Products/Drought_Monitoring/Data/climatologyJun_Jul.csv")
#Data_Clim<-raster::raster(x=paste("Data/Drought_Monitoring/Climatology_",Month_name,"_",Year,".nc",sep=""))
DD=filter(Data_Clim,Data_Clim$Month== Stat_Month | Data_Clim$Month==End_Month)
DD=filter(DD,DD$Day<=15,DD$Day>=5)
Data_Clim=DD
Raster_file<-rasterFromXYZ(Data_Clim[c("X","Y","Cum2")])

Data_interpolted_Clim<-raster::disaggregate(Raster_file,8,method="bilinear")

Data_Masked_Clim<-raster::mask(Data_interpolted_Clim,Africa)

Data_df_Clim<- as.data.frame(rasterToPoints(Data_Masked_Clim))

names(Data_df_Clim)[3]="Climatology"

Percentage<-merge(Data_df_Cum,Data_df_Clim,by=c("x","y"))

Percentage$RR<-ifelse(Percentage$Climatology<=10,100,(Percentage$Precipitation/Percentage$Climatology)*100)

rio::export(Percentage[,c("x","y","RR")],paste("Products/Drought_Monitoring/Data/Percentage_",Name,".csv",sep=""))



###############################Mapping##########################################


mybreaks <- c(0,50,75,125,200,Inf)

#Function to return the dersired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("red","orange","darkgray","#69d205","darkgreen"))(5)
  colors[1:x]
}

#Function to create labels for legend
breaklabel <- function(x){
  labels<- as.character(c("Well below average","Below average","Near average","Above average","Well above average"))
  labels[1:x]
}

Title = paste("MONTHLY PRECIPITATION IN PERCENT OF AVERAGE FOR ",toupper(Name)," ", Year,"\n Data source: CAMS-OPI",sep="")

Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Percentage, aes(x,y,z = RR),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(5), name="", drop=FALSE, guide = guide_legend(reverse = TRUE))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.1, .1),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20,face = "bold"),plot.title = element_text(hjust = 0.5,size=20,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
last<-last+  annotation_custom(Im, xmin = 50, xmax = 60, ymin =30, ymax = 40) +coord_cartesian(clip = "off")
last<-last+ metR::scale_x_longitude(limits = c(-25, 60),breaks = seq(-25, 60,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10))
last<-last+labs(title = Title,x="",y="")

  
dir.create(paste("Products/Drought_Monitoring/Maps/",Name,sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/Drought_Monitoring/Maps/",Name,"/Percentage_",Name,"_",Year,".jpeg",sep=""),
     width = 11,
     height = 12,
     units = "in",
     res=300)
print(last)
dev.off()

