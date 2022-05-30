library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())

setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")

k=1981
for (k in 1981:1982) {
  print(k)
  if(k==1981){
    Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))

    Data$Year<-format(Data$T,"%Y")
    Data$Month<-as.numeric(format(Data$T,"%b"))

    Data=filter(Data,Month %in% c("Jan","Feb","Mar"))
    Data_Cum<-Data%>%
      group_by(X,Y,Year)%>%
      summarise(Cum=sum(rain,na.rm=T))
    
  }else{
    Data1<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
    
    Data1$Year<-format(Data1$T,"%Y")
    Data1$Month<-format(Data1$T,"%b")
    
    Data1=filter(Data1,Month %in% c("Jan","Feb","Mar"))
    Data_Cum1<-Data1%>%
      group_by(X,Y,Year)%>%
      summarise(Cum=sum(rain,na.rm=T))
    
    Data_Cum<-rbind( Data_Cum, Data_Cum1)
  }
}




Annual_Total<-Data_Cum%>%
  group_by(X,Y)%>%
  summarise(Mean=mean(Cum,na.rm=T))

#rio::export(Annual_Total,"Data/Annual_Total_Mean_JFM.csv")


#################################################################

########################### Maps ###############################

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
Data_Source="CHIRPS"
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40



Raster_file<-rasterFromXYZ(Annual_Total[c("X","Y","Mean")])

Raster_file_1=disaggregate(Raster_file,8,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

Annual_Total <- as.data.frame(rasterToPoints(rr ))
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
mybreaks <- c(-Inf,100,400,600,1200,1500,Inf)

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

l<-ggplot()+geom_contour_filled(data=Annual_Total, aes(x,y,z =Mean),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))

last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/climatologie/Afrique/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/climatologie/Afrique/","clim_23.jpeg",sep=""),
     width = 14,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()
