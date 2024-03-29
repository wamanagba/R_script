



library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())

Data_Source="CPC-UNIFIED"

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40


  cpt=5
  k=2022
  for (cpt in 1:5) {
    


  
  A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
  
  month_stady=paste(A[cpt],sep = "")
  
  dd=data.frame()
  
  
  
    
  Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
  Data2=rio::import(paste("Data/CPC-UNIFIED/Number_RainDays/Month2_5/",month_stady,".csv"))
  Data$Month=format(Data$T,"%b")
  Data$rain[is.na(Data$rain)]=0
  Data= filter(Data,Month %in% A[cpt])
  Data$count=ifelse(Data$rain<2.5,0,1)
    
  NumberDay=Data%>%
    group_by(X,Y)%>%
    summarise(Numb=sum(count))
    
  Data_F=merge(Data2,NumberDay,by=c("X","Y"))
  Data_F$anomaly=Data_F$Numb-Data_F$Number
  dir.create(paste("Data/CPC-UNIFIED/Number_RainDays/Month2_5/Anomaly/",sep = ""),recursive = T,showWarnings = F)
  rio::export(Data_F,paste("Data/CPC-UNIFIED/Number_RainDays/Month2_5/Anomaly/",k,"_",month_stady,".csv"))
  
  
  Data_F=filter(Data_F,Number>=1)
  
  Raster_file<-rasterFromXYZ(Data_F[c("X","Y","anomaly")])
  
  Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
  
  rr = raster::mask(Raster_file_1 ,Africa)
  
  Data_F <- as.data.frame(rasterToPoints(rr ))
  #rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
  mybreaks <- c(-13,-8,-4,0,4,8,12,Inf)
  
  #Function to return the desired number of colors
  
  mycolors<- function(x) {
    colors<-colorRampPalette(c("#FF0000" ,"#FFDB00", "darkviolet", "#00FF92", "#0092FF", "#4900FF" ,"#FF00DB"))(7)
    colors[1:x]
  }
  
  #Function to create labels for legend
  
  breaklabel <- function(x){
    labels<- as.character(c("[-14,-8[","[-8,-4[","[-4,0[","[0,4[","[4,8[","[8,12[","[12,Inf["))
    labels[1:x]
  }
  ################################################################################
  
  #Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")
  Title<-toupper(paste("Anomaly of Number of rainy days from ",month_stady,"\nRef: 1981-2010","\nData Source: ",Data_Source,sep=""))
  
  #Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)
  
  l<-ggplot()+geom_contour_filled(data=Data_F, aes(x,y,z =anomaly),breaks= mybreaks, show.legend = TRUE) +
    scale_fill_manual(palette=mycolors, values=breaklabel(7), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
  
  last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
  
  #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
  
  last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
  
  last<-last+labs(title = Title,x="",y="")
  dir.create(paste("Products/Number_Rain_Day/Month2_5/Anomaly/",sep=""),recursive = T,showWarnings = F)
  
  jpeg(filename = paste("Products/Number_Rain_Day/Month2_5/Anomaly/",k,"_",month_stady,".jpeg",sep=""),
       width = 14,
       height =14,
       units = "in",
       res=300)
  print(last)
  dev.off()

  }
