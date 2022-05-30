library(ncdf4)
library(dplyr)
library(rio)
rm(list = ls())

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
Data_Source="CHIRPS"
setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")

Season=c("Jan","Feb","Mar")

Seas="JFM"
i=1981

for (i in 1981:1983) {
  print(i);
  print(Season)
  Data<-rio::import(paste("Data/CHIRPS/CSV_Format/",i,".csv",sep=""))
  Data$Year<-format(Data$T,"%Y")
  Data$Month1=(format(Data$T,"%b"))
  #Data$prcp[is.na(Data$prcp)]=0
  Data=filter(Data,Month1 %in% Season & prcp>=2.5)
  
  if(i==1981){
    Annual<-Data%>%
    group_by(X,Y,Year)%>%
    summarise(Annual_Total=sum(prcp,na.rm = T))
  }else{
    Annual_1<-Data%>%
      group_by(X,Y,Year)%>%
      summarise(Annual_Total=sum(prcp,na.rm = T))
    Annual<-rbind(Annual,Annual_1)
  }
}

Climatology<-Annual%>%
  group_by(X,Y)%>%
  summarise(Mean=mean(Annual_Total))


#Data=filter(Data,binary==1)
#Data$clim= Data$clim*Data$binary

#Raster_file<-rasterFromXYZ(Climatology[c("X","Y","Mean")])

#Raster_file<-raster::raster("Data/CHIRPS/Cum_1981.nc")
Raster_file<-raster::raster("Data/CPC-UNIFIED/1981.nc")
#x11();plot(Raster_file)
Raster_file_1=disaggregate(Raster_file,4,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

Data <- as.data.frame(rasterToPoints(rr))

#Data$Mean<-ifelse(Data$Mean<=150,NA,Data$Mean)
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
mybreaks <- c(-Inf,100,200,400,800,1000,1200,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("gray","#f0ff00","#ffce00","#ff9a00","#ff5a00","#ff0000","darkred"))(7)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<-as.character(c(0,100,300,400,800,1000,1200))
  #labels<- as.character(seq(1,5))
  labels[1:x]
}
################################################################################

Title<-paste("Average number of Dry spell over 10 days","\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z=precipitation),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(7), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))

last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/climatologie/Afrique/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/climatologie/Afrique/Test_a",Data_Source,".jpeg",sep=""),
     width = 14,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()



