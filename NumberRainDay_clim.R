
library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())

Data_Source="CPC-UNIFIED"
setwd("C:/Users/Yacou/Desktop/ACMAD_Git/")
Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
#Data_Source="CPC-UNIFIED"
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40
cpt=1


for (cpt in 1:12) {
  


cpt1=(cpt+1)%%12
if(cpt1==0) cpt1=12
cpt2=(cpt+2)%%12
if(cpt2==0) cpt2=12
s=cpt
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")

season=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")

# dd=data.frame()
# 
# k=2000

# for (k in 1981:2010) {
#   print(k);
#   print(season)
#   
#   Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
#   Data$Month=format(Data$T,"%b")
#   Data$rain[is.na(Data$rain)]=0
#   Data= filter(Data,Month %in% A[c(cpt,cpt1,cpt2)])
#   
#   Data$count=ifelse(Data$rain<20,0,1)
#   
#   NumberDay=Data%>%
#     group_by(X,Y)%>%
#     summarise(Numbers=sum(count))
#   
#   dd=rbind(dd,NumberDay)
#   
# }
  
# MeanNumberDay=dd%>%
#   group_by(X,Y)%>%
#   summarise(Number=mean(Numbers))
  
# dir.create(paste("Data/CPC-UNIFIED/Number_RainDays/Days20/",sep = ""),recursive = T,showWarnings = F)
# rio::export(MeanNumberDay,paste("Data/CPC-UNIFIED/Number_RainDays/Days20/",season,".csv"))
#   
  
  Data=rio::import(paste("Data/CPC-UNIFIED/Number_RainDays/Days2_5/ ",season," .csv",sep = ""))
  #Data=rio::import(paste("Data/CPC-UNIFIED/Number_RainDays/Days20/ SON .csv",sep = ""))
  Data2=rio::import(paste("Data/CPC-UNIFIED/Mask/mask_ ",season," .csv",sep = ""))
  Data_F=merge(Data,Data2,by=c('X','Y'))
  Data_F=filter(Data_F,mask==1)
  
  Raster_file<-rasterFromXYZ(Data_F[c("X","Y","Number")])
  
  Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
  
  rr = raster::mask(Raster_file_1 ,Africa)
  
  Data_F <- as.data.frame(rasterToPoints(rr ))
  #rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
  mybreaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,Inf)
  
  #Function to return the desired number of colors
  
  mycolors<- function(x) {
    colors<-colorRampPalette(c("#8cb02c","darkviolet","#37fdf8", "blue", "#5E626EFF", "#03051AFF", "#0D0887FF","#F0F921FF",  "#CC4678FF", "#F89441FF" ,"red","darkred" ))(12)
    colors[1:x]
  }
  
  #Function to create labels for legend
  
  breaklabel <- function(x){
    labels<- as.character(c(0,5,10,15,20,25,30,35,40,45,50,55))
    labels[1:x]
  }
  ################################################################################
  
  #Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")
  Title<-toupper(paste("Number of rainy days from ",season,"\nRef: 1981-2010","\nData Source: ",Data_Source,sep=""))
  
  #Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)
  
  l<-ggplot()+geom_contour_filled(data=Data_F, aes(x,y,z =Number),breaks= mybreaks, show.legend = TRUE) +
    scale_fill_manual(palette=mycolors, values=breaklabel(12), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
  
  last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
  
  #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
  
  last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
  
  last<-last+labs(title = Title,x="",y="")
  dir.create(paste("Products/Number_Rain_Day/Day2_5/",sep=""),recursive = T,showWarnings = F)
  
  jpeg(filename = paste("Products/Number_Rain_Day/Day2_5/",s,season,".jpeg",sep=""),
       width = 14,
       height =14,
       units = "in",
       res=300)
  print(last)
  dev.off()
}

