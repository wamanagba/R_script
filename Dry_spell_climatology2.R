library(ncdf4)
library(dplyr)
library(rio)
library(rgdal)
library(ggplot2)
rm(list = ls())

Data_Source="CPC-UNIFIED"

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
#Data_Source="CPC-UNIFIED"
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40


setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")
source("C:/Users/Yacou/Desktop/ACMAD_Git/Script/Dry_spell_3.R")

k=2000
cpt=7
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
for(cpt in 10:12){
  cpt1=(cpt+1)%%12
  if(cpt1==0) cpt1=12
  cpt2=(cpt+2)%%12
  if(cpt2==0) cpt2=12
  season=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")
  
dd=data.frame()

for (k in 1981:2010) {
  print(k);print(season)

    Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
    Data$Month=(format(Data$T,"%b"))
    Data$rain[is.na(Data$rain)]=0
    Data= filter(Data,Month %in% A[c(cpt,cpt1,cpt2)])

    
    Data_Dry_Speel<-Data%>%
      group_by(X,Y)%>%
      summarise(Spell=CDD_function(rain,2.5,Nb=10))
    
    dd<-rbind(dd,Data_Dry_Speel)  
    
}
  

Data_Dry<-dd%>%
  group_by(X,Y)%>%
  summarise(Mean=mean(Spell))

Mask=rio::import(paste("Data/CPC-UNIFIED/Mask/mask_ ",season," .csv",sep = ""))
Mask=subset(Mask, select = -c(Mean) )

Data_F=merge(Data_Dry,Mask,by=c('X','Y'))






dir.create(paste("Data/Dry_Speel_Data/Climatology/mean_speel/",sep = ""),recursive = T, showWarnings = F)
rio::export(Data_F,paste("Data/Dry_Speel_Data/Climatology/mean_speel/1981_2010_",season,".csv",sep = ""))

# cpt=10 commencer par cpt egale 10 la prochaine fois




 # Create a Maps
# 
# 
# Data= filter(Data_F,mask==1)
# 
# Data_F=Data
# 
# Raster_file<-rasterFromXYZ(Data_F[c("X","Y","Mean")])
# 
# Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
# 
# rr = raster::mask(Raster_file_1 ,Africa)
# 
# Data_F <- as.data.frame(rasterToPoints(rr ))
# #rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
# mybreaks <- c(0,1,2,3,4,5,Inf)
# 
# #Function to return the desired number of colors
# 
# mycolors<- function(x) {
#   colors<-colorRampPalette(c("#8cb02c","blue","#37fdf8","darkviolet","red","#89522a"))(6)
#   colors[1:x]
# }
# 
# #Function to create labels for legend
# 
# breaklabel <- function(x){
#   labels<- as.character(c(0,1,2,3,4,5))
#   labels[1:x]
# }
# ################################################################################
# 
# #Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")
# Title<-toupper(paste("Average of dry spell ","\nRef: 1981-2010 from ",season,"\nData Source: ",Data_Source,sep=""))
# 
# #Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)
# 
# l<-ggplot()+geom_contour_filled(data=Data_F, aes(x,y,z =Mean),breaks= mybreaks, show.legend = TRUE) +
#   scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
# 
# last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
# 
# #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
# 
# last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
# 
# last<-last+labs(title = Title,x="",y="")
# dir.create(paste("Products/Dry_speel/Climatology2/mean_speel/",sep=""),recursive = T,showWarnings = F)
# 
# jpeg(filename = paste("Products/Dry_speel/Climatology2/mean_speel/",season,".jpeg",sep=""),
#      width = 14,
#      height =14,
#      units = "in",
#      res=300)
# print(last)
# dev.off()
# 


}
