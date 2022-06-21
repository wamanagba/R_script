
MapsFunct=function(Data,
                   Title,
                   legend,
                   source,
                   Names,
                   var){
  
  
  
  Raster_file<-rasterFromXYZ(Data[c("X","Y",var)])
  
  Raster_file_1=disaggregate(Raster_file,6,method='bilinear')
  
  rr = raster::mask(Raster_file_1 ,Africa)
  
  Data <- as.data.frame(rasterToPoints(rr ))
  
  mybreaks <- legend#c(10,20,50,70,90,100,Inf)
  
  #Function to return the desired number of colors
  
  mycolors<- function(x) {
    colors<-colorRampPalette(c("#8cb02c","#37fdf8","blue","#89522a","black","red"))(6)
    colors[1:x]
  }
  
  #Function to create labels for legend
  
  breaklabel <- function(x){
    labels<- as.character(c(10,20,50,70,90,100))
    labels[1:x]
  }
  ################################################################################
  
  #Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_source,"\n Season:",season,sep="")
  Title<-toupper(paste("Number of dry spell over 10 Days ","\nRef: 1981-2010 from ",season,"\nData Source: ",Data_source,sep=""))
  
  #Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)
  
  l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =rhum),breaks= mybreaks, show.legend = TRUE) +
    scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
  l
  last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
  
  #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
  
  last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
  
  last<-last+labs(title = Title,x="",y="")
  dir.create(paste("Products/malaria/",sep=""),recursive = T,showWarnings = F)
  
  jpeg(filename = paste("Products/malaria/",season,"HR.jpeg",sep=""),
       width = 14,
       height =14,
       units = "in",
       res=300)
  print(last)
  dev.off()
}