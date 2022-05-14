library(raster)
rm(list = ls())

options(download.file.extra = '--no-check-certificate')
options(timeout=600)
options(warn=-1)
rm(list=ls())
#Climatology
#https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.Monthly/.extREALTIME/.rain/T/(Jan%202021)/(Sep%202021)/RANGEEDGES/Y/-40/0.5/40/GRID/X/55/0.5/-25/GRID/T/9/runningAverage/270/mul/ngridtable+table-+skipanyNaN+3+-table+.html
#############################################################################################################################################################################
setwd("C:/Users/Yacou/Desktop/Flood_Risk/")

#Africa<-readOGR("Data\\Kaduna_Shapefile\\Kaduna_Shape_file_Adm2.Africa") 
Africa<-readOGR("Data/NGA_adm/NGA_adm1.shp") 


LZ_names=paste0("LZ",sprintf("%02d", 1:length(Africa$NAME_1)))


X=as.data.frame(matrix(ncol=6, nrow=37))
colnames(X)=c("Hazard","Pop_size","exposure", "vulnerability", "coping_capacity","Risk")


##Importation des donnees de population

Pop<-raster::raster("Population/nga_pop.grd")
Pop=as.data.frame(t(extract(Pop,Africa,mean,na.rm=T)))
Pop=Pop[-36]

a=Pop

for (i in 1:length(Pop)) {
  Pop[i]=(a[i]-min(a))/(max(a)-min(a))
  
}
X$Pop_size=c(t(Pop))

#a=cbind(state=,Africa$NAME_1 ,size=Pop)


Data=rio::import("C:\\Users\\Yacou\\Desktop\\Flood_Risk\\Hazard\\Hazard_95th.csv")[1:3]

Data=rasterFromXYZ(Data)

Data1=as.data.frame(t(extract(Data,Africa,mean,na.rm=T)))
Data1=Data1[-36]

a=Data1
for (i in 1:length(Data1)) {
  Data1[i]=(a[i]-min(a))/(max(a)-min(a))
}

X$Hazard=c(t(Data1))

#importation des donnees de vulnerabilite
vul= rio::import("Data/Vulneable.csv")
X$vulnerability=vul$Socio_Economic_Vulnerability
X$coping_capacity=vul$LCC


X$exposure= (X$Hazard+X$Pop_size)/2
X$exposure=(X$exposure-min(X$exposure))/(max(X$exposure)-min(X$exposure))

X$vulnerability=(X$vulnerability-min(X$vulnerability))/(max(X$vulnerability)-min(X$vulnerability))

X$coping_capacity=(X$coping_capacity-min(X$coping_capacity))/(max(X$coping_capacity)-min(X$coping_capacity))


X$Risk=(X$exposure+X$vulnerability+X$coping_capacity)/3

X$Risk=(X$Risk-min(X$Risk))/(max(X$Risk)-min(X$Risk))


X[38,]=NA

XXX=rbind(X[1:35,], X[38,], X[36:37,])

breakpoints=seq(0,1,0.1)
cols=colorRampPalette(c("lightgreen", "yellow","orange","red", "darkred"))(length(breakpoints)-1)

x=cut(c(t(XXX$exposure)),breaks= breakpoints) ; levels(x)=cols 


Africa[["exposure"]]=as.vector(x)

png(paste0("Products/Exposureccc.png"), height=800, width=1200, type="cairo")

plot(Africa, col=Africa$exposure,main="Hazard exposure",cex=3,cex.main=3,pch=10)

legend("toprigh",text.col=cols,legend =c("[0-0.1]","[0.1-0.2]","[0.2-0.3]","[0.3-0.4]","[0.4-0.5]","[0.5-0.6]","[0.6-0.7]","[0.7-0.8]","[0.8-0.9]","[0.9-1]"))
dev.off()

# Vulnerability

x=cut(c(t(XXX$vulnerability)),breakpoints) ; levels(x)=cols 
Africa[["vulnerability"]]=as.vector(x)

png(paste0("Products/Vulnerability.png"), height=800, width=1200, type="cairo")
plot(Africa, col=Africa$vulnerability,main="vulnerability over Nigeria",cex=3,cex.main=3)
dev.off()

#Lack of coping capacity

x=cut(c(t(XXX$coping_capacity)),breakpoints) ; levels(x)=cols 
Africa[["coping_capacity"]]=as.vector(x)

png(paste0("Products/coping_capacity.png"), height=800, width=1200, type="cairo")
plot(Africa, col=Africa$coping_capacity,main="Lack of coping capacity over Nigeria",cex=3,cex.main=3)
dev.off()


#Risk

x=cut(c(t(XXX$Risk)),breakpoints) ; levels(x)=cols 
Africa[["Risk"]]=as.vector(x)

png(paste0("Products/Risk.png"), height=800, width=1200, type="cairo")
plot(Africa, col=Africa$Risk,main="Risk over Nigeria",cex=3,cex.main=3)
dev.off()

#x11();plot(Pop_Mask)

Pop_Mask<-raster::mask(Pop,Africa)

Data_frame_Pop<-as.data.frame(rasterToPoints(Pop_Mask))

plot()

##Importation des donnees de Hazard
##Comme c'est un fichier text on le converti en raster

Hazard<-rio::import("Data/Hazard_95th.csv")

Raster_file<-raster
FromXYZ(Hazard[c("X","Y","Normalization")])

Raster_file_1=disaggregate(Raster_file,15,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

Data_Hasard<- resample(rr,Pop_Mask, method = "bilinear")


Data_Hasard_df <- as.data.frame(rasterToPoints(Data_Hasard))


Hasard_Exposure<-merge(Data_Hasard_df,Data_frame_Pop,by=c("x","y"))


Hasard_Exposure$HE<-sqrt(Hasard_Exposure$Normalization* Hasard_Exposure$nga_pop)

rio::export(Hasard_Exposure,"Data/Hasard_Exposure.csv")


Africa<-readOGR("Data\\Kaduna_Shapefile\\Kaduna_Shape_file_Adm2.Africa") 

Kaduna<-tidy(Africa)

Africa$id <- row.names(Africa)

Kaduna <- left_join(Kaduna, Africa@data)
