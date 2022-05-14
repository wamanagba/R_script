
rm(list=ls())

setwd("C:/Users/Yacou/Desktop/Flood_Risk")

############ Load necessary libraries
suppressPackageStartupMessages({
  library(sp)
  library(rgdal)
  #library(geosphere)
  library(ncdf4)
  library(lubridate)
  library(stringr)
  #library(tidyr)
  #library(ggplot2)
  #library(caTools)
  #library(RMAWGEN)
  #library(signal)
  #library(forecast)
  #library(futureheatwaves)
  #library(RmarineHeatWaves)
  #library(plyr)
  #library(dplyr)
  #library(vegan)
  #require(IRanges)
  #library(weathermetrics)
  #library(FactoMineR)
  #library(missMDA)
  #library(TTR)
  #library(pspline)
  #library(sfsmisc)
  library(raster)
  #library(rasterVis)
  #library(verification)
  #library(SpecsVerification)
  #library(s2dverification)
  #library(easyVerification)
  #library(PRROC)
  #library(SpatialVx)
  #library(extRemes)
  #library(gridExtra)
  #library("readxl")
})
set_Polypath(FALSE)

### Data directories and some parameters and functions

#basepath="/research/geog/data1/kg312/RCCC/Sierra_Leone_floods/" 

basepath="Data/"
#-----------------------------------------------------------------------------------------------------------------------------------------


### Shapefile

shp=readOGR("Data\\NGA_adm\\NGA_adm2.shp")
#shp=shapefile("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/TAMSAT/BFA_adm1.shp")

LZ_names=paste0("LZ",sprintf("%02d", 1:length(shp$NAME_2)))


#first import all files in a single folder as a list 
list <- list.files(path = "Data/", pattern='.tif$', all.files=TRUE, full.names=FALSE)
S= stack(paste0(basepath,cpt) )
#### Read Precip data
d=data.frame()
for(cpt in list){
  print(cpt)
  S= stack(paste0(basepath,cpt) )
  A=as.data.frame(t(extract(S,shp,mean,na.rm=T)))
  d=rbind(A,d)
  
}

togo=d
A=d
colnames(A)= as.vector(LZ_names)

ddd=data.frame()

for (cpt in 2000:2017){
  if (cpt!=2004){
    ddd=rbind(ddd,def(cpt,52))
  }
  else{
    ddd=rbind(ddd,def2(cpt,34))
  }
}

A=cbind(ddd,A)


colnames(A)[1:775]=shp$NAME_2

# Cumule saisonnier

seas= ifelse(A$week %in% 26:39,1,0)
seas_clim = aggregate (A[-c(1:2)], list(seas,A$year),mean, na.rm=T)
seas_clim=seas_clim[seq(2,nrow(seas_clim),2),-1]
#colnames(seas_clim)[2:ncol(seas_clim)]=shp$NAME_2
colnames(seas_clim)[1]=c("year")




# Exportation
library(here)
write.csv(seas_clim, here::here("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\NOAA_NDVI\\output","NDVI.csv"),row.names = FALSE )
save(togo,file="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/NOAA_NDVI/output/NDVI.Rdata")



print("Well done")
