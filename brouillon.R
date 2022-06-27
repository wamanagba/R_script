setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")
options(download.file.extra = '--no-check-certificate')
options(timeout=600)
options(warn=-1)
rm(list=ls())
#https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.Monthly/.extREALTIME/.rain/
Link_TAMSAT="https://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.v3p1/.daily/.rfe/"
Link_ARC2="https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.ARC2/.daily/.est_prcp/"
Link_CHIRPS="https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp/"
Link_unified='https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/'
Data_Source="NCEP-NCAR"
linkk="http://iridl.ldeo.columbia.edu/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Intrinsic/.MSL/.pressure/"
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40
dir.create(paste(Data_Source,sep=""),recursive = T,showWarnings = F)
#https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/T/(0000%201%20Jan%201982)/(0000%2031%20Dec%201982)/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/data.nc
for (i in 2021:2022) {
  download.file(paste(linkk,"T/(0000%201%20Jan%20",i,")/(0000%2031%20Dec%20",i,")/RANGEEDGES/Y/",MinLat,"/0.5/",MaxLat,"/GRID/X/",MinLon,"/0.5/",MaxLon,"/GRID/data.nc",sep=""),mode="wb",paste(Data_Source,"/",i,".nc",sep=""))
}
