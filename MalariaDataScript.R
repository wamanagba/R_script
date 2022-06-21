
setwd("C:\\Users\\Yacou\\Desktop\\ACMAD_Git/")

rm(list = ls())

Data_source="NCEP-NCAR"

# Humidité Relative mensuelle
var="HR"
Month="May"
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Intrinsic/.PressureLevel/.rhum/P/(1000)/VALUES/Y/-40/2/40/GRID/X/-20/2/55/GRID/T/(",Month,"%20",Year,")/VALUES/data.nc")
dir.create(paste("Data/",Data_source,"/CSV",sep = ""),recursive = T,showWarnings = F)

dir.create(paste(Data_source,sep = ""),recursive = T,showWarnings = F)
download.file(link_HR,mode = "wb",paste(Data_source,"/",Name,".nc",sep = ""))
Data<-tidync(paste(Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))

#Température mensuelle en surface 
var="TempM"
Month="May"
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
link_HR=paste("http://iridl.ldeo.columbia.edu/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.surface/.temp/(Celsius_scale)/unitconvert/Y/-40/2/40/GRID/X/-20/2/55/GRID/T/(",Month,"%20",Year,")/VALUES/data.nc")

dir.create(paste(Data_source,sep = ""),recursive = T,showWarnings = F)
download.file(link_HR,mode = "wb",paste(Data_source,"/",Name,".nc",sep = ""))

Data<-tidync(paste(Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)

rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))



# MONTHLY PRECIPITATION

var="Prep"
Month="May"
statDay=1
endday=31
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.RFEv2/.est_prcp/T/(",statDay,"%20",Month,"%20",Year,")/(",endday,"%20",Month,"%20",Year,")/RANGEEDGES/Y/-40/0.5/40/GRID/X/-20/0.5/55/GRID/T/SUM/data.nc")

dir.create(paste(Data_source,sep = ""),recursive = T,showWarnings = F)
download.file(link_HR,mode = "wb",paste(Data_source,"/",Name,".nc",sep = ""))

Data<-tidync(paste(Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))


# MONTHLY PRECIPITATION ANOMALY

var="PrepAnomaly"
Month="May"
statDay=1
endday=31
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.anomaly/.prcp/T/(days%20since%201960-01-01)/streamgridunitconvert/T/differential_mul/T/(",Month,"%20",Year,")/(",Month,"%20",Year,")/RANGEEDGES/Y/(40S)/(40N)/RANGEEDGES/X/(20W)/(55E)/RANGEEDGES/data.nc")

dir.create(paste(Data_source,sep = ""),recursive = T,showWarnings = F)
download.file(link_HR,mode = "wb",paste(Data_source,"/",Name,".nc",sep = ""))

Data<-tidync(paste(Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))


#Temperature de surface hebdomadaire

var="surface_temp "
Month="May"
statDay=1
endday=31
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Diagnostic/.surface/.temp/(Celsius_scale)/unitconvert/X/(20W)/(55E)/RANGEEDGES/T/(",statDay,"%20",Month,"%20",Year,")/(",endday,"%20",Month,"%20",Year,")/RANGEEDGES/Y/(40N)/(40S)/RANGEEDGES/%5BT%5Daverage/data.nc")

dir.create(paste(Data_source,sep = ""),recursive = T,showWarnings = F)
download.file(link_HR,mode = "wb",paste(Data_source,"/",Name,".nc",sep = ""))

Data<-tidync(paste(Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))



#Vent Méridien hebdo


var="meridional_wind "
Month="May"
statDay=29
statmonth="May"
endday=5
endMonth="Jun"
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.v/X/(40W)/(72E)/RANGEEDGES/T/(",statDay,"%20",statmonth,"%20",Year,")/(",endday,"%20",endMonth,"%20",Year,")/RANGEEDGES/Y/(50N)/(50S)/RANGEEDGES/P/(1000)/VALUES/%5BT%5Daverage/data.nc")

dir.create(paste(Data_source,sep = ""),recursive = T,showWarnings = F)
download.file(link_HR,mode = "wb",paste(Data_source,"/",Name,".nc",sep = ""))

Data<-tidync(paste(Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))







#Humidité Relative hebdo


var="Humidité_Relative_hebdo "
Month="May"
statDay=29
statmonth="May"
endday=5
endMonth="Jun"
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.rhum/T/(",statDay,"%20",statmonth,"%20",Year,")/(",endday,"%20",endMonth,"%20",Year,")/RANGEEDGES/Y/(50N)/(50S)/RANGEEDGES/P/(1000)/VALUES/X/(40W)/(72E)/RANGEEDGES/%5BT%5Daverage/data.nc")

dir.create(paste(Data_source,sep = ""),recursive = T,showWarnings = F)
download.file(link_HR,mode = "wb",paste(Data_source,"/",Name,".nc",sep = ""))

Data<-tidync(paste(Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))





