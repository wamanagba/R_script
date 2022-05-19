################################################################################
#Optimal Script for Profiles and Onset_Date
#Ibrahim DAN DIJE, Intern at ACMAD
#05-11-2021
#This script is used for the station tha have the start from Jan to Jul
################################################################################

#########################Libraries##############################################
library(rio)
library(zoo)
library(reshape2)
library(dplyr)
#########################Language###############################################

Sys.setlocale("LC_TIME", 'english')

#########################Delete all environment data############################
rm(list = ls())

#Give the analogue years
Analogues_Years<-c(2001,2009,2012,2018,2021)
#Give two best analogue year
Best_Analogue<-c(2018,2012)
#Give the Current year

Cur_Year=2022
################################################################################
Year_Legend<-c("Mean","75% Mean","125% Mean",Analogues_Years)
#########################Gives the path#########################################

#Specify the working directory
setwd("D:/Onset_Methods/")

#The name of the folder where the data are stored
Data_Folder="Data"

#The name of the folder where the profiles will be saved: The folder will be created automatically

Profile_Folder="Western_Africa"

#Data Source
Source="CPC/Unified"

 
#Giev the file that contains the Station list

Station<-rio::import("Synoptic_Stations_January_Start.csv")


## Make sure that your Station list file contains at least the follwing column
## Station  Country
##
##

i=1
#Additional Array: Don't modify
Date_Legend<-c("")
Color_Legend<-c("")
Analogue_Onset<-c()
Max_Cum<-c()

nbelm=0 

################################################Don't need to modify anything here##################################
for (i in 1:length(Station$Station)){
     print(paste(Station$Country[i],": ",Station$Station[i]))
     Analogue_Onset<-c()
  
    #Read Data
    #Make sure that the columns of your data contain the following at least
    #Station Date Precipitation
    #Data<-rio::import(file=paste(Data_Folder,"/",Station$Station[i],".csv",sep=""))
     if(Station$Data_Source[i]=="CPC_Unified"){
      Data<-rio::import(paste("CPC_Unified/",Station$Station[i],".csv",sep=""))
        
     }
     if(Station$Data_Source[i]=="ARC2"){
        
       Data<-rio::import(paste("ARC2/Data/",Station$Station[i],".csv",sep=""))
       
      }
     
     Data<-Data %>% distinct(Date, .keep_all = TRUE)
     
     Last_Date<-Data$Date[length(Data$Date)]
     
     istation<-Station$Station[i]
     
     Data$year<-format(Data$Date,format="%Y")
     
     Data$month<-format(Data$Date,format="%m")
     
     Data$day<-format(Data$Date,format="%d")
     
     Data$Precipitation<-replace(Data$Precipitation,is.na(Data$Precipitation),0)
     Data$Precipitation<-replace(Data$Precipitation,Data$Precipitation>300,0)
     
     dd.instat <- round(acast(Data,  month+day~year, value.var="Precipitation"),2)
     
     #Condition
     Instat <- dd.instat
     #Cumulative sum of the daily mean
     Clim.Mean <- rowMeans(Instat, na.rm=T)
     Clim.Cumsum <- cumsum(Clim.Mean)
     #75% cumulative sum
     Clim.Cumsum.75 <- Clim.Cumsum*0.75
     
     #125% cumulative sum
     Clim.Cumsum.125<- Clim.Cumsum*1.25
     
     Max_Cum<-c(Clim.Cumsum,Clim.Cumsum.75,Clim.Cumsum.125)
     
     if(max(Clim.Cumsum)>200){
       print("Yes")
       Data_1<-as.data.frame(dd.instat)
       Data_1$Cum_Mean<-Clim.Cumsum
       Data_1$doy<-seq(1,366)
       MeanOnset<-filter(Data_1,Cum_Mean>=50)
       Start_Cum<-MeanOnset$doy[1]-30
       
       
       Date_Mean<-format(as.Date(MeanOnset$doy[1],origin="2016-12-31"),"%d-%b")
      
       Data_75<-data.frame(Cum= Clim.Cumsum.75)
       Data_75$doy<-seq(1,366)
       Data_75<-filter(Data_75,Cum>=50)
       Onset_75=format(as.Date(Data_75$doy[1],origin="2016-12-31"),"%d-%b")
       
       Data_125<-data.frame(Cum=Clim.Cumsum.125)
       Data_125$doy<-seq(1,366)
       Data_125<-filter(Data_125,Cum>=50)
       Onset_125=format(as.Date(Data_125$doy[1],origin="2016-12-31"),"%d-%b")
       Date_Legend<-c(Date_Mean,Onset_125,Onset_75)
       if(Start_Cum<0){
         Start_Cum<-0
       }
       
       if(Start_Cum>=0){
         Instat[1:Start_Cum,]=0
         
         for (Year in Analogues_Years) {
           
             Cum_Year<-cumsum(na.fill(Instat[,as.character(Year)],0))
               
             Onset_Year<-data.frame(Cum=Cum_Year)
             Onset_Year$doy<-seq(1,366)
             Onset_Year<-filter(Onset_Year,Cum>=50)
             Onset_Year_Date=format(as.Date(Onset_Year$doy[1],origin="2016-12-31"),"%d-%b")
             Onset_Year_Date_Numeric<-Onset_Year$doy[1]
             #length(Date_Legend)
             Date_Legend[length(Date_Legend)+1]<-Onset_Year_Date
             Analogue_Onset[length(Analogue_Onset)+1]<-Onset_Year_Date_Numeric
             Max_Cum[length(Max_Cum)+1]<-max(Cum_Year)
         }
       }
       #Current Year
       l<-length(seq(as.Date(paste(Cur_Year,"-01-01",sep="")),Last_Date,by="days"))
       Current_Year <- cumsum(na.fill(Instat[1:l+1,as.character(Cur_Year)],0))
       Onset_Year<-data.frame(Cum=Current_Year)
       Onset_Year$doy<-seq(1,length(Onset_Year$Cum))
       Onset_Year<-filter(Onset_Year,Cum>=50)
       Onset_Year_Date_Current=format(as.Date(Onset_Year$doy[1],origin="2016-12-31"),"%d-%b")
       Max_Cum<-c(Max_Cum,Current_Year)
       #Graphics
       dir.create(Profile_Folder,recursive = T,showWarnings = F)
       
       jpeg(filename =paste(Profile_Folder,"/",istation,"_Profile.jpeg",sep=""),
            width = 20,
            height = 10,
            units = "in",
            res=200)
       #Unchanged
       
       plot(Clim.Cumsum.125, lwd=4, col=3, ty="l", axes=F, xlab="Time(day)",
            main=paste( Data$Country[1], ": ", "Cumulative precipitation for",istation,"\nData source:",Station$Data_Source[i],", Last update: ",Last_Date), ylab="Precipitation (mm)",cex.axis=2.0, cex.main=1.3, cex.lab=1.6,ylim = range(0,max(Max_Cum)+100))
      
       lines(Clim.Cumsum.75, lwd=4, col="orange")
        
       lines(Clim.Cumsum, lwd=4, col=4)
       Color_Legend<-c(1,3,"orange",4)
       #Analogue Years
       col=2
       lty=1
       for (Year in Analogues_Years) {
         
         Color_Legend[length(Color_Legend)+1]<-col
         
         Cum_Year<-cumsum(na.fill(Instat[,as.character(Year)],0))
         
         lines(Cum_Year, lwd=3, col=col, lty=lty)
         
         col=col+1
          
         lty=lty+1
       }
    
       
       lines(Current_Year, lwd=5, col=1, lty=1)
       #Threshold
       onset <- rep(50,350)
       lines(onset, lwd=3, lty=2)

       #Legend
       Legend<-c(paste(Cur_Year,": ",Onset_Year_Date_Current,sep=""))
       for(L in 1:length(Date_Legend)){
           a=paste(Year_Legend[L],": ",Date_Legend[L],sep="")
           Legend[length(Legend)+1]=a
          
       }
       
       for (KK in 1:length(Analogue_Onset)) {
         if(KK==1){
           Analogues_Years_Onset<-data.frame(Year=Analogues_Years[KK],Doy=Analogue_Onset[KK])
         }
         else{
           Analogues_Years_Onset1<-data.frame(Year=Analogues_Years[KK],Doy=Analogue_Onset[KK])
           Analogues_Years_Onset<-rbind(Analogues_Years_Onset,Analogues_Years_Onset1)
         }
         
       }
       legend("topleft",Legend, text.col=Color_Legend,title=as.expression(bquote(bold("Year & Onset Date"))))
       
       DayOfYear <- format(as.Date(seq(0,366,15), origin=paste(1983, "01", "01", sep="-"), "%Y-%m-%d"), "%d-%b")
       axis(1, seq(0,366,15), DayOfYear,font=2,cex.axis=1.0)
       s1<-seq(0,100,50)
       s2<-seq(100,max(Max_Cum)+100,100)
       s<-c(s1,s2)
       axis(2, at=s,las=2)
       dev.off()
       if(nbelm==0){
         Onset<-data.frame(Station=Station$Station[i],Lon=Station$longitude[i],Lat=Station$latitude[i],Onset75=Data_75$doy[1],MeanOnset=MeanOnset$doy[1],Onset125=Data_125$doy[1],OnsetCurrentYear=Onset_Year$doy[1],OnsetAnalogue1D=Analogues_Years_Onset[Analogues_Years_Onset$Year==Best_Analogue[1],]$Doy,OnsetAnalogue2D=Analogues_Years_Onset[Analogues_Years_Onset$Year==Best_Analogue[2],]$Doy)
         nbelm=nbelm+1
       }
       else{
         Onset1<-data.frame(Station=Station$Station[i],Lon=Station$longitude[i],Lat=Station$latitude[i],Onset75=Data_75$doy[1],MeanOnset=MeanOnset$doy[1],Onset125=Data_125$doy[1],OnsetCurrentYear=Onset_Year$doy[1],OnsetAnalogue1D=Analogues_Years_Onset[Analogues_Years_Onset$Year==Best_Analogue[1],]$Doy,OnsetAnalogue2D=Analogues_Years_Onset[Analogues_Years_Onset$Year==Best_Analogue[2],]$Doy)
         Onset<-rbind(Onset,Onset1)
       }
     }
}

dir.create("Onset",recursive = T,showWarnings = F)

rio::export(Onset,paste("Onset/Onset_Date_CPC_UNIFIED_",gsub("-","",Sys.Date()),".csv",sep=""))
