
rm(list = ls())
library(rio)
library(dplyr)
library(ggplot2)
library(ggpubr)
#http://154.66.220.45:8080/thredds/fileServer/ACMAD/CDD/climatedataservice/Synoptic_Daily_CPC_Unified_Data/Niger/ZINDER.csv
setwd("C:/Users/Yacou/Desktop/ACMAD_Git")
#Importation of station list
Station<-rio::import("Synoptic_Station_All.csv")

#Station<-filter(Station,Country %in% c("Burkina","Niger"))


Data_Source="ARC2"

df <- data.frame(COUNTRY = character(),
                 Station = character(),
                 Cumul = double(),
                 stringsAsFactors = FALSE)

find_dryspells <- function(
  x,                    # a numeric vector with precipitation data
  dates,                # vector containing the dates of above values
  threshold = 2.5        # upper threshold
) {
  
  # check if the day was dry
  res_vec <- ifelse(x > threshold, 0, 1)
  
  # eliminate single days
  if(res_vec[1] == 1 & res_vec[2] == 0) res_vec[1] <- 0
  if(res_vec[length(res_vec)] == 1 & 
     res_vec[length(res_vec)-1] == 0) res_vec[length(res_vec)] <- 0
  for (i in 2:(length(res_vec)-1)) {
    if(res_vec[i] & !res_vec[i-1] & !res_vec[i+1]) res_vec[i] <- 0
  }
  res_vec[length(res_vec)+1]=0
  # make human-readable
  tag_vec <- vector()
  tag_vec[1] <- ifelse(res_vec[1] == 1, "start", NA)
  for (i in seq(res_vec)[c(-1, -length(res_vec))]) {
    if(res_vec[i] & !res_vec[i-1]) tag_vec[i] <- "start"
    if(res_vec[i] & !res_vec[i+1]) tag_vec[i] <- "end"
  }
  
  readable <- as.data.frame(
    matrix(
      strftime(dates)[tag_vec %in% c("start", "end")],
      ncol = 2,
      byrow = T
    )
  )
  
  colnames(readable) <- c("start", "end")
  
  readable$length_in_days <- as.integer(
    difftime(
      as.POSIXlt(readable$end),
      as.POSIXlt(readable$start),
      units = "days"
    ) + 1
  )
  readable=filter(readable,length_in_days>0)
  readable=as.data.frame(readable)
  
  return(readable)
}







i=1
for (i in 1:length(Station$Station)) {
  print(i);print(Station$Station[i]);print(Station$Country[i])
  
  
  
  station= Station$Station[i]
  CounTry=Station$Country[i]
  data=rio::import(paste('Data/',Data_Source,"/",CounTry,'/',station,'.csv',sep=""))
  #data$Precipitation= ifelse(data$Precipitation<2.5,0,1)
  data=na.omit(data)
  data$Month<-(format(data$Date,"%b"))
  data$Year = as.numeric(format(data$Date,'%Y'))
  
  #data= filter(data, Year==Y & Month %in% c("jul","Aug","Sep"))
  
  
  
  
  Data=data
  DD=data.frame()
  
  
      
      
    for (Y in seq(1983,2021)) {
        data=Data
        data= filter(data, Year==Y & Month %in% c("jul","Aug","Sep"))
        cumul=sum(data$Precipitation)
        
        if(cumul<100){
          
          print("Cumul of precipitation is inferior to 100 mm")
          Null_sattion=data.frame(CounTry,station,cumul)
          df=rbind(df,Null_sattion)
          
        }else{
    
    cdd=find_dryspells(x=data$Precipitation,dates=data$Date,2.5)
    
    # Create a folder each station
    dir.create(paste('Data/',Data_Source,"/",CounTry,'/Dry_Spell/',Y,sep = ""),recursive = T, showWarnings = F)
    
    rio::export(cdd,paste('Data/',Data_Source,"/",CounTry,'/Dry_Spell/',Y,"/",station,'.csv',sep = ""))
    
    
    DD=rbind(cdd,DD)
    
    
  }}
  
  if(nrow(DD)==0){
    print(paste(station,'is Null'))
  }else{
    
  D1=filter(DD,DD$length_in_days>10)
    
    cdd <- DD %>%
      group_by(length_in_days) %>%
      summarise(counts = n())
    cdd
    
    
    pl=ggplot(cdd, aes(x = length_in_days, y = counts)) +
      geom_bar(fill = "#0073C2FF", stat = "identity") +
      geom_text(aes(label = counts), vjust = -0.3) + 
      ggplot2::labs(title=paste("Nuber of Dry Spell for ",station), subtitle="", caption="") +
      ggplot2::xlab(label="Consecutif Dry Day") +
      ggplot2::theme(axis.title=ggplot2::element_text(face="bold"), axis.text=ggplot2::element_text(face="bold"), axis.title.x=ggplot2::element_text(size=9.0, face="bold"), axis.title.y=ggplot2::element_text(size=9.0), plot.title=ggplot2::element_text(size=11.0, face="bold"), axis.text.y=ggplot2::element_text(size=11, face="bold", colour="black")) + ggplot2::labs(title=paste("Anomaly of Annual Extrem Precipitation for ",station), subtitle="", caption="") +
      ggplot2::ylab(label="Frequence")
    theme_pubclean()
    
    
    dir.create(paste('Products/',Data_Source,"/",CounTry,'/Dry_Spell/',sep = ""),recursive = T, showWarnings = F)
    ggplot2::ggsave(filename=paste('Products/',Data_Source,"/",CounTry,'/Dry_Spell/',station,'.jpeg',sep = ""), width=14,height=8,limitsize = FALSE ,plot=pl)
    
  }
  
}


#data= filter(data, Year==2009 & Month %in% c("jul","Aug","Sep"))



#cdd=find_dryspells(x=data$Precipitation,dates=data$Date,2.5)

#cdd=as.data.frame(find_dryspells(x=data$Precipitation,dates=data$Date,2.5))


# ggplot(DD, aes(length_in_days)) +
#   geom_bar(fill = "#0073C2FF") +
#   theme_pubclean()
# 
# cdd <- DD %>%
#   group_by(length_in_days) %>%
#   summarise(counts = n())
# cdd
# 
# 
# ggplot(cdd, aes(x = length_in_days, y = counts)) +
#   geom_bar(fill = "#0073C2FF", stat = "identity") +
#   geom_text(aes(label = counts), vjust = -0.3) + 
#   ggplot2::labs(title=paste("Nuber of Dry Spell for ",station), subtitle="", caption="") +
#   ggplot2::xlab(label="Consecutif Dry Day") +
#   ggplot2::ylab(label="Frequence")
#   theme_pubclean()
