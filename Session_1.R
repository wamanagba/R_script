library(rio)
library(dplyr)
library(ggplot2)
setwd("C:/Users/HP/Desktop/Training")

#importation of the Data Set
Niamey<-rio::import("Data/NIAMEY-AERO.csv")

#Convert column Date in the Date format

Niamey$Date<-as.Date(Niamey$Date)

#Split Year+Month+Day_In_Month

Niamey$Year<-as.numeric(format(Niamey$Date,"%Y"))

Niamey$Month<-(format(Niamey$Date,"%b"))

Niamey$Day_In_Month<-as.numeric(format(Niamey$Date,"%d"))


Niamey_By_Year<-Niamey%>%
  group_by(Station,Year)%>%
  summarise(Annual_Total=sum(Precipitation,na.rm = T))
  
#Export Data Set
rio::export(Niamey_By_Year,"Products/Annual_Total.xlsx")

#Compute the Mean

Niamey_Mean<-filter(Niamey_By_Year,Year<2022)%>%
  group_by(Station)%>%
  summarise(Mean=mean(Annual_Total))

#Merge two Data Frame

Niamey_By_Year<-merge(Niamey_By_Year,Niamey_Mean,by=c("Station"))

#Anomaly=Annual_Total-Mean ()

Niamey_By_Year$Anomaly<-Niamey_By_Year$Annual_Total- Niamey_By_Year$Mean
  
Niamey_By_Year$AnoPosit<-ifelse(Niamey_By_Year$Anomaly<0,0,Niamey_By_Year$Anomaly)

Niamey_By_Year$AnoNeg<-ifelse(Niamey_By_Year$Anomaly>0,0,Niamey_By_Year$Anomaly)

Niamey_By_Year<-filter(Niamey_By_Year,Year<2022)

last_graph <- ggplot2::ggplot(data=Niamey_By_Year, mapping=ggplot2::aes(x=Year, y=Anomaly)) + ggplot2::geom_bar(position="dodge", stat="identity", fill="red") + ggplot2::geom_bar(mapping=ggplot2::aes(y=AnoPosit), fill="blue", stat="identity", position="dodge") + ggplot2::geom_smooth(mapping=ggplot2::aes(y=Anomaly), method="lm", formula="y ~ x", colour="black", se=FALSE) + theme_grey() + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, size=20, colour="black"), axis.title=ggplot2::element_text(face="bold"), axis.text=ggplot2::element_text(face="bold"), axis.title.x=ggplot2::element_text(size=15.0, face="bold"), axis.title.y=ggplot2::element_text(size=15.0), plot.title=ggplot2::element_text(size=20.0, face="bold"), axis.text.y=ggplot2::element_text(size=20, face="bold", colour="black")) + ggplot2::labs(title=paste("Anomaly of Annual Total Rainfall for Niamey"), subtitle="", caption="") + ggplot2::xlab(label="Years") + ggplot2::scale_x_continuous(breaks=seq(by=1, to=2021, from=1980)) + ggplot2::ylab(label="Rainfall Anomaly (mm)") + ggplot2::scale_y_continuous(breaks=seq(by=40, to=800, from=-800))

ggplot2::ggsave(filename=paste("Products/Anomaly_Niamey.jpeg",sep=""), width=14,height=8,limitsize = FALSE ,plot=last_graph)

summary(lm(Niamey_By_Year$Year~Niamey_By_Year$Anomaly))
