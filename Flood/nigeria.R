library(cowplot)
library(tidyverse)
library(broom)
library(ggpubr)
library(rgdal)
library(dplyr)
rm(list = ls())
setwd("C:/Users/Yacou/Desktop/Flood_Risk/")
NHSBoards<-readOGR("Data/NGA_adm/NGA_adm1.shp")
#NHSBoards <- readOGR("D:\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp",use_iconv = TRUE,encoding = "UTF-8")
basepath= 'Data'
#Data$model
Data= rio::import("Data/Vulnerability.csv")
Data=Data[order(Data$`DATA$COUNTRY`), ]



  #d1=t(Data)
  #d1=round(d1,digits = 2)
  #d1=replace(d1, d1<=0, 0)
NHSBoards_tidy <- tidy(NHSBoards)
NHSBoards$id <- row.names(NHSBoards)
NHSBoards_tidy <- left_join(NHSBoards_tidy, NHSBoards@data)

Data_State<-NHSBoards_tidy[c("long","lat","NAME_1")]

names(Data_State)[3]="State"

names(Data)[1]="State"

Data_State<-full_join(Data_State,Data,by=c("State"))

rio::export(Data_State,"Data/vulnerability_index.csv")


Data2=dplyr::select(Data,long, lat)

  
  dr=as.data.frame(NHSBoards@data)
  hospitalsSco <- data.frame(NAME_2  = sort(dr$NAME_2),
                             indice = as.numeric(d1))
  NHSBoards_tidy <- left_join(NHSBoards_tidy, hospitalsSco)
  
  
  HBLabel <- NHSBoards_tidy %>%
    group_by(NAME_2) %>%
    summarise(label_long = mean(range(long)), label_lat = mean(range(lat)), score = mean(score))
  
  map <- ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group, fill = score)) +
    geom_polygon(color = "black", size = 0.1) +
    coord_equal() +
    theme_void() +
    #theme(legend.position = "none")+
    guides(color = guide_legend(override.aes = list(size = 10)))+
    labs(title = model) +
    theme(plot.title = element_text(margin = margin(t = 40, b = -40)))
  
  
  map=map +geom_text(data = HBLabel, mapping = aes(x = label_long, y = label_lat, label = score, group = NA)
                     , cex = 0, col = "blue")



setwd(basepath)

x11();RidgeCV
x11();RandomForest

RidgeCV=score('RidgeCV')
Lasso=score('Lasso')
RandomForest=score('RandomForest')
DecisionTree=score('DecisionTree')

png("map4_mil.png")
map=plot_grid(RidgeCV,  RandomForest,Lasso,DecisionTree,
          label_size = 1)
x11();map

dev.off()

