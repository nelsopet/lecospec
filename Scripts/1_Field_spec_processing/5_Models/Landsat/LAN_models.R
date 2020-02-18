#######################Creates models from field spec data resapmepled on landsat bandpasses######################################
library(tidyverse)
library(randomForest)
library(randomForestExplainer)

###Reads in all predictors for scans
Lichen_groups_LAN_VIs<-read.csv("Outputs/1_Field_spec/1_Processing/Landsat_data/Lichen_groups_LAN_VIs.csv")

##we'll need to apply a function to all dataframes that omits unwanted metadata
Lichen_color_l8<-Lichen_groups_LAN_VIs%>%dplyr::select(color_l8,NDVI,NDWI,NDMI,NDSI,NBR,EVI)
Lichen_color_l4<-Lichen_groups_LAN_VIs%>%dplyr::select(color_l4,NDVI,NDWI,NDMI,NDSI,NBR,EVI)
Lichen_color_l3<-Lichen_groups_LAN_VIs%>%dplyr::select(color_l3,NDVI,NDWI,NDMI,NDSI,NBR,EVI)

set.seed(2017)
##Lets run some different models so we can asses whiich ones are the best for prediction later
rf_LAN_VIsl8   <-randomForest(color_l8~.,data=Lichen_color_l8       ,mtry=sqrt(ncol(Lichen_color_l8        )),ntree=1001,localImp = TRUE)
rf_LAN_VIsl4   <-randomForest(color_l4~.,data=Lichen_color_l4       ,mtry=sqrt(ncol(Lichen_color_l4        )),ntree=1001,localImp = TRUE)
rf_LAN_VIsl3   <-randomForest(color_l3~.,data=Lichen_color_l3       ,mtry=sqrt(ncol(Lichen_color_l3        )),ntree=1001,localImp = TRUE)

##Lets create a data frame that will combine the class.error of all the categories of each model
rf_LAN_VIs_confusionl8   <-rf_LAN_VIsl8$confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_LAN_VIsl8 =class.error)
rf_LAN_VIs_confusionl4   <-rf_LAN_VIsl4$confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_LAN_VIsl4 =class.error)
rf_LAN_VIs_confusionl3   <-rf_LAN_VIsl3$confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_LAN_VIsl3 =class.error)


write.csv(rf_LAN_VIs_confusionl8,"Outputs/1_Field_spec/2_Models/Landsat/rf_LAN_VIsl8_confusion.csv")
write.csv(rf_LAN_VIs_confusionl4,"Outputs/1_Field_spec/2_Models/Landsat/rf_LAN_VIsl4_confusion.csv")
write.csv(rf_LAN_VIs_confusionl3,"Outputs/1_Field_spec/2_Models/Landsat/rf_LAN_VIsl3_confusion.csv")
