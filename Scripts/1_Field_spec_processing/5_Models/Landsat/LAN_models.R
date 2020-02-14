#######################Creates models from field spec data resapmepled on landsat bandpasses######################################
library(tidyverse)
library(randomForest)
library(randomForestExplainer)

###Reads in all predictors for scans
Lichen_groups_LAN_VIs<-read.csv("Outputs/1_Field_spec/1_Processing/Landsat_data/Lichen_groups_LAN_VIs.csv")

##we'll need to apply a function to all dataframes that omits unwanted metadata
Lichen_groups_LAN_VIs      [c("ScanID","PFT","PFT_2.x","PFT_3.x","PFT_4","veg_lifeform_code","group","Freq1","Freq2","Freq3","area" )] = NULL

set.seed(2017)
##Lets run some different models so we can asses whiich ones are the best for prediction later
rf_LAN_VIs   <-randomForest(color~.,data=Lichen_groups_LAN_VIs       ,mtry=sqrt(ncol(Lichen_groups_LAN_VIs        )),ntree=1001,localImp = TRUE)

##Lets create a data frame that will combine the class.error of all the categories of each model
rf_LAN_VIs_confusion   <-rf_LAN_VIs   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_LAN_VIs =class.error )

write.csv(rf_LAN_VIs_confusion,"Outputs/1_Field_spec/2_Models/Landsat/rf_LAN_VIs_confusion.csv")
