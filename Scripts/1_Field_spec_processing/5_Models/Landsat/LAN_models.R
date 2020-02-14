#######################Creates models from field spec data resapmepled on landsat bandpasses######################################
library(tidyverse)
library(randomForest)
library(randomForestExplainer)

###Reads in all predictors for scans
alaskaSpeclib_LAN_VIs<-read.csv("Outputs/1_Field_spec/1_Processing/Landsat_data/alaskaSpeclib_LAN_VIs.csv")

##we'll need to apply a function to all dataframes that omits unwanted metadata
alaskaSpeclib_LAN_VIs       [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq4")] = NULL

set.seed(2017)
##Lets run some different models so we can asses whiich ones are the best for prediction later
rf_LAN_VIs   <-randomForest(PFT_3~.,data=alaskaSpeclib_LAN_VIs        ,mtry=sqrt(ncol(alaskaSpeclib_LAN_VIs        )),ntree=1001,localImp = TRUE)