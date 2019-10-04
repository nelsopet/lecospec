library(tidyverse)
library(randomForest)

###reads in alaskasspeclib for vascular plants
alaskaSpecLib_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_vascular.csv")
alaskaSpecLib_5nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_5nm_vascular.csv")
alaskaSpecLib_10nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_10nm_vascular.csv")
alaskaSpecLib_50nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_50nm_vascular.csv")
alaskaSpecLib_100nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_100nm_vascular.csv")

## Remove unwanted metadata
alaskaSpecLib_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_5nm_vascular[c("ScanID","PFT","PFT_3","PFT_2","area")] = NULL
alaskaSpecLib_10nm_vascular[c("ScanID","PFT","PFT_3","PFT_2","area")] = NULL
alaskaSpecLib_50nm_vascular[c("ScanID","PFT","PFT_3","PFT_2","area")] = NULL
alaskaSpecLib_100nm_vascular[c("ScanID","PFT","PFT_3","PFT_2","area")] = NULL

#combines all preds for vascular plants
all_pred_vascular<-cbind(alaskaSpecLib_vascular,
                         alaskaSpecLib_5nm_vascular,
                         alaskaSpecLib_10nm_vascular,
                         alaskaSpecLib_50nm_vascular,
                         alaskaSpecLib_100nm_vascular)

#Convert to factor
all_pred_vascular$PFT_2<-as.factor(all_pred_vascular$PFT_2)

#Create training and testing dataset (all bands)
dataset_size_all_pred_vascular=floor(nrow(all_pred_vascular)*0.80)
index<-sample(1:nrow(all_pred_vascular),size=dataset_size_all_pred_vascular)
training_all_pred_vascular<-all_pred_vascular[index,]
testing_all_pred_vascular<-all_pred_vascular[-index,]

###random forest model for (vascular palnts)
rf_all_preds_vascular<-randomForest(PFT_2~.,data=training_all_pred_vascular,mtry=5,ntree=20001,importance=TRUE)