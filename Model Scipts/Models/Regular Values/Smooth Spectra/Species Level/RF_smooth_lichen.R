library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_smooth_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_lichen.csv")
alaskaSpecLib_smooth_5nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_5nm_lichen.csv")
alaskaSpecLib_smooth_10nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_10nm_lichen.csv")
alaskaSpecLib_smooth_50nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_50nm_lichen.csv")
alaskaSpecLib_smooth_100nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_100nm_lichen.csv")

## Remove unwanted metadata
alaskaSpecLib_smooth_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_5nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_10nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_50nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_100nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL

#Convert to factor
alaskaSpecLib_smooth_lichen$PFT_2<-as.factor(alaskaSpecLib_smooth_lichen$PFT_2)
alaskaSpecLib_smooth_5nm_lichen$PFT_2<-as.factor(alaskaSpecLib_smooth_5nm_lichen$PFT_2)
alaskaSpecLib_smooth_10nm_lichen$PFT_2<-as.factor(alaskaSpecLib_smooth_10nm_lichen$PFT_2)
alaskaSpecLib_smooth_50nm_lichen$PFT_2<-as.factor(alaskaSpecLib_smooth_50nm_lichen$PFT_2)
alaskaSpecLib_smooth_100nm_lichen$PFT_2<-as.factor(alaskaSpecLib_smooth_50nm_lichen$PFT_2)

################################Model all bands########################################
#Create training and testing dataset (all bands)
dataset_size_lichen=floor(nrow(alaskaSpecLib_smooth_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_lichen),size=dataset_size_lichen)
training_lichen<-alaskaSpecLib_smooth_lichen[index,]
testing_lichen<-alaskaSpecLib_smooth_lichen[-index,]

###random forest model for (plant and abiotic scans)
rf_lichen<-randomForest(PFT_2~.,data=training_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_lichen)
result_lichen<-data.frame(testing_lichen$PFT_2,predict(rf_lichen,testing_lichen,type = "response"))

################################Model 5nm bands########################################
#Create training and testing dataset (5nm bands)
dataset_size_smooth_5nm_lichen=floor(nrow(alaskaSpecLib_smooth_5nm_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_5nm_lichen),size=dataset_size_smooth_5nm_lichen)
training_smooth_5nm_lichen<-alaskaSpecLib_smooth_5nm_lichen[index,]
testing_smooth_5nm_lichen<-alaskaSpecLib_smooth_5nm_lichen[-index,]

###random forest model for (5nm bands)
rf_smooth_5nm_lichen<-randomForest(PFT_2~.,data=training_smooth_5nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_5nm_lichen)
result_smooth_5nm_lichen<-data.frame(testing_smooth_5nm_lichen$PFT_2,predict(rf_smooth_5nm_lichen,testing_smooth_5nm_lichen,type = "response"))

################################Model 10nm bands########################################
#Create training and testing dataset (10nm bands)
dataset_size_smooth_10nm_lichen=floor(nrow(alaskaSpecLib_smooth_10nm_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_10nm_lichen),size=dataset_size_smooth_10nm_lichen)
training_smooth_10nm_lichen<-alaskaSpecLib_smooth_10nm_lichen[index,]
testing_smooth_10nm_lichen<-alaskaSpecLib_smooth_10nm_lichen[-index,]

###random forest model for (10nm bands)
rf_smooth_10nm_lichen<-randomForest(PFT_2~.,data=training_smooth_10nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_10nm_lichen)
result_smooth_10nm_lichen<-data.frame(testing_smooth_10nm_lichen$PFT_2,predict(rf_smooth_10nm_lichen,testing_smooth_10nm_lichen,type = "response"))

################################Model 50nm bands########################################
#Create training and testing dataset (50nm bands)
dataset_size_smooth_50nm_lichen=floor(nrow(alaskaSpecLib_smooth_50nm_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_50nm_lichen),size=dataset_size_smooth_50nm_lichen)
training_smooth_50nm_lichen<-alaskaSpecLib_smooth_50nm_lichen[index,]
testing_smooth_50nm_lichen<-alaskaSpecLib_smooth_50nm_lichen[-index,]

###random forest model for (50nm bands)
rf_smooth_50nm_lichen<-randomForest(PFT_2~.,data=training_smooth_50nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_50nm_lichen)
result_smooth_50nm_lichen<-data.frame(testing_smooth_50nm_lichen$PFT_2,predict(rf_smooth_50nm_lichen,testing_smooth_50nm_lichen,type = "response"))

################################Model 100nm bands########################################
#Create training and testing dataset (100nm bands)
dataset_size_smooth_100nm_lichen=floor(nrow(alaskaSpecLib_smooth_100nm_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_100nm_lichen),size=dataset_size_smooth_100nm_lichen)
training_smooth_100nm_lichen<-alaskaSpecLib_smooth_100nm_lichen[index,]
testing_smooth_100nm_lichen<-alaskaSpecLib_smooth_100nm_lichen[-index,]

###random forest model for (100nm bands)
rf_smooth_100nm_lichen<-randomForest(PFT_2~.,data=training_smooth_100nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_100nm_lichen)
result_smooth_100nm_lichen<-data.frame(testing_smooth_100nm_lichen$PFT_2,predict(rf_smooth_100nm_lichen,testing_smooth_100nm_lichen,type = "response"))