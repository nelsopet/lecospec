library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_smooth_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_plants.csv")
alaskaSpecLib_smooth_5nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_5nm_plants.csv")
alaskaSpecLib_smooth_10nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_10nm_plants.csv")
alaskaSpecLib_smooth_50nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_50nm_plants.csv")
alaskaSpecLib_smooth_100nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_100nm_plants.csv")

## Remove unwanted metadata
alaskaSpecLib_smooth_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_5nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_10nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_50nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_100nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL

#Convert to factor
alaskaSpecLib_smooth_plants$PFT_2<-as.factor(alaskaSpecLib_smooth_plants$PFT_2)
alaskaSpecLib_smooth_5nm_plants$PFT_2<-as.factor(alaskaSpecLib_smooth_5nm_plants$PFT_2)
alaskaSpecLib_smooth_10nm_plants$PFT_2<-as.factor(alaskaSpecLib_smooth_10nm_plants$PFT_2)
alaskaSpecLib_smooth_50nm_plants$PFT_2<-as.factor(alaskaSpecLib_smooth_50nm_plants$PFT_2)
alaskaSpecLib_smooth_100nm_plants$PFT_2<-as.factor(alaskaSpecLib_smooth_50nm_plants$PFT_2)

################################Model all bands########################################
#Create training and testing dataset (all bands)
dataset_size_plants=floor(nrow(alaskaSpecLib_smooth_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_plants),size=dataset_size_plants)
training_plants<-alaskaSpecLib_smooth_plants[index,]
testing_plants<-alaskaSpecLib_smooth_plants[-index,]

###random forest model for (plant and abiotic scans)
rf_plants<-randomForest(PFT_2~.,data=training_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_plants)
result_plants<-data.frame(testing_plants$PFT_2,predict(rf_plants,testing_plants,type = "response"))

################################Model 5nm bands########################################
#Create training and testing dataset (5nm bands)
dataset_size_smooth_5nm_plants=floor(nrow(alaskaSpecLib_smooth_5nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_5nm_plants),size=dataset_size_smooth_5nm_plants)
training_smooth_5nm_plants<-alaskaSpecLib_smooth_5nm_plants[index,]
testing_smooth_5nm_plants<-alaskaSpecLib_smooth_5nm_plants[-index,]

###random forest model for (5nm bands)
rf_smooth_5nm_plants<-randomForest(PFT_2~.,data=training_smooth_5nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_5nm_plants)
result_smooth_5nm_plants<-data.frame(testing_smooth_5nm_plants$PFT_2,predict(rf_smooth_5nm_plants,testing_smooth_5nm_plants,type = "response"))

################################Model 10nm bands########################################
#Create training and testing dataset (10nm bands)
dataset_size_smooth_10nm_plants=floor(nrow(alaskaSpecLib_smooth_10nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_10nm_plants),size=dataset_size_smooth_10nm_plants)
training_smooth_10nm_plants<-alaskaSpecLib_smooth_10nm_plants[index,]
testing_smooth_10nm_plants<-alaskaSpecLib_smooth_10nm_plants[-index,]

###random forest model for (10nm bands)
rf_smooth_10nm_plants<-randomForest(PFT_2~.,data=training_smooth_10nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_10nm_plants)
result_smooth_10nm_plants<-data.frame(testing_smooth_10nm_plants$PFT_2,predict(rf_smooth_10nm_plants,testing_smooth_10nm_plants,type = "response"))

################################Model 50nm bands########################################
#Create training and testing dataset (50nm bands)
dataset_size_smooth_50nm_plants=floor(nrow(alaskaSpecLib_smooth_50nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_50nm_plants),size=dataset_size_smooth_50nm_plants)
training_smooth_50nm_plants<-alaskaSpecLib_smooth_50nm_plants[index,]
testing_smooth_50nm_plants<-alaskaSpecLib_smooth_50nm_plants[-index,]

###random forest model for (50nm bands)
rf_smooth_50nm_plants<-randomForest(PFT_2~.,data=training_smooth_50nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_50nm_plants)
result_smooth_50nm_plants<-data.frame(testing_smooth_50nm_plants$PFT_2,predict(rf_smooth_50nm_plants,testing_smooth_50nm_plants,type = "response"))

################################Model 100nm bands########################################
#Create training and testing dataset (100nm bands)
dataset_size_smooth_100nm_plants=floor(nrow(alaskaSpecLib_smooth_100nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_100nm_plants),size=dataset_size_smooth_100nm_plants)
training_smooth_100nm_plants<-alaskaSpecLib_smooth_100nm_plants[index,]
testing_smooth_100nm_plants<-alaskaSpecLib_smooth_100nm_plants[-index,]

###random forest model for (100nm bands)
rf_smooth_100nm_plants<-randomForest(PFT_2~.,data=training_smooth_100nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_100nm_plants)
result_smooth_100nm_plants<-data.frame(testing_smooth_100nm_plants$PFT_2,predict(rf_smooth_100nm_plants,testing_smooth_100nm_plants,type = "response"))