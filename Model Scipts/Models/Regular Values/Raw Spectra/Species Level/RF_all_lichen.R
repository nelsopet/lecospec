library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_lichen.csv")
alaskaSpecLib_5nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_5nm_lichen.csv")
alaskaSpecLib_10nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_10nm_lichen.csv")
alaskaSpecLib_50nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_50nm_lichen.csv")
alaskaSpecLib_100nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_100nm_lichen.csv")

## Remove unwanted metadata
alaskaSpecLib_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_5nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_10nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_50nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_100nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL

#Convert to factor
alaskaSpecLib_lichen$PFT_2<-as.factor(alaskaSpecLib_lichen$PFT_2)
alaskaSpecLib_5nm_lichen$PFT_2<-as.factor(alaskaSpecLib_5nm_lichen$PFT_2)
alaskaSpecLib_10nm_lichen$PFT_2<-as.factor(alaskaSpecLib_10nm_lichen$PFT_2)
alaskaSpecLib_50nm_lichen$PFT_2<-as.factor(alaskaSpecLib_50nm_lichen$PFT_2)
alaskaSpecLib_100nm_lichen$PFT_2<-as.factor(alaskaSpecLib_50nm_lichen$PFT_2)

################################Model all bands#######################################
#Create training and testing dataset (all bands)
dataset_size_lichen=floor(nrow(alaskaSpecLib_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_lichen),size=dataset_size_lichen)
training_lichen<-alaskaSpecLib_lichen[index,]
testing_lichen<-alaskaSpecLib_lichen[-index,]

###random forest model for (plant and abiotic scans)
rf_lichen<-randomForest(PFT_2~.,data=training_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_lichen)
result_lichen<-data.frame(testing_lichen$PFT_2,predict(rf_lichen,testing_lichen,type = "response"))

################################Model 5nm bands########################################
#Create training and testing dataset (5nm bands)
dataset_size_5nm_lichen=floor(nrow(alaskaSpecLib_5nm_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_5nm_lichen),size=dataset_size_5nm_lichen)
training_5nm_lichen<-alaskaSpecLib_5nm_lichen[index,]
testing_5nm_lichen<-alaskaSpecLib_5nm_lichen[-index,]

###random forest model for (5nm bands)
rf_5nm_lichen<-randomForest(PFT_2~.,data=training_5nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_5nm_lichen)
result_5nm_lichen<-data.frame(testing_5nm_lichen$PFT_2,predict(rf_5nm_lichen,testing_5nm_lichen,type = "response"))

################################Model 10nm bands########################################
##alaskaSpecLib (10nm bands)
#Create training and testing dataset (10nm bands)
dataset_size_10nm_lichen=floor(nrow(alaskaSpecLib_10nm_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_10nm_lichen),size=dataset_size_10nm_lichen)
training_10nm_lichen<-alaskaSpecLib_10nm_lichen[index,]
testing_10nm_lichen<-alaskaSpecLib_10nm_lichen[-index,]

###random forest model for (10nm bands)
rf_10nm_lichen<-randomForest(PFT_2~.,data=training_10nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_10nm_lichen)
result_10nm_lichen<-data.frame(testing_10nm_lichen$PFT_2,predict(rf_10nm_lichen,testing_10nm_lichen,type = "response"))

################################Model 50nm bands########################################
#Create training and testing dataset (50nm bands)
dataset_size_50nm_lichen=floor(nrow(alaskaSpecLib_50nm_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_50nm_lichen),size=dataset_size_50nm_lichen)
training_50nm_lichen<-alaskaSpecLib_50nm_lichen[index,]
testing_50nm_lichen<-alaskaSpecLib_50nm_lichen[-index,]

###random forest model for (50nm bands)
rf_50nm_lichen<-randomForest(PFT_2~.,data=training_50nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_50nm_lichen)
result_50nm_lichen<-data.frame(testing_50nm_lichen$PFT_2,predict(rf_50nm_lichen,testing_50nm_lichen,type = "response"))

################################Model 100nm bands########################################
#Create training and testing dataset (100nm bands)
dataset_size_100nm_lichen=floor(nrow(alaskaSpecLib_100nm_lichen)*0.80)
index<-sample(1:nrow(alaskaSpecLib_100nm_lichen),size=dataset_size_100nm_lichen)
training_100nm_lichen<-alaskaSpecLib_100nm_lichen[index,]
testing_100nm_lichen<-alaskaSpecLib_100nm_lichen[-index,]

###random forest model for (100nm bands)
rf_100nm_lichen<-randomForest(PFT_2~.,data=training_100nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_100nm_lichen)
result_100nm_lichen<-data.frame(testing_100nm_lichen$PFT_2,predict(rf_100nm_lichen,testing_100nm_lichen,type = "response"))