library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_2019_plants      <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_2019_plants.csv")
alaskaSpecLib_2019_005nm_plants<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_2019_005nm_plants.csv")
alaskaSpecLib_2019_010nm_plants<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_2019_010nm_plants.csv")
alaskaSpecLib_2019_050nm_plants<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_2019_050nm_plants.csv")
alaskaSpecLib_2019_100nm_plants<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_2019_100nm_plants.csv")

## Remove unwanted metadata
alaskaSpecLib_2019_plants      [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_2019_005nm_plants[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_2019_010nm_plants[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_2019_050nm_plants[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_2019_100nm_plants[c("ScanID","PFT","PFT_3","area","Freq")] = NULL

#Convert to factor
alaskaSpecLib_2019_plants$PFT_2      <-as.factor(alaskaSpecLib_2019_plants$PFT_2)
alaskaSpecLib_2019_005nm_plants$PFT_2<-as.factor(alaskaSpecLib_2019_005nm_plants$PFT_2)
alaskaSpecLib_2019_010nm_plants$PFT_2<-as.factor(alaskaSpecLib_2019_010nm_plants$PFT_2)
alaskaSpecLib_2019_050nm_plants$PFT_2<-as.factor(alaskaSpecLib_2019_050nm_plants$PFT_2)
alaskaSpecLib_2019_100nm_plants$PFT_2<-as.factor(alaskaSpecLib_2019_050nm_plants$PFT_2)

################################Model all bands#######################################
#Create training and testing dataset (all bands)
dataset_size_plants=floor(nrow(alaskaSpecLib_2019_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_2019_plants),size=dataset_size_plants)
training_plants<-alaskaSpecLib_2019_plants[index,]
testing_plants<-alaskaSpecLib_2019_plants[-index,]

###random forest model for (plant and abiotic scans)
rf_plants<-randomForest(PFT_2~.,data=training_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_plants)
result_plants<-data.frame(testing_plants$PFT_2,predict(rf_plants,testing_plants,type = "response"))

################################Model 5nm bands########################################
#Create training and testing dataset (5nm bands)
dataset_size_005nm_plants=floor(nrow(alaskaSpecLib_2019_005nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_2019_005nm_plants),size=dataset_size_005nm_plants)
training_005nm_plants<-alaskaSpecLib_2019_005nm_plants[index,]
testing_005nm_plants<-alaskaSpecLib_2019_005nm_plants[-index,]

###random forest model for (5nm bands)
rf_005nm_plants<-randomForest(PFT_2~.,data=training_005nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_005nm_plants)
result_005nm_plants<-data.frame(testing_005nm_plants$PFT_2,predict(rf_005nm_plants,testing_005nm_plants,type = "response"))

################################Model 10nm bands########################################
##alaskaSpecLib (10nm bands)
#Create training and testing dataset (10nm bands)
dataset_size_010nm_plants=floor(nrow(alaskaSpecLib_2019_010nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_2019_010nm_plants),size=dataset_size_010nm_plants)
training_010nm_plants<-alaskaSpecLib_2019_010nm_plants[index,]
testing_010nm_plants<-alaskaSpecLib_2019_010nm_plants[-index,]

###random forest model for (10nm bands)
rf_010nm_plants<-randomForest(PFT_2~.,data=training_010nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_010nm_plants)
result_010nm_plants<-data.frame(testing_010nm_plants$PFT_2,predict(rf_010nm_plants,testing_010nm_plants,type = "response"))

################################Model 50nm bands########################################
#Create training and testing dataset (50nm bands)
dataset_size_050nm_plants=floor(nrow(alaskaSpecLib_2019_050nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_2019_050nm_plants),size=dataset_size_050nm_plants)
training_050nm_plants<-alaskaSpecLib_2019_050nm_plants[index,]
testing_050nm_plants<-alaskaSpecLib_2019_050nm_plants[-index,]

###random forest model for (50nm bands)
rf_050nm_plants<-randomForest(PFT_2~.,data=training_050nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_050nm_plants)
result_050nm_plants<-data.frame(testing_050nm_plants$PFT_2,predict(rf_050nm_plants,testing_050nm_plants,type = "response"))

################################Model 100nm bands########################################
#Create training and testing dataset (100nm bands)
dataset_size_100nm_plants=floor(nrow(alaskaSpecLib_2019_100nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_2019_100nm_plants),size=dataset_size_100nm_plants)
training_100nm_plants<-alaskaSpecLib_2019_100nm_plants[index,]
testing_100nm_plants<-alaskaSpecLib_2019_100nm_plants[-index,]

###random forest model for (100nm bands)
rf_100nm_plants<-randomForest(PFT_2~.,data=training_100nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_100nm_plants)
result_100nm_plants<-data.frame(testing_100nm_plants$PFT_2,predict(rf_100nm_plants,testing_100nm_plants,type = "response"))

###extract error rate
error_rf_plants<-as.data.frame(rf_plants$err.rate[2001,1])
names(error_rf_plants)[1]<-"all_bands"
error_rf_005nm_plants<-as.data.frame(rf_005nm_plants$err.rate[2001,1])
names(error_rf_005nm_plants)[1]<-"005nm"
error_rf_010nm_plants<-as.data.frame(rf_010nm_plants$err.rate[2001,1])
names(error_rf_010nm_plants)[1]<-"010nm"
error_rf_050nm_plants<-as.data.frame(rf_050nm_plants$err.rate[2001,1])
names(error_rf_050nm_plants)[1]<-"050nm"
error_rf_100nm_plants<-as.data.frame(rf_100nm_plants$err.rate[2001,1])
names(error_rf_100nm_plants)[1]<-"100nm"

###Make data frame from error rate
error_rate_plants<-cbind(error_rf_plants,
                         error_rf_005nm_plants,
                         error_rf_010nm_plants,
                         error_rf_050nm_plants,
                         error_rf_100nm_plants)
error_rate_plants$category<-"All_plants"

##write to folder
write.csv(error_rate_plants,"Processed_spec/Error_rates/Regular/error_rate_plants.csv",row.names = F)
