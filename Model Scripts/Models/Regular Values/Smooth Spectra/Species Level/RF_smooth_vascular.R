library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_smooth_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_vascular.csv")
alaskaSpecLib_smooth_5nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_5nm_vascular.csv")
alaskaSpecLib_smooth_10nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_10nm_vascular.csv")
alaskaSpecLib_smooth_50nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_50nm_vascular.csv")
alaskaSpecLib_smooth_100nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_100nm_vascular.csv")

## Remove unwanted metadata
alaskaSpecLib_smooth_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_5nm_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_10nm_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_50nm_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_100nm_vascular[c("ScanID","PFT","PFT_3","area")] = NULL

#Convert to factor
alaskaSpecLib_smooth_vascular$PFT_2<-as.factor(alaskaSpecLib_smooth_vascular$PFT_2)
alaskaSpecLib_smooth_5nm_vascular$PFT_2<-as.factor(alaskaSpecLib_smooth_5nm_vascular$PFT_2)
alaskaSpecLib_smooth_10nm_vascular$PFT_2<-as.factor(alaskaSpecLib_smooth_10nm_vascular$PFT_2)
alaskaSpecLib_smooth_50nm_vascular$PFT_2<-as.factor(alaskaSpecLib_smooth_50nm_vascular$PFT_2)
alaskaSpecLib_smooth_100nm_vascular$PFT_2<-as.factor(alaskaSpecLib_smooth_50nm_vascular$PFT_2)

################################Model all bands########################################
#Create training and testing dataset (all bands)
dataset_size_vascular=floor(nrow(alaskaSpecLib_smooth_vascular)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_vascular),size=dataset_size_vascular)
training_smooth_vascular<-alaskaSpecLib_smooth_vascular[index,]
testing_smooth_vascular<-alaskaSpecLib_smooth_vascular[-index,]

###random forest model for (plant and abiotic scans)
rf_smooth_vascular<-randomForest(PFT_2~.,data=training_smooth_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_vascular)
result_smooth_vascular<-data.frame(testing_smooth_vascular$PFT_2,predict(rf_smooth_vascular,testing_smooth_vascular,type = "response"))

################################Model 5nm bands########################################
#Create training and testing dataset (5nm bands)
dataset_size_smooth_5nm_vascular=floor(nrow(alaskaSpecLib_smooth_5nm_vascular)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_5nm_vascular),size=dataset_size_smooth_5nm_vascular)
training_smooth_5nm_vascular<-alaskaSpecLib_smooth_5nm_vascular[index,]
testing_smooth_5nm_vascular<-alaskaSpecLib_smooth_5nm_vascular[-index,]

###random forest model for (5nm bands)
rf_smooth_5nm_vascular<-randomForest(PFT_2~.,data=training_smooth_5nm_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_5nm_vascular)
result_smooth_5nm_vascular<-data.frame(testing_smooth_5nm_vascular$PFT_2,predict(rf_smooth_5nm_vascular,testing_smooth_5nm_vascular,type = "response"))

################################Model 10nm bands########################################
#Create training and testing dataset (10nm bands)
dataset_size_smooth_10nm_vascular=floor(nrow(alaskaSpecLib_smooth_10nm_vascular)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_10nm_vascular),size=dataset_size_smooth_10nm_vascular)
training_smooth_10nm_vascular<-alaskaSpecLib_smooth_10nm_vascular[index,]
testing_smooth_10nm_vascular<-alaskaSpecLib_smooth_10nm_vascular[-index,]

###random forest model for (10nm bands)
rf_smooth_10nm_vascular<-randomForest(PFT_2~.,data=training_smooth_10nm_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_10nm_vascular)
result_smooth_10nm_vascular<-data.frame(testing_smooth_10nm_vascular$PFT_2,predict(rf_smooth_10nm_vascular,testing_smooth_10nm_vascular,type = "response"))

################################Model 50nm bands########################################
#Create training and testing dataset (50nm bands)
dataset_size_smooth_50nm_vascular=floor(nrow(alaskaSpecLib_smooth_50nm_vascular)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_50nm_vascular),size=dataset_size_smooth_50nm_vascular)
training_smooth_50nm_vascular<-alaskaSpecLib_smooth_50nm_vascular[index,]
testing_smooth_50nm_vascular<-alaskaSpecLib_smooth_50nm_vascular[-index,]

###random forest model for (50nm bands)
rf_smooth_50nm_vascular<-randomForest(PFT_2~.,data=training_smooth_50nm_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_50nm_vascular)
result_smooth_50nm_vascular<-data.frame(testing_smooth_50nm_vascular$PFT_2,predict(rf_smooth_50nm_vascular,testing_smooth_50nm_vascular,type = "response"))

################################Model 100nm bands########################################
#Create training and testing dataset (100nm bands)
dataset_size_smooth_100nm_vascular=floor(nrow(alaskaSpecLib_smooth_100nm_vascular)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_100nm_vascular),size=dataset_size_smooth_100nm_vascular)
training_smooth_100nm_vascular<-alaskaSpecLib_smooth_100nm_vascular[index,]
testing_smooth_100nm_vascular<-alaskaSpecLib_smooth_100nm_vascular[-index,]

###random forest model for (100nm bands)
rf_smooth_100nm_vascular<-randomForest(PFT_2~.,data=training_smooth_100nm_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_100nm_vascular)
result_smooth_100nm_vascular<-data.frame(testing_smooth_100nm_vascular$PFT_2,predict(rf_smooth_100nm_vascular,testing_smooth_100nm_vascular,type = "response"))

###extract error rate
error_rf_smooth_vascular<-as.data.frame(rf_smooth_vascular$err.rate[2001,1])
names(error_rf_smooth_vascular)[1]<-"smooth_all_bands"
error_rf_smooth_5nm_vascular<-as.data.frame(rf_smooth_5nm_vascular$err.rate[2001,1])
names(error_rf_smooth_5nm_vascular)[1]<-"smooth_5nm"
error_rf_smooth_10nm_vascular<-as.data.frame(rf_smooth_10nm_vascular$err.rate[2001,1])
names(error_rf_smooth_10nm_vascular)[1]<-"smooth_10nm"
error_rf_smooth_50nm_vascular<-as.data.frame(rf_smooth_50nm_vascular$err.rate[2001,1])
names(error_rf_smooth_50nm_vascular)[1]<-"smooth_50nm"
error_rf_smooth_100nm_vascular<-as.data.frame(rf_smooth_100nm_vascular$err.rate[2001,1])
names(error_rf_smooth_100nm_vascular)[1]<-"smooth_100nm"

###Make data frame from error rate
error_rate_smooth_vascular<-cbind(error_rf_smooth_vascular,
                                  error_rf_smooth_5nm_vascular,
                                  error_rf_smooth_10nm_vascular,
                                  error_rf_smooth_50nm_vascular,
                                  error_rf_smooth_100nm_vascular)
error_rate_smooth_vascular$category<-"Vascular"

##write to folder
write.csv(error_rate_smooth_vascular,"/Alaska_Spectral_Library/Models/Error Rates/Regular/error_rate_smooth_vascular.csv",row.names= F)