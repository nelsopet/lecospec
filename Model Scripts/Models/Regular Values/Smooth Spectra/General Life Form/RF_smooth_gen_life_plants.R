library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_smooth_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_plants.csv")
alaskaSpecLib_smooth_5nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_5nm_plants.csv")
alaskaSpecLib_smooth_10nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_10nm_plants.csv")
alaskaSpecLib_smooth_50nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_50nm_plants.csv")
alaskaSpecLib_smooth_100nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_100nm_plants.csv")

## Remove unwanted metadata
alaskaSpecLib_smooth_plants[c("ScanID","PFT","PFT_2","area")] = NULL
alaskaSpecLib_smooth_5nm_plants[c("ScanID","PFT","PFT_2","area")] = NULL
alaskaSpecLib_smooth_10nm_plants[c("ScanID","PFT","PFT_2","area")] = NULL
alaskaSpecLib_smooth_50nm_plants[c("ScanID","PFT","PFT_2","area")] = NULL
alaskaSpecLib_smooth_100nm_plants[c("ScanID","PFT","PFT_2","area")] = NULL

#Convert to factor
alaskaSpecLib_smooth_plants$PFT_3<-as.factor(alaskaSpecLib_smooth_plants$PFT_3)
alaskaSpecLib_smooth_5nm_plants$PFT_3<-as.factor(alaskaSpecLib_smooth_5nm_plants$PFT_3)
alaskaSpecLib_smooth_10nm_plants$PFT_3<-as.factor(alaskaSpecLib_smooth_10nm_plants$PFT_3)
alaskaSpecLib_smooth_50nm_plants$PFT_3<-as.factor(alaskaSpecLib_smooth_50nm_plants$PFT_3)
alaskaSpecLib_smooth_100nm_plants$PFT_3<-as.factor(alaskaSpecLib_smooth_50nm_plants$PFT_3)

################################Model all bands########################################
#Create training and testing dataset (all bands)
dataset_size_plants=floor(nrow(alaskaSpecLib_smooth_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_plants),size=dataset_size_plants)
training_smooth_plants<-alaskaSpecLib_smooth_plants[index,]
testing_smooth_plants<-alaskaSpecLib_smooth_plants[-index,]

###random forest model for (plant and abiotic scans)
rf_smooth_plants<-randomForest(PFT_3~.,data=training_smooth_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_plants)
result_smooth_plants<-data.frame(testing_smooth_plants$PFT_3,predict(rf_smooth_plants,testing_smooth_plants,type = "response"))

################################Model 5nm bands########################################
#Create training and testing dataset (5nm bands)
dataset_size_smooth_5nm_plants=floor(nrow(alaskaSpecLib_smooth_5nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_5nm_plants),size=dataset_size_smooth_5nm_plants)
training_smooth_5nm_plants<-alaskaSpecLib_smooth_5nm_plants[index,]
testing_smooth_5nm_plants<-alaskaSpecLib_smooth_5nm_plants[-index,]

###random forest model for (5nm bands)
rf_smooth_5nm_plants<-randomForest(PFT_3~.,data=training_smooth_5nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_5nm_plants)
result_smooth_5nm_plants<-data.frame(testing_smooth_5nm_plants$PFT_3,predict(rf_smooth_5nm_plants,testing_smooth_5nm_plants,type = "response"))

################################Model 10nm bands########################################
#Create training and testing dataset (10nm bands)
dataset_size_smooth_10nm_plants=floor(nrow(alaskaSpecLib_smooth_10nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_10nm_plants),size=dataset_size_smooth_10nm_plants)
training_smooth_10nm_plants<-alaskaSpecLib_smooth_10nm_plants[index,]
testing_smooth_10nm_plants<-alaskaSpecLib_smooth_10nm_plants[-index,]

###random forest model for (10nm bands)
rf_smooth_10nm_plants<-randomForest(PFT_3~.,data=training_smooth_10nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_10nm_plants)
result_smooth_10nm_plants<-data.frame(testing_smooth_10nm_plants$PFT_3,predict(rf_smooth_10nm_plants,testing_smooth_10nm_plants,type = "response"))

################################Model 50nm bands########################################
#Create training and testing dataset (50nm bands)
dataset_size_smooth_50nm_plants=floor(nrow(alaskaSpecLib_smooth_50nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_50nm_plants),size=dataset_size_smooth_50nm_plants)
training_smooth_50nm_plants<-alaskaSpecLib_smooth_50nm_plants[index,]
testing_smooth_50nm_plants<-alaskaSpecLib_smooth_50nm_plants[-index,]

###random forest model for (50nm bands)
rf_smooth_50nm_plants<-randomForest(PFT_3~.,data=training_smooth_50nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_50nm_plants)
result_smooth_50nm_plants<-data.frame(testing_smooth_50nm_plants$PFT_3,predict(rf_smooth_50nm_plants,testing_smooth_50nm_plants,type = "response"))

################################Model 100nm bands########################################
#Create training and testing dataset (100nm bands)
dataset_size_smooth_100nm_plants=floor(nrow(alaskaSpecLib_smooth_100nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_100nm_plants),size=dataset_size_smooth_100nm_plants)
training_smooth_100nm_plants<-alaskaSpecLib_smooth_100nm_plants[index,]
testing_smooth_100nm_plants<-alaskaSpecLib_smooth_100nm_plants[-index,]

###random forest model for (100nm bands)
rf_smooth_100nm_plants<-randomForest(PFT_3~.,data=training_smooth_100nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_100nm_plants)
result_smooth_100nm_plants<-data.frame(testing_smooth_100nm_plants$PFT_3,predict(rf_smooth_100nm_plants,testing_smooth_100nm_plants,type = "response"))

###extract error rate
error_rf_smooth_plants<-as.data.frame(rf_smooth_plants$err.rate[2001,1])
names(error_rf_smooth_plants)[1]<-"smooth_all_bands"
error_rf_smooth_5nm_plants<-as.data.frame(rf_smooth_5nm_plants$err.rate[2001,1])
names(error_rf_smooth_5nm_plants)[1]<-"smooth_5nm"
error_rf_smooth_10nm_plants<-as.data.frame(rf_smooth_10nm_plants$err.rate[2001,1])
names(error_rf_smooth_10nm_plants)[1]<-"smooth_10nm"
error_rf_smooth_50nm_plants<-as.data.frame(rf_smooth_50nm_plants$err.rate[2001,1])
names(error_rf_smooth_50nm_plants)[1]<-"smooth_50nm"
error_rf_smooth_100nm_plants<-as.data.frame(rf_smooth_100nm_plants$err.rate[2001,1])
names(error_rf_smooth_100nm_plants)[1]<-"smooth_100nm"

###Make data frame from error rate
error_rate_smooth_gen_life_form<-cbind(error_rf_smooth_plants,
                                       error_rf_smooth_5nm_plants,
                                       error_rf_smooth_10nm_plants,
                                       error_rf_smooth_50nm_plants,
                                       error_rf_smooth_100nm_plants)
error_rate_smooth_gen_life_form$category<-"Courser_levels"

##write to folder
write.csv(error_rate_smooth_gen_life_form,"/Alaska_Spectral_Library/Models/Error Rates/Regular/error_rate_smooth_gen_life_form.csv",row.names= F)