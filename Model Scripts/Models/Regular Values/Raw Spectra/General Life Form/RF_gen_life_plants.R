library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_plants.csv")
alaskaSpecLib_5nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_5nm_plants.csv")
alaskaSpecLib_10nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_10nm_plants.csv")
alaskaSpecLib_50nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_50nm_plants.csv")
alaskaSpecLib_100nm_plants<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_100nm_plants.csv")

## Remove unwanted metadata
alaskaSpecLib_plants[c("ScanID","PFT","PFT_2","area")] = NULL
alaskaSpecLib_5nm_plants[c("ScanID","PFT","PFT_2","area")] = NULL
alaskaSpecLib_10nm_plants[c("ScanID","PFT","PFT_2","area")] = NULL
alaskaSpecLib_50nm_plants[c("ScanID","PFT","PFT_2","area")] = NULL
alaskaSpecLib_100nm_plants[c("ScanID","PFT","PFT_2","area")] = NULL

#Convert to factor
alaskaSpecLib_plants$PFT_3<-as.factor(alaskaSpecLib_plants$PFT_3)
alaskaSpecLib_5nm_plants$PFT_3<-as.factor(alaskaSpecLib_5nm_plants$PFT_3)
alaskaSpecLib_10nm_plants$PFT_3<-as.factor(alaskaSpecLib_10nm_plants$PFT_3)
alaskaSpecLib_50nm_plants$PFT_3<-as.factor(alaskaSpecLib_50nm_plants$PFT_3)
alaskaSpecLib_100nm_plants$PFT_3<-as.factor(alaskaSpecLib_50nm_plants$PFT_3)

################################Model all bands#######################################
#Create training and testing dataset (all bands)
dataset_size_plants=floor(nrow(alaskaSpecLib_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_plants),size=dataset_size_plants)
training_plants<-alaskaSpecLib_plants[index,]
testing_plants<-alaskaSpecLib_plants[-index,]

###random forest model for (plant and abiotic scans)
rf_plants<-randomForest(PFT_3~.,data=training_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_plants)
result_plants<-data.frame(testing_plants$PFT_3,predict(rf_plants,testing_plants,type = "response"))

################################Model 5nm bands########################################
#Create training and testing dataset (5nm bands)
dataset_size_5nm_plants=floor(nrow(alaskaSpecLib_5nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_5nm_plants),size=dataset_size_5nm_plants)
training_5nm_plants<-alaskaSpecLib_5nm_plants[index,]
testing_5nm_plants<-alaskaSpecLib_5nm_plants[-index,]

###random forest model for (5nm bands)
rf_5nm_plants<-randomForest(PFT_3~.,data=training_5nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_5nm_plants)
result_5nm_plants<-data.frame(testing_5nm_plants$PFT_3,predict(rf_5nm_plants,testing_5nm_plants,type = "response"))

################################Model 10nm bands########################################
##alaskaSpecLib (10nm bands)
#Create training and testing dataset (10nm bands)
dataset_size_10nm_plants=floor(nrow(alaskaSpecLib_10nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_10nm_plants),size=dataset_size_10nm_plants)
training_10nm_plants<-alaskaSpecLib_10nm_plants[index,]
testing_10nm_plants<-alaskaSpecLib_10nm_plants[-index,]

###random forest model for (10nm bands)
rf_10nm_plants<-randomForest(PFT_3~.,data=training_10nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_10nm_plants)
result_10nm_plants<-data.frame(testing_10nm_plants$PFT_3,predict(rf_10nm_plants,testing_10nm_plants,type = "response"))

################################Model 50nm bands########################################
#Create training and testing dataset (50nm bands)
dataset_size_50nm_plants=floor(nrow(alaskaSpecLib_50nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_50nm_plants),size=dataset_size_50nm_plants)
training_50nm_plants<-alaskaSpecLib_50nm_plants[index,]
testing_50nm_plants<-alaskaSpecLib_50nm_plants[-index,]

###random forest model for (50nm bands)
rf_50nm_plants<-randomForest(PFT_3~.,data=training_50nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_50nm_plants)
result_50nm_plants<-data.frame(testing_50nm_plants$PFT_3,predict(rf_50nm_plants,testing_50nm_plants,type = "response"))

################################Model 100nm bands########################################
#Create training and testing dataset (100nm bands)
dataset_size_100nm_plants=floor(nrow(alaskaSpecLib_100nm_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_100nm_plants),size=dataset_size_100nm_plants)
training_100nm_plants<-alaskaSpecLib_100nm_plants[index,]
testing_100nm_plants<-alaskaSpecLib_100nm_plants[-index,]

###random forest model for (100nm bands)
rf_100nm_plants<-randomForest(PFT_3~.,data=training_100nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_100nm_plants)
result_100nm_plants<-data.frame(testing_100nm_plants$PFT_3,predict(rf_100nm_plants,testing_100nm_plants,type = "response"))

###extract error rate
error_rf_plants<-as.data.frame(rf_plants$err.rate[2001,1])
names(error_rf_plants)[1]<-"all_bands"
error_rf_5nm_plants<-as.data.frame(rf_5nm_plants$err.rate[2001,1])
names(error_rf_5nm_plants)[1]<-"5nm"
error_rf_10nm_plants<-as.data.frame(rf_10nm_plants$err.rate[2001,1])
names(error_rf_10nm_plants)[1]<-"10nm"
error_rf_50nm_plants<-as.data.frame(rf_50nm_plants$err.rate[2001,1])
names(error_rf_50nm_plants)[1]<-"50nm"
error_rf_100nm_plants<-as.data.frame(rf_100nm_plants$err.rate[2001,1])
names(error_rf_100nm_plants)[1]<-"100nm"

###Make data frame from error rate
error_rate_gen_life_form<-cbind(error_rf_plants,
                                error_rf_5nm_plants,
                                error_rf_10nm_plants,
                                error_rf_50nm_plants,
                                error_rf_100nm_plants)
error_rate_gen_life_form$category<-"Courser_levels"

##write to folder
write.csv(error_rate_gen_life_form,"/Alaska_Spectral_Library/Models/Error Rates/Regular/error_rate_gen_life_form.csv",row.names = F)
