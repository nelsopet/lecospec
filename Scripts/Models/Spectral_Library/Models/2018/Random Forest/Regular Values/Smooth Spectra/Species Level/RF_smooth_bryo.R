library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_smooth_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_bryo.csv")
alaskaSpecLib_smooth_5nm_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_5nm_bryo.csv")
alaskaSpecLib_smooth_10nm_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_10nm_bryo.csv")
alaskaSpecLib_smooth_50nm_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_50nm_bryo.csv")
alaskaSpecLib_smooth_100nm_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_100nm_bryo.csv")

## Remove unwanted metadata
alaskaSpecLib_smooth_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_5nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_10nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_50nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_100nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL

#Convert to factor
alaskaSpecLib_smooth_bryo$PFT_2<-as.factor(alaskaSpecLib_smooth_bryo$PFT_2)
alaskaSpecLib_smooth_5nm_bryo$PFT_2<-as.factor(alaskaSpecLib_smooth_5nm_bryo$PFT_2)
alaskaSpecLib_smooth_10nm_bryo$PFT_2<-as.factor(alaskaSpecLib_smooth_10nm_bryo$PFT_2)
alaskaSpecLib_smooth_50nm_bryo$PFT_2<-as.factor(alaskaSpecLib_smooth_50nm_bryo$PFT_2)
alaskaSpecLib_smooth_100nm_bryo$PFT_2<-as.factor(alaskaSpecLib_smooth_50nm_bryo$PFT_2)

################################Model all bands########################################
#Create training and testing dataset (all bands)
dataset_size_bryo=floor(nrow(alaskaSpecLib_smooth_bryo)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_bryo),size=dataset_size_bryo)
training_smooth_bryo<-alaskaSpecLib_smooth_bryo[index,]
testing_smooth_bryo<-alaskaSpecLib_smooth_bryo[-index,]

###random forest model for (plant and abiotic scans)
rf_smooth_bryo<-randomForest(PFT_2~.,data=training_smooth_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_bryo)
result_smooth_bryo<-data.frame(testing_smooth_bryo$PFT_2,predict(rf_smooth_bryo,testing_smooth_bryo,type = "response"))

################################Model 5nm bands########################################
#Create training and testing dataset (5nm bands)
dataset_size_smooth_5nm_bryo=floor(nrow(alaskaSpecLib_smooth_5nm_bryo)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_5nm_bryo),size=dataset_size_smooth_5nm_bryo)
training_smooth_5nm_bryo<-alaskaSpecLib_smooth_5nm_bryo[index,]
testing_smooth_5nm_bryo<-alaskaSpecLib_smooth_5nm_bryo[-index,]

###random forest model for (5nm bands)
rf_smooth_5nm_bryo<-randomForest(PFT_2~.,data=training_smooth_5nm_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_5nm_bryo)
result_smooth_5nm_bryo<-data.frame(testing_smooth_5nm_bryo$PFT_2,predict(rf_smooth_5nm_bryo,testing_smooth_5nm_bryo,type = "response"))

################################Model 10nm bands########################################
#Create training and testing dataset (10nm bands)
dataset_size_smooth_10nm_bryo=floor(nrow(alaskaSpecLib_smooth_10nm_bryo)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_10nm_bryo),size=dataset_size_smooth_10nm_bryo)
training_smooth_10nm_bryo<-alaskaSpecLib_smooth_10nm_bryo[index,]
testing_smooth_10nm_bryo<-alaskaSpecLib_smooth_10nm_bryo[-index,]

###random forest model for (10nm bands)
rf_smooth_10nm_bryo<-randomForest(PFT_2~.,data=training_smooth_10nm_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_10nm_bryo)
result_smooth_10nm_bryo<-data.frame(testing_smooth_10nm_bryo$PFT_2,predict(rf_smooth_10nm_bryo,testing_smooth_10nm_bryo,type = "response"))

################################Model 50nm bands########################################
#Create training and testing dataset (50nm bands)
dataset_size_smooth_50nm_bryo=floor(nrow(alaskaSpecLib_smooth_50nm_bryo)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_50nm_bryo),size=dataset_size_smooth_50nm_bryo)
training_smooth_50nm_bryo<-alaskaSpecLib_smooth_50nm_bryo[index,]
testing_smooth_50nm_bryo<-alaskaSpecLib_smooth_50nm_bryo[-index,]

###random forest model for (50nm bands)
rf_smooth_50nm_bryo<-randomForest(PFT_2~.,data=training_smooth_50nm_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_50nm_bryo)
result_smooth_50nm_bryo<-data.frame(testing_smooth_50nm_bryo$PFT_2,predict(rf_smooth_50nm_bryo,testing_smooth_50nm_bryo,type = "response"))

################################Model 100nm bands########################################
#Create training and testing dataset (100nm bands)
dataset_size_smooth_100nm_bryo=floor(nrow(alaskaSpecLib_smooth_100nm_bryo)*0.80)
index<-sample(1:nrow(alaskaSpecLib_smooth_100nm_bryo),size=dataset_size_smooth_100nm_bryo)
training_smooth_100nm_bryo<-alaskaSpecLib_smooth_100nm_bryo[index,]
testing_smooth_100nm_bryo<-alaskaSpecLib_smooth_100nm_bryo[-index,]

###random forest model for (100nm bands)
rf_smooth_100nm_bryo<-randomForest(PFT_2~.,data=training_smooth_100nm_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_100nm_bryo)
result_smooth_100nm_bryo<-data.frame(testing_smooth_100nm_bryo$PFT_2,predict(rf_smooth_100nm_bryo,testing_smooth_100nm_bryo,type = "response"))

###extract error rate
error_rf_smooth_bryo<-as.data.frame(rf_smooth_bryo$err.rate[2001,1])
names(error_rf_smooth_bryo)[1]<-"smooth_all_bands"
error_rf_smooth_5nm_bryo<-as.data.frame(rf_smooth_5nm_bryo$err.rate[2001,1])
names(error_rf_smooth_5nm_bryo)[1]<-"smooth_5nm"
error_rf_smooth_10nm_bryo<-as.data.frame(rf_smooth_10nm_bryo$err.rate[2001,1])
names(error_rf_smooth_10nm_bryo)[1]<-"smooth_10nm"
error_rf_smooth_50nm_bryo<-as.data.frame(rf_smooth_50nm_bryo$err.rate[2001,1])
names(error_rf_smooth_50nm_bryo)[1]<-"smooth_50nm"
error_rf_smooth_100nm_bryo<-as.data.frame(rf_smooth_100nm_bryo$err.rate[2001,1])
names(error_rf_smooth_100nm_bryo)[1]<-"smooth_100nm"

###Make data frame from error rate
error_rate_smooth_bryo<-cbind(error_rf_smooth_bryo,
                              error_rf_smooth_5nm_bryo,
                              error_rf_smooth_10nm_bryo,
                              error_rf_smooth_50nm_bryo,
                              error_rf_smooth_100nm_bryo)
error_rate_smooth_bryo$category<-"Bryophytes"

##write to folder
write.csv(error_rate_smooth_bryo,"Model Scripts/Error Rates/Regular/error_rate_smooth_bryo.csv",row.names= F)
