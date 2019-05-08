library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_smooth_plants<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_plants.csv")
alaskaSpecLib_smooth_5nm_plants<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_5nm_plants.csv")
alaskaSpecLib_smooth_10nm_plants<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_10nm_plants.csv")
alaskaSpecLib_smooth_50nm_plants<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_50nm_plants.csv")
alaskaSpecLib_smooth_100nm_plants<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_100nm_plants.csv")

## Remove unwanted metadata
alaskaSpecLib_smooth_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_5nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_10nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_50nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_100nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL

################################Model all bands########################################
##PCA (all bands)
pca_smooth_plants<- prcomp(alaskaSpecLib_smooth_plants[,-2152], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_plants <- round(pca_smooth_plants$sdev^2/sum(pca_smooth_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_plants$PFT_2))
pca_smooth_plants<-as.data.frame(cbind(names_species,pca_smooth_plants$x[,pc6]))
names(pca_smooth_plants)[1]<-paste("PFT_2")
pca_smooth_plants$PFT_2<-as.factor(pca_smooth_plants$PFT_2)

#Create training and testing dataset (all bands)
dataset_size_plants=floor(nrow(pca_smooth_plants)*0.80)
index<-sample(1:nrow(pca_smooth_plants),size=dataset_size_plants)
training_smooth_plants<-pca_smooth_plants[index,]
testing_smooth_plants<-pca_smooth_plants[-index,]

###random forest model for (plant and abiotic scans)
rf_smooth_plants<-randomForest(PFT_2~.,data=training_smooth_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_plants)
result_smooth_plants<-data.frame(testing_smooth_plants$PFT_2,predict(rf_smooth_plants,testing_smooth_plants,type = "response"))

################################Model 5nm bands########################################
##PCA (5nm bands)
pca_smooth_5nm_plants<- prcomp(alaskaSpecLib_smooth_5nm_plants[,-432], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_5nm_plants <- round(pca_smooth_5nm_plants$sdev^2/sum(pca_smooth_5nm_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_5nm_plants$PFT_2))
pca_smooth_5nm_plants<-as.data.frame(cbind(names_species,pca_smooth_5nm_plants$x[,pc6]))
names(pca_smooth_5nm_plants)[1]<-paste("PFT_2")
pca_smooth_5nm_plants$PFT_2<-as.factor(pca_smooth_5nm_plants$PFT_2)

#Create training and testing dataset (5nm bands)
dataset_size_smooth_5nm_plants=floor(nrow(pca_smooth_5nm_plants)*0.80)
index<-sample(1:nrow(pca_smooth_5nm_plants),size=dataset_size_smooth_5nm_plants)
training_smooth_5nm_plants<-pca_smooth_5nm_plants[index,]
testing_smooth_5nm_plants<-pca_smooth_5nm_plants[-index,]

###random forest model for (5nm bands)
rf_smooth_5nm_plants<-randomForest(PFT_2~.,data=training_smooth_5nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_5nm_plants)
result_smooth_5nm_plants<-data.frame(testing_smooth_5nm_plants$PFT_2,predict(rf_smooth_5nm_plants,testing_smooth_5nm_plants,type = "response"))

################################Model 10nm bands########################################
##PCA (10nm bands)
pca_smooth_10nm_plants<- prcomp(alaskaSpecLib_smooth_10nm_plants[,-217], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_10nm_plants <- round(pca_smooth_10nm_plants$sdev^2/sum(pca_smooth_10nm_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_10nm_plants$PFT_2))
pca_smooth_10nm_plants<-as.data.frame(cbind(names_species,pca_smooth_10nm_plants$x[,pc6]))
names(pca_smooth_10nm_plants)[1]<-paste("PFT_2")
pca_smooth_10nm_plants$PFT_2<-as.factor(pca_smooth_10nm_plants$PFT_2)

#Create training and testing dataset (10nm bands)
dataset_size_smooth_10nm_plants=floor(nrow(pca_smooth_10nm_plants)*0.80)
index<-sample(1:nrow(pca_smooth_10nm_plants),size=dataset_size_smooth_10nm_plants)
training_smooth_10nm_plants<-pca_smooth_10nm_plants[index,]
testing_smooth_10nm_plants<-pca_smooth_10nm_plants[-index,]

###random forest model for (10nm bands)
rf_smooth_10nm_plants<-randomForest(PFT_2~.,data=training_smooth_10nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_10nm_plants)
result_smooth_10nm_plants<-data.frame(testing_smooth_10nm_plants$PFT_2,predict(rf_smooth_10nm_plants,testing_smooth_10nm_plants,type = "response"))

################################Model 50nm bands########################################
##PCA (50nm bands)
pca_smooth_50nm_plants<- prcomp(alaskaSpecLib_smooth_50nm_plants[,-45], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_50nm_plants <- round(pca_smooth_50nm_plants$sdev^2/sum(pca_smooth_50nm_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_50nm_plants$PFT_2))
pca_smooth_50nm_plants<-as.data.frame(cbind(names_species,pca_smooth_50nm_plants$x[,pc6]))
names(pca_smooth_50nm_plants)[1]<-paste("PFT_2")
pca_smooth_50nm_plants$PFT_2<-as.factor(pca_smooth_50nm_plants$PFT_2)

#Create training and testing dataset (50nm bands)
dataset_size_smooth_50nm_plants=floor(nrow(pca_smooth_50nm_plants)*0.80)
index<-sample(1:nrow(pca_smooth_50nm_plants),size=dataset_size_smooth_50nm_plants)
training_smooth_50nm_plants<-pca_smooth_50nm_plants[index,]
testing_smooth_50nm_plants<-pca_smooth_50nm_plants[-index,]

###random forest model for (50nm bands)
rf_smooth_50nm_plants<-randomForest(PFT_2~.,data=training_smooth_50nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_50nm_plants)
result_smooth_50nm_plants<-data.frame(testing_smooth_50nm_plants$PFT_2,predict(rf_smooth_50nm_plants,testing_smooth_50nm_plants,type = "response"))

################################Model 100nm bands########################################
##PCA (100nm bands)
pca_smooth_100nm_plants<- prcomp(alaskaSpecLib_smooth_100nm_plants[,-23], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_100nm_plants <- round(pca_smooth_100nm_plants$sdev^2/sum(pca_smooth_100nm_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_100nm_plants$PFT_2))
pca_smooth_100nm_plants<-as.data.frame(cbind(names_species,pca_smooth_100nm_plants$x[,pc6]))
names(pca_smooth_100nm_plants)[1]<-paste("PFT_2")
pca_smooth_100nm_plants$PFT_2<-as.factor(pca_smooth_100nm_plants$PFT_2)

#Create training and testing dataset (100nm bands)
dataset_size_smooth_100nm_plants=floor(nrow(pca_smooth_100nm_plants)*0.80)
index<-sample(1:nrow(pca_smooth_100nm_plants),size=dataset_size_smooth_100nm_plants)
training_smooth_100nm_plants<-pca_smooth_100nm_plants[index,]
testing_smooth_100nm_plants<-pca_smooth_100nm_plants[-index,]

###random forest model for (100nm bands)
rf_smooth_100nm_plants<-randomForest(PFT_2~.,data=training_smooth_100nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_100nm_plants)
result_smooth_100nm_plants<-data.frame(testing_smooth_100nm_plants$PFT_2,predict(rf_smooth_100nm_plants,testing_smooth_100nm_plants,type = "response"))

###extract error rate
error_rf_PCA_smooth_plants<-as.data.frame(rf_smooth_plants$err.rate[2001,1])
names(error_rf_PCA_smooth_plants)[1]<-"smooth_all_bands"
error_rf_PCA_smooth_5nm_plants<-as.data.frame(rf_smooth_5nm_plants$err.rate[2001,1])
names(error_rf_PCA_smooth_5nm_plants)[1]<-"smooth_5nm"
error_rf_PCA_smooth_10nm_plants<-as.data.frame(rf_smooth_10nm_plants$err.rate[2001,1])
names(error_rf_PCA_smooth_10nm_plants)[1]<-"smooth_10nm"
error_rf_PCA_smooth_50nm_plants<-as.data.frame(rf_smooth_50nm_plants$err.rate[2001,1])
names(error_rf_PCA_smooth_50nm_plants)[1]<-"smooth_50nm"
error_rf_PCA_smooth_100nm_plants<-as.data.frame(rf_smooth_100nm_plants$err.rate[2001,1])
names(error_rf_PCA_smooth_100nm_plants)[1]<-"smooth_100nm"

###Make data frame from error rate
error_rate_PCA_smooth_plants<-cbind(error_rf_PCA_smooth_plants,
                                error_rf_PCA_smooth_5nm_plants,
                                error_rf_PCA_smooth_10nm_plants,
                                error_rf_PCA_smooth_50nm_plants,
                                error_rf_PCA_smooth_100nm_plants)
error_rate_PCA_smooth_plants$category<-"All_plants"

##write to folder
write.csv(error_rate_PCA_smooth_plants,"Model Scripts/Error Rates/PCA/error_rate_PCA_smooth_plants.csv",row.names = F)