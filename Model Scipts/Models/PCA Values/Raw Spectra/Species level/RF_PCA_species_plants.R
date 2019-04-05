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
alaskaSpecLib_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_5nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_10nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_50nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_100nm_plants[c("ScanID","PFT","PFT_3","area")] = NULL

################################Model all bands########################################
##PCA (all bands)
pca_plants<- prcomp(alaskaSpecLib_plants[,-2152], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_plants <- round(pca_plants$sdev^2/sum(pca_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_plants$PFT_2))
pca_plants<-as.data.frame(cbind(names_species,pca_plants$x[,pc6]))
names(pca_plants)[1]<-paste("PFT_2")
pca_plants$PFT_2<-as.factor(pca_plants$PFT_2)

#Create training and testing dataset (all bands)
dataset_size_plants=floor(nrow(pca_plants)*0.80)
index<-sample(1:nrow(pca_plants),size=dataset_size_plants)
training_plants<-pca_plants[index,]
testing_plants<-pca_plants[-index,]

###random forest model for (plant and abiotic scans)
rf_plants<-randomForest(PFT_2~.,data=training_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_plants)
result_plants<-data.frame(testing_plants$PFT_2,predict(rf_plants,testing_plants,type = "response"))

################################Model 5nm bands########################################
##PCA (5nm bands)
pca_5nm_plants<- prcomp(alaskaSpecLib_5nm_plants[,-432], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_5nm_plants <- round(pca_5nm_plants$sdev^2/sum(pca_5nm_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_5nm_plants$PFT_2))
pca_5nm_plants<-as.data.frame(cbind(names_species,pca_5nm_plants$x[,pc6]))
names(pca_5nm_plants)[1]<-paste("PFT_2")
pca_5nm_plants$PFT_2<-as.factor(pca_5nm_plants$PFT_2)

#Create training and testing dataset (5nm bands)
dataset_size_5nm_plants=floor(nrow(pca_5nm_plants)*0.80)
index<-sample(1:nrow(pca_5nm_plants),size=dataset_size_5nm_plants)
training_5nm_plants<-pca_5nm_plants[index,]
testing_5nm_plants<-pca_5nm_plants[-index,]

###random forest model for (5nm bands)
rf_5nm_plants<-randomForest(PFT_2~.,data=training_5nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_5nm_plants)
result_5nm_plants<-data.frame(testing_5nm_plants$PFT_2,predict(rf_5nm_plants,testing_5nm_plants,type = "response"))

################################Model 10nm bands########################################
##PCA (10nm bands)
pca_10nm_plants<- prcomp(alaskaSpecLib_10nm_plants[,-217], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_10nm_plants <- round(pca_10nm_plants$sdev^2/sum(pca_10nm_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_10nm_plants$PFT_2))
pca_10nm_plants<-as.data.frame(cbind(names_species,pca_10nm_plants$x[,pc6]))
names(pca_10nm_plants)[1]<-paste("PFT_2")
pca_10nm_plants$PFT_2<-as.factor(pca_10nm_plants$PFT_2)

#Create training and testing dataset (10nm bands)
dataset_size_10nm_plants=floor(nrow(pca_10nm_plants)*0.80)
index<-sample(1:nrow(pca_10nm_plants),size=dataset_size_10nm_plants)
training_10nm_plants<-pca_10nm_plants[index,]
testing_10nm_plants<-pca_10nm_plants[-index,]

###random forest model for (10nm bands)
rf_10nm_plants<-randomForest(PFT_2~.,data=training_10nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_10nm_plants)
result_10nm_plants<-data.frame(testing_10nm_plants$PFT_2,predict(rf_10nm_plants,testing_10nm_plants,type = "response"))

################################Model 50nm bands########################################
##PCA (50nm bands)
pca_50nm_plants<- prcomp(alaskaSpecLib_50nm_plants[,-45], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_50nm_plants <- round(pca_50nm_plants$sdev^2/sum(pca_50nm_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_50nm_plants$PFT_2))
pca_50nm_plants<-as.data.frame(cbind(names_species,pca_50nm_plants$x[,pc6]))
names(pca_50nm_plants)[1]<-paste("PFT_2")
pca_50nm_plants$PFT_2<-as.factor(pca_50nm_plants$PFT_2)

#Create training and testing dataset (50nm bands)
dataset_size_50nm_plants=floor(nrow(pca_50nm_plants)*0.80)
index<-sample(1:nrow(pca_50nm_plants),size=dataset_size_50nm_plants)
training_50nm_plants<-pca_50nm_plants[index,]
testing_50nm_plants<-pca_50nm_plants[-index,]

###random forest model for (50nm bands)
rf_50nm_plants<-randomForest(PFT_2~.,data=training_50nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_50nm_plants)
result_50nm_plants<-data.frame(testing_50nm_plants$PFT_2,predict(rf_50nm_plants,testing_50nm_plants,type = "response"))

################################Model 100nm bands########################################
##PCA (100nm bands)
pca_100nm_plants<- prcomp(alaskaSpecLib_100nm_plants[,-23], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_100nm_plants <- round(pca_100nm_plants$sdev^2/sum(pca_100nm_plants$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_100nm_plants$PFT_2))
pca_100nm_plants<-as.data.frame(cbind(names_species,pca_100nm_plants$x[,pc6]))
names(pca_100nm_plants)[1]<-paste("PFT_2")
pca_100nm_plants$PFT_2<-as.factor(pca_100nm_plants$PFT_2)

#Create training and testing dataset (100nm bands)
dataset_size_100nm_plants=floor(nrow(pca_100nm_plants)*0.80)
index<-sample(1:nrow(pca_100nm_plants),size=dataset_size_100nm_plants)
training_100nm_plants<-pca_100nm_plants[index,]
testing_100nm_plants<-pca_100nm_plants[-index,]

###random forest model for (100nm bands)
rf_100nm_plants<-randomForest(PFT_2~.,data=training_100nm_plants,mtry=5,ntree=2001,importance=TRUE)
print(rf_100nm_plants)
result_100nm_plants<-data.frame(testing_100nm_plants$PFT_2,predict(rf_100nm_plants,testing_100nm_plants,type = "response"))
