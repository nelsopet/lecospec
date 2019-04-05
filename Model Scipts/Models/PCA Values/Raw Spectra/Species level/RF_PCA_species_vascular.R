library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_vascular.csv")
alaskaSpecLib_5nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_5nm_vascular.csv")
alaskaSpecLib_10nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_10nm_vascular.csv")
alaskaSpecLib_50nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_50nm_vascular.csv")
alaskaSpecLib_100nm_vascular<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_100nm_vascular.csv")

## Remove unwanted metadata
alaskaSpecLib_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_5nm_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_10nm_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_50nm_vascular[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_100nm_vascular[c("ScanID","PFT","PFT_3","area")] = NULL

################################Model all bands########################################
##PCA (all bands)
pca_vascular<- prcomp(alaskaSpecLib_vascular[,-2152], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_vascular <- round(pca_vascular$sdev^2/sum(pca_vascular$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_vascular$PFT_2))
pca_vascular<-as.data.frame(cbind(names_species,pca_vascular$x[,pc6]))
names(pca_vascular)[1]<-paste("PFT_2")
pca_vascular$PFT_2<-as.factor(pca_vascular$PFT_2)

#Create training and testing dataset (all bands)
dataset_size_vascular=floor(nrow(pca_vascular)*0.80)
index<-sample(1:nrow(pca_vascular),size=dataset_size_vascular)
training_vascular<-pca_vascular[index,]
testing_vascular<-pca_vascular[-index,]

###random forest model for (plant and abiotic scans)
rf_vascular<-randomForest(PFT_2~.,data=training_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_vascular)
result_vascular<-data.frame(testing_vascular$PFT_2,predict(rf_vascular,testing_vascular,type = "response"))

################################Model 5nm bands########################################
##PCA (5nm bands)
pca_5nm_vascular<- prcomp(alaskaSpecLib_5nm_vascular[,-432], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_5nm_vascular <- round(pca_5nm_vascular$sdev^2/sum(pca_5nm_vascular$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_5nm_vascular$PFT_2))
pca_5nm_vascular<-as.data.frame(cbind(names_species,pca_5nm_vascular$x[,pc6]))
names(pca_5nm_vascular)[1]<-paste("PFT_2")
pca_5nm_vascular$PFT_2<-as.factor(pca_5nm_vascular$PFT_2)

#Create training and testing dataset (5nm bands)
dataset_size_5nm_vascular=floor(nrow(pca_5nm_vascular)*0.80)
index<-sample(1:nrow(pca_5nm_vascular),size=dataset_size_5nm_vascular)
training_5nm_vascular<-pca_5nm_vascular[index,]
testing_5nm_vascular<-pca_5nm_vascular[-index,]

###random forest model for (5nm bands)
rf_5nm_vascular<-randomForest(PFT_2~.,data=training_5nm_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_5nm_vascular)
result_5nm_vascular<-data.frame(testing_5nm_vascular$PFT_2,predict(rf_5nm_vascular,testing_5nm_vascular,type = "response"))

################################Model 10nm bands########################################
##PCA (10nm bands)
pca_10nm_vascular<- prcomp(alaskaSpecLib_10nm_vascular[,-217], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_10nm_vascular <- round(pca_10nm_vascular$sdev^2/sum(pca_10nm_vascular$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_10nm_vascular$PFT_2))
pca_10nm_vascular<-as.data.frame(cbind(names_species,pca_10nm_vascular$x[,pc6]))
names(pca_10nm_vascular)[1]<-paste("PFT_2")
pca_10nm_vascular$PFT_2<-as.factor(pca_10nm_vascular$PFT_2)

#Create training and testing dataset (10nm bands)
dataset_size_10nm_vascular=floor(nrow(pca_10nm_vascular)*0.80)
index<-sample(1:nrow(pca_10nm_vascular),size=dataset_size_10nm_vascular)
training_10nm_vascular<-pca_10nm_vascular[index,]
testing_10nm_vascular<-pca_10nm_vascular[-index,]

###random forest model for (10nm bands)
rf_10nm_vascular<-randomForest(PFT_2~.,data=training_10nm_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_10nm_vascular)
result_10nm_vascular<-data.frame(testing_10nm_vascular$PFT_2,predict(rf_10nm_vascular,testing_10nm_vascular,type = "response"))

################################Model 50nm bands########################################
##PCA (50nm bands)
pca_50nm_vascular<- prcomp(alaskaSpecLib_50nm_vascular[,-45], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_50nm_vascular <- round(pca_50nm_vascular$sdev^2/sum(pca_50nm_vascular$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_50nm_vascular$PFT_2))
pca_50nm_vascular<-as.data.frame(cbind(names_species,pca_50nm_vascular$x[,pc6]))
names(pca_50nm_vascular)[1]<-paste("PFT_2")
pca_50nm_vascular$PFT_2<-as.factor(pca_50nm_vascular$PFT_2)

#Create training and testing dataset (50nm bands)
dataset_size_50nm_vascular=floor(nrow(pca_50nm_vascular)*0.80)
index<-sample(1:nrow(pca_50nm_vascular),size=dataset_size_50nm_vascular)
training_50nm_vascular<-pca_50nm_vascular[index,]
testing_50nm_vascular<-pca_50nm_vascular[-index,]

###random forest model for (50nm bands)
rf_50nm_vascular<-randomForest(PFT_2~.,data=training_50nm_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_50nm_vascular)
result_50nm_vascular<-data.frame(testing_50nm_vascular$PFT_2,predict(rf_50nm_vascular,testing_50nm_vascular,type = "response"))

################################Model 100nm bands########################################
##PCA (100nm bands)
pca_100nm_vascular<- prcomp(alaskaSpecLib_100nm_vascular[,-23], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_100nm_vascular <- round(pca_100nm_vascular$sdev^2/sum(pca_100nm_vascular$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_100nm_vascular$PFT_2))
pca_100nm_vascular<-as.data.frame(cbind(names_species,pca_100nm_vascular$x[,pc6]))
names(pca_100nm_vascular)[1]<-paste("PFT_2")
pca_100nm_vascular$PFT_2<-as.factor(pca_100nm_vascular$PFT_2)

#Create training and testing dataset (100nm bands)
dataset_size_100nm_vascular=floor(nrow(pca_100nm_vascular)*0.80)
index<-sample(1:nrow(pca_100nm_vascular),size=dataset_size_100nm_vascular)
training_100nm_vascular<-pca_100nm_vascular[index,]
testing_100nm_vascular<-pca_100nm_vascular[-index,]

###random forest model for (100nm bands)
rf_100nm_vascular<-randomForest(PFT_2~.,data=training_100nm_vascular,mtry=5,ntree=2001,importance=TRUE)
print(rf_100nm_vascular)
result_100nm_vascular<-data.frame(testing_100nm_vascular$PFT_2,predict(rf_100nm_vascular,testing_100nm_vascular,type = "response"))
