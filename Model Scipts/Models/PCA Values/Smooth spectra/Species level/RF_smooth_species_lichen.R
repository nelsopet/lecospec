library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_smooth_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_lichen.csv")
alaskaSpecLib_smooth_5nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_5nm_lichen.csv")
alaskaSpecLib_smooth_10nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_10nm_lichen.csv")
alaskaSpecLib_smooth_50nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_50nm_lichen.csv")
alaskaSpecLib_smooth_100nm_lichen<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_100nm_lichen.csv")

## Remove unwanted metadata
alaskaSpecLib_smooth_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_5nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_10nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_50nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_100nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL

################################Model all bands########################################
##PCA (all bands)
pca_smooth_lichen<- prcomp(alaskaSpecLib_smooth_lichen[,-2152], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_lichen <- round(pca_smooth_lichen$sdev^2/sum(pca_smooth_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_lichen$PFT_2))
pca_smooth_lichen<-as.data.frame(cbind(names_species,pca_smooth_lichen$x[,pc6]))
names(pca_smooth_lichen)[1]<-paste("PFT_2")
pca_smooth_lichen$PFT_2<-as.factor(pca_smooth_lichen$PFT_2)

#Create training and testing dataset (all bands)
dataset_size_lichen=floor(nrow(pca_smooth_lichen)*0.80)
index<-sample(1:nrow(pca_smooth_lichen),size=dataset_size_lichen)
training_lichen<-pca_smooth_lichen[index,]
testing_lichen<-pca_smooth_lichen[-index,]

###random forest model for (plant and abiotic scans)
rf_smooth_lichen<-randomForest(PFT_2~.,data=training_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_lichen)
result_smooth_lichen<-data.frame(testing_lichen$PFT_2,predict(rf_smooth_lichen,testing_lichen,type = "response"))

################################Model 5nm bands########################################
##PCA (5nm bands)
pca_smooth_5nm_lichen<- prcomp(alaskaSpecLib_smooth_5nm_lichen[,-432], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_5nm_lichen <- round(pca_smooth_5nm_lichen$sdev^2/sum(pca_smooth_5nm_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_5nm_lichen$PFT_2))
pca_smooth_5nm_lichen<-as.data.frame(cbind(names_species,pca_smooth_5nm_lichen$x[,pc6]))
names(pca_smooth_5nm_lichen)[1]<-paste("PFT_2")
pca_smooth_5nm_lichen$PFT_2<-as.factor(pca_smooth_5nm_lichen$PFT_2)

#Create training and testing dataset (5nm bands)
dataset_size_smooth_5nm_lichen=floor(nrow(pca_smooth_5nm_lichen)*0.80)
index<-sample(1:nrow(pca_smooth_5nm_lichen),size=dataset_size_smooth_5nm_lichen)
training_smooth_5nm_lichen<-pca_smooth_5nm_lichen[index,]
testing_smooth_5nm_lichen<-pca_smooth_5nm_lichen[-index,]

###random forest model for (5nm bands)
rf_smooth_5nm_lichen<-randomForest(PFT_2~.,data=training_smooth_5nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_5nm_lichen)
result_smooth_5nm_lichen<-data.frame(testing_smooth_5nm_lichen$PFT_2,predict(rf_smooth_5nm_lichen,testing_smooth_5nm_lichen,type = "response"))

################################Model 10nm bands########################################
##PCA (10nm bands)
pca_smooth_10nm_lichen<- prcomp(alaskaSpecLib_smooth_10nm_lichen[,-217], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_10nm_lichen <- round(pca_smooth_10nm_lichen$sdev^2/sum(pca_smooth_10nm_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_10nm_lichen$PFT_2))
pca_smooth_10nm_lichen<-as.data.frame(cbind(names_species,pca_smooth_10nm_lichen$x[,pc6]))
names(pca_smooth_10nm_lichen)[1]<-paste("PFT_2")
pca_smooth_10nm_lichen$PFT_2<-as.factor(pca_smooth_10nm_lichen$PFT_2)

#Create training and testing dataset (10nm bands)
dataset_size_smooth_10nm_lichen=floor(nrow(pca_smooth_10nm_lichen)*0.80)
index<-sample(1:nrow(pca_smooth_10nm_lichen),size=dataset_size_smooth_10nm_lichen)
training_smooth_10nm_lichen<-pca_smooth_10nm_lichen[index,]
testing_smooth_10nm_lichen<-pca_smooth_10nm_lichen[-index,]

###random forest model for (10nm bands)
rf_smooth_10nm_lichen<-randomForest(PFT_2~.,data=training_smooth_10nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_10nm_lichen)
result_smooth_10nm_lichen<-data.frame(testing_smooth_10nm_lichen$PFT_2,predict(rf_smooth_10nm_lichen,testing_smooth_10nm_lichen,type = "response"))

################################Model 50nm bands########################################
##PCA (50nm bands)
pca_smooth_50nm_lichen<- prcomp(alaskaSpecLib_smooth_50nm_lichen[,-45], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_50nm_lichen <- round(pca_smooth_50nm_lichen$sdev^2/sum(pca_smooth_50nm_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_50nm_lichen$PFT_2))
pca_smooth_50nm_lichen<-as.data.frame(cbind(names_species,pca_smooth_50nm_lichen$x[,pc6]))
names(pca_smooth_50nm_lichen)[1]<-paste("PFT_2")
pca_smooth_50nm_lichen$PFT_2<-as.factor(pca_smooth_50nm_lichen$PFT_2)

#Create training and testing dataset (50nm bands)
dataset_size_smooth_50nm_lichen=floor(nrow(pca_smooth_50nm_lichen)*0.80)
index<-sample(1:nrow(pca_smooth_50nm_lichen),size=dataset_size_smooth_50nm_lichen)
training_smooth_50nm_lichen<-pca_smooth_50nm_lichen[index,]
testing_smooth_50nm_lichen<-pca_smooth_50nm_lichen[-index,]

###random forest model for (50nm bands)
rf_smooth_50nm_lichen<-randomForest(PFT_2~.,data=training_smooth_50nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_50nm_lichen)
result_smooth_50nm_lichen<-data.frame(testing_smooth_50nm_lichen$PFT_2,predict(rf_smooth_50nm_lichen,testing_smooth_50nm_lichen,type = "response"))

################################Model 100nm bands########################################
##PCA (100nm bands)
pca_smooth_100nm_lichen<- prcomp(alaskaSpecLib_smooth_100nm_lichen[,-23], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_100nm_lichen <- round(pca_smooth_100nm_lichen$sdev^2/sum(pca_smooth_100nm_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_100nm_lichen$PFT_2))
pca_smooth_100nm_lichen<-as.data.frame(cbind(names_species,pca_smooth_100nm_lichen$x[,pc6]))
names(pca_smooth_100nm_lichen)[1]<-paste("PFT_2")
pca_smooth_100nm_lichen$PFT_2<-as.factor(pca_smooth_100nm_lichen$PFT_2)

#Create training and testing dataset (100nm bands)
dataset_size_smooth_100nm_lichen=floor(nrow(pca_smooth_100nm_lichen)*0.80)
index<-sample(1:nrow(pca_smooth_100nm_lichen),size=dataset_size_smooth_100nm_lichen)
training_smooth_100nm_lichen<-pca_smooth_100nm_lichen[index,]
testing_smooth_100nm_lichen<-pca_smooth_100nm_lichen[-index,]

###random forest model for (100nm bands)
rf_smooth_100nm_lichen<-randomForest(PFT_2~.,data=training_smooth_100nm_lichen,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_100nm_lichen)
result_smooth_100nm_lichen<-data.frame(testing_smooth_100nm_lichen$PFT_2,predict(rf_smooth_100nm_lichen,testing_smooth_100nm_lichen,type = "response"))

