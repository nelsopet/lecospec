library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskasspeclib
alaskaSpecLib_smooth_bryo<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_bryo.csv")
alaskaSpecLib_smooth_5nm_bryo<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_5nm_bryo.csv")
alaskaSpecLib_smooth_10nm_bryo<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_10nm_bryo.csv")
alaskaSpecLib_smooth_50nm_bryo<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_50nm_bryo.csv")
alaskaSpecLib_smooth_100nm_bryo<-read.csv("/Alaska_Spectral_Library/processed spec/AlaskaSpecLib/alaskaSpecLib_smooth_100nm_bryo.csv")

## Remove unwanted metadata
alaskaSpecLib_smooth_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_5nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_10nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_50nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_smooth_100nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL

################################Model all bands########################################
##PCA (all bands)
pca_smooth_bryo<- prcomp(alaskaSpecLib_smooth_bryo[,-2152], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_bryo <- round(pca_smooth_bryo$sdev^2/sum(pca_smooth_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_bryo$PFT_2))
pca_smooth_bryo<-as.data.frame(cbind(names_species,pca_smooth_bryo$x[,pc6]))
names(pca_smooth_bryo)[1]<-paste("PFT_2")
pca_smooth_bryo$PFT_2<-as.factor(pca_smooth_bryo$PFT_2)

#Create training and testing dataset (all bands)
dataset_size_bryo=floor(nrow(pca_smooth_bryo)*0.80)
index<-sample(1:nrow(pca_smooth_bryo),size=dataset_size_bryo)
training_bryo<-pca_smooth_bryo[index,]
testing_bryo<-pca_smooth_bryo[-index,]

###random forest model for (plant and abiotic scans)
rf_smooth_bryo<-randomForest(PFT_2~.,data=training_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_bryo)
result_smooth_bryo<-data.frame(testing_bryo$PFT_2,predict(rf_smooth_bryo,testing_bryo,type = "response"))

################################Model 5nm bands########################################
##PCA (5nm bands)
pca_smooth_5nm_bryo<- prcomp(alaskaSpecLib_smooth_5nm_bryo[,-432], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_5nm_bryo <- round(pca_smooth_5nm_bryo$sdev^2/sum(pca_smooth_5nm_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_5nm_bryo$PFT_2))
pca_smooth_5nm_bryo<-as.data.frame(cbind(names_species,pca_smooth_5nm_bryo$x[,pc6]))
names(pca_smooth_5nm_bryo)[1]<-paste("PFT_2")
pca_smooth_5nm_bryo$PFT_2<-as.factor(pca_smooth_5nm_bryo$PFT_2)

#Create training and testing dataset (5nm bands)
dataset_size_smooth_5nm_bryo=floor(nrow(pca_smooth_5nm_bryo)*0.80)
index<-sample(1:nrow(pca_smooth_5nm_bryo),size=dataset_size_smooth_5nm_bryo)
training_smooth_5nm_bryo<-pca_smooth_5nm_bryo[index,]
testing_smooth_5nm_bryo<-pca_smooth_5nm_bryo[-index,]

###random forest model for (5nm bands)
rf_smooth_5nm_bryo<-randomForest(PFT_2~.,data=training_smooth_5nm_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_5nm_bryo)
result_smooth_5nm_bryo<-data.frame(testing_smooth_5nm_bryo$PFT_2,predict(rf_smooth_5nm_bryo,testing_smooth_5nm_bryo,type = "response"))

################################Model 10nm bands########################################
##PCA (10nm bands)
pca_smooth_10nm_bryo<- prcomp(alaskaSpecLib_smooth_10nm_bryo[,-217], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_10nm_bryo <- round(pca_smooth_10nm_bryo$sdev^2/sum(pca_smooth_10nm_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_10nm_bryo$PFT_2))
pca_smooth_10nm_bryo<-as.data.frame(cbind(names_species,pca_smooth_10nm_bryo$x[,pc6]))
names(pca_smooth_10nm_bryo)[1]<-paste("PFT_2")
pca_smooth_10nm_bryo$PFT_2<-as.factor(pca_smooth_10nm_bryo$PFT_2)

#Create training and testing dataset (10nm bands)
dataset_size_smooth_10nm_bryo=floor(nrow(pca_smooth_10nm_bryo)*0.80)
index<-sample(1:nrow(pca_smooth_10nm_bryo),size=dataset_size_smooth_10nm_bryo)
training_smooth_10nm_bryo<-pca_smooth_10nm_bryo[index,]
testing_smooth_10nm_bryo<-pca_smooth_10nm_bryo[-index,]

###random forest model for (10nm bands)
rf_smooth_10nm_bryo<-randomForest(PFT_2~.,data=training_smooth_10nm_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_10nm_bryo)
result_smooth_10nm_bryo<-data.frame(testing_smooth_10nm_bryo$PFT_2,predict(rf_smooth_10nm_bryo,testing_smooth_10nm_bryo,type = "response"))

################################Model 50nm bands########################################
##PCA (50nm bands)
pca_smooth_50nm_bryo<- prcomp(alaskaSpecLib_smooth_50nm_bryo[,-45], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_50nm_bryo <- round(pca_smooth_50nm_bryo$sdev^2/sum(pca_smooth_50nm_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_50nm_bryo$PFT_2))
pca_smooth_50nm_bryo<-as.data.frame(cbind(names_species,pca_smooth_50nm_bryo$x[,pc6]))
names(pca_smooth_50nm_bryo)[1]<-paste("PFT_2")
pca_smooth_50nm_bryo$PFT_2<-as.factor(pca_smooth_50nm_bryo$PFT_2)

#Create training and testing dataset (50nm bands)
dataset_size_smooth_50nm_bryo=floor(nrow(pca_smooth_50nm_bryo)*0.80)
index<-sample(1:nrow(pca_smooth_50nm_bryo),size=dataset_size_smooth_50nm_bryo)
training_smooth_50nm_bryo<-pca_smooth_50nm_bryo[index,]
testing_smooth_50nm_bryo<-pca_smooth_50nm_bryo[-index,]

###random forest model for (50nm bands)
rf_smooth_50nm_bryo<-randomForest(PFT_2~.,data=training_smooth_50nm_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_50nm_bryo)
result_smooth_50nm_bryo<-data.frame(testing_smooth_50nm_bryo$PFT_2,predict(rf_smooth_50nm_bryo,testing_smooth_50nm_bryo,type = "response"))

################################Model 100nm bands########################################
##PCA (100nm bands)
pca_smooth_100nm_bryo<- prcomp(alaskaSpecLib_smooth_100nm_bryo[,-23], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_smooth_100nm_bryo <- round(pca_smooth_100nm_bryo$sdev^2/sum(pca_smooth_100nm_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_smooth_100nm_bryo$PFT_2))
pca_smooth_100nm_bryo<-as.data.frame(cbind(names_species,pca_smooth_100nm_bryo$x[,pc6]))
names(pca_smooth_100nm_bryo)[1]<-paste("PFT_2")
pca_smooth_100nm_bryo$PFT_2<-as.factor(pca_smooth_100nm_bryo$PFT_2)

#Create training and testing dataset (100nm bands)
dataset_size_smooth_100nm_bryo=floor(nrow(pca_smooth_100nm_bryo)*0.80)
index<-sample(1:nrow(pca_smooth_100nm_bryo),size=dataset_size_smooth_100nm_bryo)
training_smooth_100nm_bryo<-pca_smooth_100nm_bryo[index,]
testing_smooth_100nm_bryo<-pca_smooth_100nm_bryo[-index,]

###random forest model for (100nm bands)
rf_smooth_100nm_bryo<-randomForest(PFT_2~.,data=training_smooth_100nm_bryo,mtry=5,ntree=2001,importance=TRUE)
print(rf_smooth_100nm_bryo)
result_smooth_100nm_bryo<-data.frame(testing_smooth_100nm_bryo$PFT_2,predict(rf_smooth_100nm_bryo,testing_smooth_100nm_bryo,type = "response"))

