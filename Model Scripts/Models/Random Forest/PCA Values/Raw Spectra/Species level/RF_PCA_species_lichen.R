library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskaspeclib
alaskaSpecLib_lichen<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_lichen.csv")
alaskaSpecLib_5nm_lichen<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_5nm_lichen.csv")
alaskaSpecLib_10nm_lichen<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_10nm_lichen.csv")
alaskaSpecLib_50nm_lichen<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_50nm_lichen.csv")
alaskaSpecLib_100nm_lichen<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_100nm_lichen.csv")

## Remove unwanted metadata
alaskaSpecLib_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_5nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_10nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_50nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_100nm_lichen[c("ScanID","PFT","PFT_3","area")] = NULL

################################Model all bands########################################
##PCA (all bands)
pca_lichen<- prcomp(alaskaSpecLib_lichen[,-2152], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_lichen <- round(pca_lichen$sdev^2/sum(pca_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_lichen$PFT_2))
pca_lichen<-as.data.frame(cbind(names_species,pca_lichen$x[,pc6]))
names(pca_lichen)[1]<-paste("PFT_2")
pca_lichen$PFT_2<-as.factor(pca_lichen$PFT_2)

#Create training and testing dataset (all bands)
dataset_size_lichen=floor(nrow(pca_lichen)*0.80)
index<-sample(1:nrow(pca_lichen),size=dataset_size_lichen)
training_lichen<-pca_lichen[index,]
testing_lichen<-pca_lichen[-index,]

###random forest model for (plant and abiotic scans)
rf_lichen<-randomForest(PFT_2~.,data=training_lichen,mtry=5,ntree=2001,importance=TRUE)
result_lichen<-data.frame(testing_lichen$PFT_2,predict(rf_lichen,testing_lichen,type = "response"))
confusion_martrix_lichen<-as.data.frame(rf_lichen$confusion)

################################Model 5nm bands########################################
##PCA (5nm bands)
pca_5nm_lichen<- prcomp(alaskaSpecLib_5nm_lichen[,-432], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_5nm_lichen <- round(pca_5nm_lichen$sdev^2/sum(pca_5nm_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_5nm_lichen$PFT_2))
pca_5nm_lichen<-as.data.frame(cbind(names_species,pca_5nm_lichen$x[,pc6]))
names(pca_5nm_lichen)[1]<-paste("PFT_2")
pca_5nm_lichen$PFT_2<-as.factor(pca_5nm_lichen$PFT_2)

#Create training and testing dataset (5nm bands)
dataset_size_5nm_lichen=floor(nrow(pca_5nm_lichen)*0.80)
index<-sample(1:nrow(pca_5nm_lichen),size=dataset_size_5nm_lichen)
training_5nm_lichen<-pca_5nm_lichen[index,]
testing_5nm_lichen<-pca_5nm_lichen[-index,]

###random forest model for (5nm bands)
rf_5nm_lichen<-randomForest(PFT_2~.,data=training_5nm_lichen,mtry=5,ntree=2001,importance=TRUE)
result_5nm_lichen<-data.frame(testing_5nm_lichen$PFT_2,predict(rf_5nm_lichen,testing_5nm_lichen,type = "response"))
confusion_martrix_5nm_lichen<-as.data.frame(rf_5nm_lichen$confusion)

################################Model 10nm bands########################################
##PCA (10nm bands)
pca_10nm_lichen<- prcomp(alaskaSpecLib_10nm_lichen[,-217], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_10nm_lichen <- round(pca_10nm_lichen$sdev^2/sum(pca_10nm_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_10nm_lichen$PFT_2))
pca_10nm_lichen<-as.data.frame(cbind(names_species,pca_10nm_lichen$x[,pc6]))
names(pca_10nm_lichen)[1]<-paste("PFT_2")
pca_10nm_lichen$PFT_2<-as.factor(pca_10nm_lichen$PFT_2)

#Create training and testing dataset (10nm bands)
dataset_size_10nm_lichen=floor(nrow(pca_10nm_lichen)*0.80)
index<-sample(1:nrow(pca_10nm_lichen),size=dataset_size_10nm_lichen)
training_10nm_lichen<-pca_10nm_lichen[index,]
testing_10nm_lichen<-pca_10nm_lichen[-index,]

###random forest model for (10nm bands)
rf_10nm_lichen<-randomForest(PFT_2~.,data=training_10nm_lichen,mtry=5,ntree=2001,importance=TRUE)
result_10nm_lichen<-data.frame(testing_10nm_lichen$PFT_2,predict(rf_10nm_lichen,testing_10nm_lichen,type = "response"))
confusion_martrix_10nm_lichen<-as.data.frame(rf_10nm_lichen$confusion)

################################Model 50nm bands########################################
##PCA (50nm bands)
pca_50nm_lichen<- prcomp(alaskaSpecLib_50nm_lichen[,-45], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_50nm_lichen <- round(pca_50nm_lichen$sdev^2/sum(pca_50nm_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_50nm_lichen$PFT_2))
pca_50nm_lichen<-as.data.frame(cbind(names_species,pca_50nm_lichen$x[,pc6]))
names(pca_50nm_lichen)[1]<-paste("PFT_2")
pca_50nm_lichen$PFT_2<-as.factor(pca_50nm_lichen$PFT_2)

#Create training and testing dataset (50nm bands)
dataset_size_50nm_lichen=floor(nrow(pca_50nm_lichen)*0.80)
index<-sample(1:nrow(pca_50nm_lichen),size=dataset_size_50nm_lichen)
training_50nm_lichen<-pca_50nm_lichen[index,]
testing_50nm_lichen<-pca_50nm_lichen[-index,]

###random forest model for (50nm bands)
rf_50nm_lichen<-randomForest(PFT_2~.,data=training_50nm_lichen,mtry=5,ntree=2001,importance=TRUE)
result_50nm_lichen<-data.frame(testing_50nm_lichen$PFT_2,predict(rf_50nm_lichen,testing_50nm_lichen,type = "response"))
confusion_martrix_50nm_lichen<-as.data.frame(rf_50nm_lichen$confusion)

################################Model 100nm bands########################################
##PCA (100nm bands)
pca_100nm_lichen<- prcomp(alaskaSpecLib_100nm_lichen[,-23], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_100nm_lichen <- round(pca_100nm_lichen$sdev^2/sum(pca_100nm_lichen$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_100nm_lichen$PFT_2))
pca_100nm_lichen<-as.data.frame(cbind(names_species,pca_100nm_lichen$x[,pc6]))
names(pca_100nm_lichen)[1]<-paste("PFT_2")
pca_100nm_lichen$PFT_2<-as.factor(pca_100nm_lichen$PFT_2)

#Create training and testing dataset (100nm bands)
dataset_size_100nm_lichen=floor(nrow(pca_100nm_lichen)*0.80)
index<-sample(1:nrow(pca_100nm_lichen),size=dataset_size_100nm_lichen)
training_100nm_lichen<-pca_100nm_lichen[index,]
testing_100nm_lichen<-pca_100nm_lichen[-index,]

###random forest model for (100nm bands)
rf_100nm_lichen<-randomForest(PFT_2~.,data=training_100nm_lichen,mtry=5,ntree=2001,importance=TRUE)
result_100nm_lichen<-data.frame(testing_100nm_lichen$PFT_2,predict(rf_100nm_lichen,testing_100nm_lichen,type = "response"))
confusion_martrix_100nm_lichen<-as.data.frame(rf_100nm_lichen$confusion)

###extract error rate
error_rf_PCA_lichen<-as.data.frame(rf_lichen$err.rate[2001,1])
names(error_rf_PCA_lichen)[1]<-"all_bands"
error_rf_PCA_5nm_lichen<-as.data.frame(rf_5nm_lichen$err.rate[2001,1])
names(error_rf_PCA_5nm_lichen)[1]<-"5nm"
error_rf_PCA_10nm_lichen<-as.data.frame(rf_10nm_lichen$err.rate[2001,1])
names(error_rf_PCA_10nm_lichen)[1]<-"10nm"
error_rf_PCA_50nm_lichen<-as.data.frame(rf_50nm_lichen$err.rate[2001,1])
names(error_rf_PCA_50nm_lichen)[1]<-"50nm"
error_rf_PCA_100nm_lichen<-as.data.frame(rf_100nm_lichen$err.rate[2001,1])
names(error_rf_PCA_100nm_lichen)[1]<-"100nm"

###Make data frame from error rate
error_rate_PCA_lichen<-cbind(error_rf_PCA_lichen,
                         error_rf_PCA_5nm_lichen,
                         error_rf_PCA_10nm_lichen,
                         error_rf_PCA_50nm_lichen,
                         error_rf_PCA_100nm_lichen)
error_rate_PCA_lichen$category<-"Lichen"

##write to folder
write.csv(error_rate_PCA_lichen,"Model Scripts/Error Rates/PCA/Error/error_rate_PCA_lichen.csv",row.names = F)
write.csv(confusion_martrix_lichen,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_raw_lichen.csv",row.names = F)
write.csv(confusion_martrix_5nm_lichen,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_5nm_lichen.csv",row.names = F)
write.csv(confusion_martrix_10nm_lichen,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_10nm_lichen.csv",row.names = F)
write.csv(confusion_martrix_50nm_lichen,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_50nm_lichen.csv",row.names = F)
write.csv(confusion_martrix_100nm_lichen,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_100nm_lichen.csv",row.names = F)

