library(pls)
library(randomForest)
setwd("/Alaska_Spectral_Library")

###reads in alaskaspeclib
alaskaSpecLib_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_bryo.csv")
alaskaSpecLib_5nm_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_5nm_bryo.csv")
alaskaSpecLib_10nm_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_10nm_bryo.csv")
alaskaSpecLib_50nm_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_50nm_bryo.csv")
alaskaSpecLib_100nm_bryo<-read.csv("processed spec/AlaskaSpecLib/alaskaSpecLib_100nm_bryo.csv")

## Remove unwanted metadata
alaskaSpecLib_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_5nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_10nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_50nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_100nm_bryo[c("ScanID","PFT","PFT_3","area")] = NULL

################################Model all bands########################################
##PCA (all bands)
pca_bryo<- prcomp(alaskaSpecLib_bryo[,-2152], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_bryo <- round(pca_bryo$sdev^2/sum(pca_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_bryo$PFT_2))
pca_bryo<-as.data.frame(cbind(names_species,pca_bryo$x[,pc6]))
names(pca_bryo)[1]<-paste("PFT_2")
pca_bryo$PFT_2<-as.factor(pca_bryo$PFT_2)

#Create training and testing dataset (all bands)
dataset_size_bryo=floor(nrow(pca_bryo)*0.80)
index<-sample(1:nrow(pca_bryo),size=dataset_size_bryo)
training_bryo<-pca_bryo[index,]
testing_bryo<-pca_bryo[-index,]

###random forest model for (plant and abiotic scans)
rf_bryo<-randomForest(PFT_2~.,data=training_bryo,mtry=5,ntree=2001,importance=TRUE)
result_bryo<-data.frame(testing_bryo$PFT_2,predict(rf_bryo,testing_bryo,type = "response"))
confusion_martrix_bryo<-as.data.frame(rf_bryo$confusion)

################################Model 5nm bands########################################
##PCA (5nm bands)
pca_5nm_bryo<- prcomp(alaskaSpecLib_5nm_bryo[,-432], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_5nm_bryo <- round(pca_5nm_bryo$sdev^2/sum(pca_5nm_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_5nm_bryo$PFT_2))
pca_5nm_bryo<-as.data.frame(cbind(names_species,pca_5nm_bryo$x[,pc6]))
names(pca_5nm_bryo)[1]<-paste("PFT_2")
pca_5nm_bryo$PFT_2<-as.factor(pca_5nm_bryo$PFT_2)

#Create training and testing dataset (5nm bands)
dataset_size_5nm_bryo=floor(nrow(pca_5nm_bryo)*0.80)
index<-sample(1:nrow(pca_5nm_bryo),size=dataset_size_5nm_bryo)
training_5nm_bryo<-pca_5nm_bryo[index,]
testing_5nm_bryo<-pca_5nm_bryo[-index,]

###random forest model for (5nm bands)
rf_5nm_bryo<-randomForest(PFT_2~.,data=training_5nm_bryo,mtry=5,ntree=2001,importance=TRUE)
result_5nm_bryo<-data.frame(testing_5nm_bryo$PFT_2,predict(rf_5nm_bryo,testing_5nm_bryo,type = "response"))
confusion_martrix_5nm_bryo<-as.data.frame(rf_5nm_bryo$confusion)

################################Model 10nm bands########################################
##PCA (10nm bands)
pca_10nm_bryo<- prcomp(alaskaSpecLib_10nm_bryo[,-217], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_10nm_bryo <- round(pca_10nm_bryo$sdev^2/sum(pca_10nm_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_10nm_bryo$PFT_2))
pca_10nm_bryo<-as.data.frame(cbind(names_species,pca_10nm_bryo$x[,pc6]))
names(pca_10nm_bryo)[1]<-paste("PFT_2")
pca_10nm_bryo$PFT_2<-as.factor(pca_10nm_bryo$PFT_2)

#Create training and testing dataset (10nm bands)
dataset_size_10nm_bryo=floor(nrow(pca_10nm_bryo)*0.80)
index<-sample(1:nrow(pca_10nm_bryo),size=dataset_size_10nm_bryo)
training_10nm_bryo<-pca_10nm_bryo[index,]
testing_10nm_bryo<-pca_10nm_bryo[-index,]

###random forest model for (10nm bands)
rf_10nm_bryo<-randomForest(PFT_2~.,data=training_10nm_bryo,mtry=5,ntree=2001,importance=TRUE)
result_10nm_bryo<-data.frame(testing_10nm_bryo$PFT_2,predict(rf_10nm_bryo,testing_10nm_bryo,type = "response"))
confusion_martrix_10nm_bryo<-as.data.frame(rf_10nm_bryo$confusion)

################################Model 50nm bands########################################
##PCA (50nm bands)
pca_50nm_bryo<- prcomp(alaskaSpecLib_50nm_bryo[,-45], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_50nm_bryo <- round(pca_50nm_bryo$sdev^2/sum(pca_50nm_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_50nm_bryo$PFT_2))
pca_50nm_bryo<-as.data.frame(cbind(names_species,pca_50nm_bryo$x[,pc6]))
names(pca_50nm_bryo)[1]<-paste("PFT_2")
pca_50nm_bryo$PFT_2<-as.factor(pca_50nm_bryo$PFT_2)

#Create training and testing dataset (50nm bands)
dataset_size_50nm_bryo=floor(nrow(pca_50nm_bryo)*0.80)
index<-sample(1:nrow(pca_50nm_bryo),size=dataset_size_50nm_bryo)
training_50nm_bryo<-pca_50nm_bryo[index,]
testing_50nm_bryo<-pca_50nm_bryo[-index,]

###random forest model for (50nm bands)
rf_50nm_bryo<-randomForest(PFT_2~.,data=training_50nm_bryo,mtry=5,ntree=2001,importance=TRUE)
result_50nm_bryo<-data.frame(testing_50nm_bryo$PFT_2,predict(rf_50nm_bryo,testing_50nm_bryo,type = "response"))
confusion_martrix_50nm_bryo<-as.data.frame(rf_50nm_bryo$confusion)

################################Model 100nm bands########################################
##PCA (100nm bands)
pca_100nm_bryo<- prcomp(alaskaSpecLib_100nm_bryo[,-23], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_100nm_bryo <- round(pca_100nm_bryo$sdev^2/sum(pca_100nm_bryo$sdev^2)*100)
pc6<-c(1:6)####fist 6 PC's
names_species<-as.data.frame(as.character(alaskaSpecLib_100nm_bryo$PFT_2))
pca_100nm_bryo<-as.data.frame(cbind(names_species,pca_100nm_bryo$x[,pc6]))
names(pca_100nm_bryo)[1]<-paste("PFT_2")
pca_100nm_bryo$PFT_2<-as.factor(pca_100nm_bryo$PFT_2)

#Create training and testing dataset (100nm bands)
dataset_size_100nm_bryo=floor(nrow(pca_100nm_bryo)*0.80)
index<-sample(1:nrow(pca_100nm_bryo),size=dataset_size_100nm_bryo)
training_100nm_bryo<-pca_100nm_bryo[index,]
testing_100nm_bryo<-pca_100nm_bryo[-index,]

###random forest model for (100nm bands)
rf_100nm_bryo<-randomForest(PFT_2~.,data=training_100nm_bryo,mtry=5,ntree=2001,importance=TRUE)
result_100nm_bryo<-data.frame(testing_100nm_bryo$PFT_2,predict(rf_100nm_bryo,testing_100nm_bryo,type = "response"))
confusion_martrix_100nm_bryo<-as.data.frame(rf_100nm_bryo$confusion)

###extract error rate
error_rf_PCA_bryo<-as.data.frame(rf_bryo$err.rate[2001,1])
names(error_rf_PCA_bryo)[1]<-"all_bands"
error_rf_PCA_5nm_bryo<-as.data.frame(rf_5nm_bryo$err.rate[2001,1])
names(error_rf_PCA_5nm_bryo)[1]<-"5nm"
error_rf_PCA_10nm_bryo<-as.data.frame(rf_10nm_bryo$err.rate[2001,1])
names(error_rf_PCA_10nm_bryo)[1]<-"10nm"
error_rf_PCA_50nm_bryo<-as.data.frame(rf_50nm_bryo$err.rate[2001,1])
names(error_rf_PCA_50nm_bryo)[1]<-"50nm"
error_rf_PCA_100nm_bryo<-as.data.frame(rf_100nm_bryo$err.rate[2001,1])
names(error_rf_PCA_100nm_bryo)[1]<-"100nm"

###Make data frame from error rate
error_rate_PCA_bryo<-cbind(error_rf_PCA_bryo,
                       error_rf_PCA_5nm_bryo,
                       error_rf_PCA_10nm_bryo,
                       error_rf_PCA_50nm_bryo,
                       error_rf_PCA_100nm_bryo)
error_rate_PCA_bryo$category<-"Bryophytes"

##write to folder
write.csv(error_rate_PCA_bryo,"Model Scripts/Error Rates/PCA/Error/error_rate_PCA_bryo.csv",row.names = F)
write.csv(confusion_martrix_bryo,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_raw_bryo.csv",row.names = F)
write.csv(confusion_martrix_5nm_bryo,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_5nm_bryo.csv",row.names = F)
write.csv(confusion_martrix_10nm_bryo,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_10nm_bryo.csv",row.names = F)
write.csv(confusion_martrix_50nm_bryo,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_50nm_bryo.csv",row.names = F)
write.csv(confusion_martrix_100nm_bryo,"Model Scripts/Error Rates/PCA/Confusion Matrix/Raw/confusion_martrix_100nm_bryo.csv",row.names = F)
