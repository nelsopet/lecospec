library(plsgenomics)
library(tidyverse)
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

###pls.lda model
model_plants<-pls.lda(Xtrain =alaskaSpecLib_plants[,(-2152)], Ytrain = alaskaSpecLib_plants[,(2152)], ncomp = 3,nruncv = 0)


##pred<-alaskaSpecLib_plants[-(1:150),-(2152)]
##resp<-alaskaSpecLib_plants[-(1:150),(2152)]
##model_plants<-pls.lda(Xtrain =pred, Ytrain = resp, ncomp = 5,nruncv = 0)
##model_plants<-pls.lda(Xtrain =alaskaSpecLib_plants[-(1:150),-(2152)], Ytrain = alaskaSpecLib_plants[-(1:150),(2152)],Xtest = alaskaSpecLib_plants[(1:150),-(2152)],ncomp = 5,nruncv = 0)
##model_plants<-pls.lda(Xtrain =training_plants[,-(2152)], Ytrain = training_plants[,(2152)],Xtest = testing_plants[,-(2152)],ncomp = 5,nruncv = 0)

