###########################Headwall_Flight_Lines_Model####################################
library(randomForest)
library(rgdal)

##read in dataframe for image
Clayton_Headwall_Tiff<-read.csv("processing imagery/processed_data/Processing/Clayton_Headwall_Tiff.csv")

###reads in spectral libraries 
alaskaSpecLib_HDW_all        <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_HDW.csv")
alaskaSpecLib_HDW_plants     <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_HDW_plants.csv")
alaskaSpecLib_HDW_lichen     <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_HDW_lichen.csv")
alaskaSpecLib_HDW_bryo       <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_HDW_bryo.csv")
alaskaSpecLib_HDW_lichen_bryo<-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_HDW_lichen_bryo.csv")
alaskaSpecLib_HDW_vascular   <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_HDW_vascular.csv")

##Removes unwanted metadata  
alaskaSpecLib_HDW_all        [c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_HDW_plants     [c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_HDW_lichen     [c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_HDW_bryo       [c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_HDW_lichen_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_HDW_vascular   [c("ScanID","PFT","PFT_3","area")] = NULL

##Converts to factor
alaskaSpecLib_HDW_all        $PFT_2<-as.factor(alaskaSpecLib_HDW            $PFT_2)
alaskaSpecLib_HDW_plants     $PFT_2<-as.factor(alaskaSpecLib_HDW_plants     $PFT_2)
alaskaSpecLib_HDW_lichen     $PFT_2<-as.factor(alaskaSpecLib_HDW_lichen     $PFT_2)
alaskaSpecLib_HDW_bryo       $PFT_2<-as.factor(alaskaSpecLib_HDW_bryo       $PFT_2)
alaskaSpecLib_HDW_lichen_bryo$PFT_2<-as.factor(alaskaSpecLib_HDW_lichen_bryo$PFT_2)
alaskaSpecLib_HDW_vascular   $PFT_2<-as.factor(alaskaSpecLib_HDW_vascular   $PFT_2)

##For now I will use the whole dataset to generate models
rf_alldata_HDW_all        <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_all        ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_HDW_plants     <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_plants     ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_HDW_lichen     <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_lichen     ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_HDW_bryo       <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_bryo       ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_HDW_lichen_bryo<-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_lichen_bryo,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_HDW_vascular   <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_vascular   ,mtry=5,ntree=2001,importance=TRUE)

##uses model from spectral library to predict images
Results_HDW_all        <-predict(rf_alldata_HDW_all        ,Clayton_HDWIRIS_Tiff)
Results_HDW_plants     <-predict(rf_alldata_HDW_plants     ,Clayton_HDWIRIS_Tiff)
Results_HDW_lichen     <-predict(rf_alldata_HDW_lichen     ,Clayton_HDWIRIS_Tiff)
Results_HDW_bryo       <-predict(rf_alldata_HDW_bryo       ,Clayton_HDWIRIS_Tiff)
Results_HDW_lichen_bryo<-predict(rf_alldata_HDW_lichen_bryo,Clayton_HDWIRIS_Tiff)
Results_HDW_vascular   <-predict(rf_alldata_HDW_vascular   ,Clayton_HDWIRIS_Tiff)

##Writes out prediction as GTiffs 