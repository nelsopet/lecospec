###################################AVIRIS_FLIGHT_LINES_Model####################################
library(randomForest)
library(rgdal)
library(sf)
library(raster)

##read in dataframe for image
Clayton_AVIRIS_Tiff_Spatial<-readGDAL(fname="Imagery/Spectral_Subset_2019_05_15T22_29_34Z_Area0.dat")
Clayton_AVIRIS_Tiff<-read.csv("processing imagery/processed_data/Processing/Clayton_AVIRIS_Tiff.csv")

##Imports all raster rasters as one raster object (i.e. all bands)
Clayton_AVIRIS_Tiff_Spatial_all_layers<-brick("Imagery/Spectral_Subset_2019_05_15T22_29_34Z_Area0.dat")

##Marks raster as unrotated
Clayton_AVIRIS_Tiff_Spatial_all_layers@rotated<-FALSE

###reads in spectral libraries 
alaskaSpecLib_AV_all        <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_AV.csv")
alaskaSpecLib_AV_plants     <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_AV_plants.csv")
alaskaSpecLib_AV_lichen     <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_AV_lichen.csv")
alaskaSpecLib_AV_bryo       <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_AV_bryo.csv")
alaskaSpecLib_AV_lichen_bryo<-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_AV_lichen_bryo.csv")
alaskaSpecLib_AV_vascular   <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_AV_vascular.csv")

##Removes unwanted metadata  
alaskaSpecLib_AV_all        [c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_AV_plants     [c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_AV_lichen     [c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_AV_bryo       [c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_AV_lichen_bryo[c("ScanID","PFT","PFT_3","area")] = NULL
alaskaSpecLib_AV_vascular   [c("ScanID","PFT","PFT_3","area")] = NULL

##Converts to factor
alaskaSpecLib_AV_all        $PFT_2<-as.factor(alaskaSpecLib_AV_all        $PFT_2)
alaskaSpecLib_AV_plants     $PFT_2<-as.factor(alaskaSpecLib_AV_plants     $PFT_2)
alaskaSpecLib_AV_lichen     $PFT_2<-as.factor(alaskaSpecLib_AV_lichen     $PFT_2)
alaskaSpecLib_AV_bryo       $PFT_2<-as.factor(alaskaSpecLib_AV_bryo       $PFT_2)
alaskaSpecLib_AV_lichen_bryo$PFT_2<-as.factor(alaskaSpecLib_AV_lichen_bryo$PFT_2)
alaskaSpecLib_AV_vascular   $PFT_2<-as.factor(alaskaSpecLib_AV_vascular   $PFT_2)

##For now I will use the whole dataset to generate models
rf_alldata_AV_all        <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_all        ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_AV_plants     <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_plants     ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_AV_lichen     <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_lichen     ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_AV_bryo       <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_bryo       ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_AV_lichen_bryo<-randomForest(PFT_2~.,data=alaskaSpecLib_AV_lichen_bryo,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_AV_vascular   <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_vascular   ,mtry=5,ntree=2001,importance=TRUE)

##pls_alldata_AV_all        <-plsgenomics::pls.lda(Xtrain=alaskaSpecLib_AV_all[,1:862],Ytrain=alaskaSpecLib_AV_all$PFT_2, ncomp=6)

##uses model from spectral library to predict images
Results_AV_all        <-predict(rf_alldata_AV_all        ,Clayton_AVIRIS_Tiff)
Results_AV_plants     <-predict(rf_alldata_AV_plants     ,Clayton_AVIRIS_Tiff)
Results_AV_lichen     <-predict(rf_alldata_AV_lichen     ,Clayton_AVIRIS_Tiff)
Results_AV_bryo       <-predict(rf_alldata_AV_bryo       ,Clayton_AVIRIS_Tiff)
Results_AV_lichen_bryo<-predict(rf_alldata_AV_lichen_bryo,Clayton_AVIRIS_Tiff)
Results_AV_vascular   <-predict(rf_alldata_AV_vascular   ,Clayton_AVIRIS_Tiff)

##Turn prediction output into data frame)
Results_AV_all        <-as.data.frame(Results_AV_all        )
names(Results_AV_all        )[1]<-"predicted"
Results_AV_plants     <-as.data.frame(Results_AV_plants     )
names(Results_AV_plants     )[1]<-"predicted"
Results_AV_lichen     <-as.data.frame(Results_AV_lichen     )
names(Results_AV_lichen     )[1]<-"predicted"
Results_AV_bryo       <-as.data.frame(Results_AV_bryo       )
names(Results_AV_bryo       )[1]<-"predicted"
Results_AV_lichen_bryo<-as.data.frame(Results_AV_lichen_bryo)
names(Results_AV_lichen_bryo)[1]<-"predicted"
Results_AV_vascular   <-as.data.frame(Results_AV_vascular   )
names(Results_AV_vascular   )[1]<-"predicted"

#add coordinate data from original image
Results_AV_all<-cbind(Results_AV_all,coordinates(Clayton_AVIRIS_Tiff_Spatial),Clayton_AVIRIS_Tiff)
Results_AV_all[1]<-NULL

Results_AV_all_sp<-

rasterFromXYZ(Results_AV_all_spdf[1:2],Results_AV_all_spdf)
Unique_PFT_AV<-Results_AV_all %>% dplyr::select(predicted) %>% unique()
Unique_PFT_AV$PFT_ID<-seq(1:nrow(Unique_PFT_AV))
Results_AV_all_num<-merge(Results_AV_all,Unique_PFT_AV, by="predicted") %>% dplyr::select(x,y,PFT_ID)
Results_AV_all_nm_spdf<-SpatialPointsDataFrame(coords = Results_AV_all_num[c("x","y")], data = Results_AV_all_num,proj4string = crs(Clayton_AVIRIS_Tiff_Spatial))
Results_AV_all_nm_spdf_ras<-raster(Results_AV_all_nm_spdf, nrows=23 , ncols=22)
Results_AV_all_nm_spdf_ras<-rasterFromXYZ(Results_AV_all_nm_spdf)

plot(Results_AV_all_nm_spdf_ras)

##converts to spaitial_points_df 
Results_AV_all_spdf <- SpatialPointsDataFrame(coords = Results_AV_all[c("x","y")], data = Results_AV_all,
                               proj4string = crs(Clayton_AVIRIS_Tiff_Spatial))

Results_AV_all_spdf <- SpatialPixelsDataFrame(points = Results_AV_all[c("x","y")], data = Results_AV_all,tolerance = 0.000856898,
                                              ##proj4string = crs(Clayton_AVIRIS_Tiff_Spatial))
##Change to raster
Results_AV_all_raster<- raster(Results_AV_all_spdf, nrows=22, ncols= 23)

###SET VALUES
values(Results_AV_all_raster)<-1:ncell(Results_AV_all_raster)

###
plot(Results_AV_all_raster)
plot(Results_AV_all_spdf, add=TRUE)
image(Results_AV_all_raster)

###
writeRaster(Results_AV_all_raster,
            filename ="processing imagery/processed_data/Model results/Results_AV_all_raster", 
            format="GTiff", overwrite=TRUE)

