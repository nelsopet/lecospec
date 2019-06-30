###################################AVIRIS_FLIGHT_LINES_Model####################################
library(randomForest)
library(rgdal)
library(raster)
library(dplyr)

##Reads in Imagery as multi layer raster
Clayton_AVIRIS_Tiff_Spatial<-brick("Imagery/Spectral_Subset_2019_05_15T22_29_34Z_Area0.dat")

##Marks raster as unrotated
Clayton_AVIRIS_Tiff_Spatial@rotated<-FALSE

##read in dataframe for image
Clayton_AVIRIS_Tiff_df<-read.csv("processing imagery/processed_data/Processing/Clayton_AVIRIS_Tiff_df.csv")

###reads in spectral library dataframes 
alaskaSpecLib_AV_all        <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_AV_all.csv")

##Removes unwanted metadata  
alaskaSpecLib_AV_all        [c("ScanID","PFT","PFT_3","area")] = NULL

##Converts to factor
alaskaSpecLib_AV_all        $PFT_2<-as.factor(alaskaSpecLib_AV_all        $PFT_2)

##For now I will use the whole dataset to generate models
rf_alldata_AV_all        <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_all        ,mtry=5,ntree=2001,importance=TRUE)

##uses model from spectral library to predict images
Results_AV_all        <-predict(rf_alldata_AV_all        ,Clayton_AVIRIS_Tiff_df[-(1:2)])

###converts prediction from rf model to dataframe
Results_AV_all<-as.data.frame(Results_AV_all)
names(Results_AV_all)[1]<-"predicted"

## Grabs x, y values from original image and combines with unique values from prediction 
Results_AV_all_dat<-cbind(Results_AV_all,Clayton_AVIRIS_Tiff_df) %>% select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_PFT_AV<-unique(as.data.frame(Results_AV_all_dat$predicted)) 
Unique_PFT_AV$PFT_ID<-seq(1:nrow(Unique_PFT_AV))
names(Unique_PFT_AV)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_AV_all_PFT<-merge(Results_AV_all_dat,Unique_PFT_AV, by="predicted")%>% select(x,y,PFT_ID)

###This is another option however this combines all the layers into one dataframe to creat a rasterbrick
Results_AV_all_brick<-merge(Results_AV_all_PFT,Clayton_AVIRIS_Tiff_df)

##Converts dataframe to a raster for predicted layer
Results_AV_all_raster<-rasterFromXYZ(Results_AV_all_PFT, crs = crs(Clayton_AVIRIS_Tiff_Spatial))
plot(Results_AV_all_raster)


##Converts dataframe to a raster for all layers
Results_AV_all_raster_brick<-rasterFromXYZ(Results_AV_all_brick, crs = crs(Clayton_AVIRIS_Tiff_Spatial))
plot(Results_AV_all_raster_brick$PFT_ID)

###writes out Rater layers
writeRaster(Results_AV_all_raster,
            filename ="processing imagery/processed_data/Model results/Results_AV_all_raster", 
            format="GTiff", overwrite=TRUE)

writeRaster(Results_AV_all_raster_brick,
            filename ="processing imagery/processed_data/Model results/Results_AV_all_raster", 
            format="GTiff", overwrite=TRUE)

