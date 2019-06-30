###################################Headwall_FLIGHT_LINES_Model####################################
library(randomForest)
library(rgdal)
library(raster)
library(dplyr)

##Reads in Imagery as multi layer raster
Clayton_Headwall_Tiff_Spatial<-brick("Imagery/raw_8580_rd_rf_or_Area0.dat")

##Convert to dataframe
Clayton_Headwall_Tiff_df<-rasterToPoints(Clayton_Headwall_Tiff_Spatial)%>%as.data.frame()

###reads in spectral library dataframes 
alaskaSpecLib_HDW_all        <-read.csv("processing imagery/processed_data/Processing/alaskaSpecLib_HDW_all.csv")

##Removes unwanted metadata  
alaskaSpecLib_HDW_all        [c("ScanID","PFT","PFT_3","area")] = NULL

##Converts to factor
alaskaSpecLib_HDW_all        $PFT_2<-as.factor(alaskaSpecLib_HDW_all        $PFT_2)

##For now I will use the whole dataset to generate models
rf_alldata_HDW_all        <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_all,mtry=5,ntree=2001,importance=TRUE)

##uses model from spectral library to predict images
Results_HDW_all        <-predict(rf_alldata_HDW_all        ,Clayton_Headwall_Tiff_df[-(1:2)])

###converts prediction from rf model to dataframe
Results_HDW_all<-as.data.frame(Results_HDW_all)
names(Results_HDW_all)[1]<-"predicted"

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_all_dat<-cbind(Results_HDW_all,Clayton_Headwall_Tiff_df) %>% select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_PFT_HDW<-unique(as.data.frame(Results_HDW_all_dat$predicted)) 
Unique_PFT_HDW$PFT_ID<-seq(1:nrow(Unique_PFT_HDW))
names(Unique_PFT_HDW)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_all_PFT<-merge(Results_HDW_all_dat,Unique_PFT_HDW, by="predicted")%>% select(x,y,PFT_ID)

###This is another option however this combines all the layers into one dataframe to creat a rasterbrick
#Results_HDW_all_brick<-merge(Results_HDW_all_PFT,Clayton_Headwall_Tiff_df)

##Converts dataframe to a raster for predicted layer
Results_HDW_all_raster<-rasterFromXYZ(Results_HDW_all_PFT, crs = crs(Clayton_Headwall_Tiff_Spatial))
plot(Results_HDW_all_raster)


##Converts dataframe to a raster for all layers
#Results_HDW_all_raster_brick<-rasterFromXYZ(Results_HDW_all_brick, crs = crs(Clayton_Headwall_Tiff_Spatial))
#plot(Results_HDW_all_raster_brick$PFT_ID)

###writes out Rater layers
writeRaster(Results_HDW_all_raster,
            filename ="processing imagery/processed_data/Model results/Results_HDW_all_raster", 
            format="GTiff", overwrite=TRUE)

##writeRaster(Results_HDW_all_raster_brick,
##filename ="processing imagery/processed_data/Model results/Results_HDW_all_raster", 
##format="GTiff", overwrite=TRUE)

