###################################Headwall_FLIGHT_LINES_Model####################################
library(randomForest)
library(rgdal)
library(raster)
library(dplyr)

##Reads in Imagery as multi layer raster
Clayton_Headwall_Tiff_Spatial<-brick("Imagery/raw_8580_rd_rf_or_Area0.dat")
plot(Clayton_Headwall_Tiff_Spatial[[1]])

##Convert to dataframe
Clayton_Headwall_Tiff_df<-rasterToPoints(Clayton_Headwall_Tiff_Spatial)%>%as.data.frame()

###reads in spectral library dataframes 
alaskaSpecLib_HDW        <-read.csv("Processed_spec/Imagery/Raw/alaskaSpecLib_HDW.csv       ")
alaskaSpecLib_HDW_plants <-read.csv("Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants.csv")
alaskaSpecLib_HDW_equa05 <-read.csv("Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal05.csv")
alaskaSpecLib_HDW_more05 <-read.csv("Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_more05.csv")

##Removes unwanted metadata  
alaskaSpecLib_HDW               [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_HDW_plants        [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_HDW_equa05        [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_HDW_more05        [c("ScanID","PFT","PFT_3","area","Freq")] = NULL

##Converts to factor
alaskaSpecLib_HDW               $PFT_2<-as.factor(alaskaSpecLib_HDW       $PFT_2)
alaskaSpecLib_HDW_plants        $PFT_2<-as.factor(alaskaSpecLib_HDW_plants$PFT_2)
alaskaSpecLib_HDW_equa05        $PFT_2<-as.factor(alaskaSpecLib_HDW_equa05$PFT_2)
alaskaSpecLib_HDW_more05        $PFT_2<-as.factor(alaskaSpecLib_HDW_more05$PFT_2)

##For now I will use the whole dataset to generate models
rfdata_HDW               <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW,mtry=5,ntree=2001,importance=TRUE)
rfdata_HDW_plants        <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_plants,mtry=5,ntree=2001,importance=TRUE)
rfdata_HDW_equa05        <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_equa05,mtry=5,ntree=2001,importance=TRUE)
rfdata_HDW_more05        <-randomForest(PFT_2~.,data=alaskaSpecLib_HDW_more05,mtry=5,ntree=2001,importance=TRUE)

##uses model from spectral library to predict images
Results_HDW               <-predict(rfdata_HDW               ,Clayton_Headwall_Tiff_df[-(1:2)])
Results_HDW_plants        <-predict(rfdata_HDW_plants        ,Clayton_Headwall_Tiff_df[-(1:2)])
Results_HDW_equa05        <-predict(rfdata_HDW_equa05        ,Clayton_Headwall_Tiff_df[-(1:2)])
Results_HDW_more05        <-predict(rfdata_HDW_more05        ,Clayton_Headwall_Tiff_df[-(1:2)])

###converts prediction from rf model to dataframe
Results_HDW       <-as.data.frame(Results_HDW       )
Results_HDW_plants<-as.data.frame(Results_HDW_plants)
Results_HDW_equa05<-as.data.frame(Results_HDW_equa05)
Results_HDW_more05<-as.data.frame(Results_HDW_more05)

names(Results_HDW       )[1]<-"predicted"
names(Results_HDW_plants)[1]<-"predicted"
names(Results_HDW_equa05)[1]<-"predicted"
names(Results_HDW_more05)[1]<-"predicted"

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_dat       <-cbind(Results_HDW       ,Clayton_Headwall_Tiff_df) %>% select(predicted,x,y)
Results_HDW_dat_plants<-cbind(Results_HDW_plants,Clayton_Headwall_Tiff_df) %>% select(predicted,x,y)
Results_HDW_dat_equa05<-cbind(Results_HDW_equa05,Clayton_Headwall_Tiff_df) %>% select(predicted,x,y)
Results_HDW_dat_more05<-cbind(Results_HDW_more05,Clayton_Headwall_Tiff_df) %>% select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_PFT_HDW       <-unique(as.data.frame(Results_HDW_dat       $predicted)) 
Unique_PFT_HDW_plants<-unique(as.data.frame(Results_HDW_dat_plants$predicted)) 
Unique_PFT_HDW_equa05<-unique(as.data.frame(Results_HDW_dat_equa05$predicted)) 
Unique_PFT_HDW_more05<-unique(as.data.frame(Results_HDW_dat_more05$predicted)) 

Unique_PFT_HDW       $PFT_ID<-seq(1:nrow(Unique_PFT_HDW       ))
Unique_PFT_HDW_plants$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_plants))
Unique_PFT_HDW_equa05$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_equa05))
Unique_PFT_HDW_more05$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_more05))

names(Unique_PFT_HDW       )[1]<-"predicted"
names(Unique_PFT_HDW_plants)[1]<-"predicted"
names(Unique_PFT_HDW_equa05)[1]<-"predicted"
names(Unique_PFT_HDW_more05)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_PFT       <-merge(Results_HDW_dat,Unique_PFT_HDW       , by="predicted")%>% select(x,y,PFT_ID)
Results_HDW_PFT_plants<-merge(Results_HDW_dat,Unique_PFT_HDW_plants, by="predicted")%>% select(x,y,PFT_ID)
Results_HDW_PFT_equa05<-merge(Results_HDW_dat,Unique_PFT_HDW_equa05, by="predicted")%>% select(x,y,PFT_ID)
Results_HDW_PFT_more05<-merge(Results_HDW_dat,Unique_PFT_HDW_more05, by="predicted")%>% select(x,y,PFT_ID)

###This is another option however this combines all the layers into one dataframe to creat a rasterbrick
#Results_HDW_brick       <-merge(Results_HDW_PFT       ,Clayton_Headwall_Tiff_df)
#Results_HDW_brick_plants<-merge(Results_HDW_PFT_plants,Clayton_Headwall_Tiff_df)
#Results_HDW_brick_equa05<-merge(Results_HDW_PFT_equa05,Clayton_Headwall_Tiff_df)
#Results_HDW_brick_more05<-merge(Results_HDW_PFT_more05,Clayton_Headwall_Tiff_df)

##Converts dataframe to a raster for predicted layer
Results_HDW_raster       <-rasterFromXYZ(Results_HDW_PFT       , crs = crs(Clayton_Headwall_Tiff_Spatial))
Results_HDW_raster_plants<-rasterFromXYZ(Results_HDW_PFT_plants, crs = crs(Clayton_Headwall_Tiff_Spatial))
Results_HDW_raster_equa05<-rasterFromXYZ(Results_HDW_PFT_equa05, crs = crs(Clayton_Headwall_Tiff_Spatial))
Results_HDW_raster_more05<-rasterFromXYZ(Results_HDW_PFT_more05, crs = crs(Clayton_Headwall_Tiff_Spatial))

plot(Results_HDW_raster       )
plot(Results_HDW_raster_plants)
plot(Results_HDW_raster_equa05)
plot(Results_HDW_raster_more05)

##Converts dataframe to a raster for all layers
#Results_HDW_raster_brick<-rasterFromXYZ(Results_HDW_brick, crs = crs(Clayton_Headwall_Tiff_Spatial))
#plot(Results_HDW_raster_brick$PFT_ID)

###writes out Rater layers

writeRaster(Clayton_Headwall_Tiff_Spatial[[1]],
            filename ="Processed_imagery/Headwall/Results/Clayton_Headwall_Tiff_Spatial[[1]]", 
            format="GTiff", overwrite=TRUE)

writeRaster(Results_HDW_raster,
            filename ="Processed_imagery/Headwall/Results/Results_HDW_raster", 
            format="GTiff", overwrite=TRUE)
writeRaster(Results_HDW_raster_plants,
            filename ="Processed_imagery/Headwall/Results/Results_HDW_raster_plants", 
            format="GTiff", overwrite=TRUE)
writeRaster(Results_HDW_raster_equa05,
            filename ="Processed_imagery/Headwall/Results/Results_HDW_raster_equa05", 
            format="GTiff", overwrite=TRUE)
writeRaster(Results_HDW_raster_more05,
            filename ="Processed_imagery/Headwall/Results/Results_HDW_raster_more05", 
            format="GTiff", overwrite=TRUE)


##writeRaster(Results_HDW_raster_brick,
##filename ="processing imagery/processed_data/Model results/Results_HDW_raster", 
##format="GTiff", overwrite=TRUE)

