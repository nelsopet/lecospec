###################################AVIRIS_FLIGHT_LINES_Model####################################
library(randomForest)
library(rgdal)
library(raster)
library(dplyr)

##Reads in Imagery as multi layer raster
Clayton_AVIRIS_Tiff_Spatial<-brick("Imagery/Spectral_Subset_2019_05_15T22_29_34Z_Area0.dat")

##Marks raster as unrotated
Clayton_AVIRIS_Tiff_Spatial@rotated<-FALSE
#plot(Clayton_AVIRIS_Tiff_Spatial[[1]])

##read in dataframe for image
Clayton_AVIRIS_Tiff_df<-read.csv("Processed_spec/Imagery/Raw/Clayton_AVIRIS_Tiff_df.csv")

###reads in spectral library dataframes
alaskaSpecLib_AV                <-read.csv("Processed_spec/Imagery/Raw/alaskaSpecLib_AV.csv"        )
alaskaSpecLib_AV_plants         <-read.csv("Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants.csv" )
alaskaSpecLib_AV_equal05        <-read.csv("Processed_spec/Imagery/Raw/alaskaSpecLib_AV_equal05.csv")
alaskaSpecLib_AV_more05         <-read.csv("Processed_spec/Imagery/Raw/alaskaSpecLib_AV_more05.csv" )

##Removes unwanted metadata 
alaskaSpecLib_AV                [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_AV_plants         [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_AV_equal05        [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_AV_more05         [c("ScanID","PFT","PFT_3","area","Freq")] = NULL

##Converts to factor
alaskaSpecLib_AV        $PFT_2<-as.factor(alaskaSpecLib_AV        $PFT_2)
alaskaSpecLib_AV_plants $PFT_2<-as.factor(alaskaSpecLib_AV_plants $PFT_2)
alaskaSpecLib_AV_equal05$PFT_2<-as.factor(alaskaSpecLib_AV_equal05$PFT_2)
alaskaSpecLib_AV_more05 $PFT_2<-as.factor(alaskaSpecLib_AV_more05 $PFT_2)         

##For now I will use the whole dataset to generate models
rf_alldata_AV                <-randomForest(PFT_2~.,data=alaskaSpecLib_AV                ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_AV_plants         <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_plants         ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_AV_equal05        <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_equal05        ,mtry=5,ntree=2001,importance=TRUE)
rf_alldata_AV_more05         <-randomForest(PFT_2~.,data=alaskaSpecLib_AV_more05         ,mtry=5,ntree=2001,importance=TRUE)

##uses model from spectral library to predict images
Results_AV               <-predict(rf_alldata_AV               ,Clayton_AVIRIS_Tiff_df[-(1:2)])
Results_AV_plants        <-predict(rf_alldata_AV_plants        ,Clayton_AVIRIS_Tiff_df[-(1:2)])
Results_AV_equal05       <-predict(rf_alldata_AV_equal05       ,Clayton_AVIRIS_Tiff_df[-(1:2)])
Results_AV_more05        <-predict(rf_alldata_AV_more05        ,Clayton_AVIRIS_Tiff_df[-(1:2)])

###converts prediction from rf model to dataframe
Results_AV        <-as.data.frame(Results_AV        )
Results_AV_plants <-as.data.frame(Results_AV_plants )
Results_AV_equal05<-as.data.frame(Results_AV_equal05)
Results_AV_more05 <-as.data.frame(Results_AV_more05 )

names(Results_AV        )[1]<-"predicted"
names(Results_AV_plants )[1]<-"predicted"
names(Results_AV_equal05)[1]<-"predicted"
names(Results_AV_more05 )[1]<-"predicted"

## Grabs x, y values from original image and combines with unique values from prediction
Results_AV_dat        <-cbind(Results_AV         ,Clayton_AVIRIS_Tiff_df) %>% select(predicted,x,y)
Results_AV_plants_dat <-cbind(Results_AV_plants  ,Clayton_AVIRIS_Tiff_df) %>% select(predicted,x,y)
Results_AV_equal05_dat<-cbind(Results_AV_equal05 ,Clayton_AVIRIS_Tiff_df) %>% select(predicted,x,y)
Results_AV_more05_dat <-cbind(Results_AV_more05  ,Clayton_AVIRIS_Tiff_df) %>% select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_PFT_AV        <-unique(as.data.frame(Results_AV_dat        $predicted))
Unique_PFT_AV_plants <-unique(as.data.frame(Results_AV_plants_dat $predicted)) 
Unique_PFT_AV_equal05<-unique(as.data.frame(Results_AV_equal05_dat$predicted)) 
Unique_PFT_AV_more05 <-unique(as.data.frame(Results_AV_more05_dat $predicted)) 

Unique_PFT_AV        $PFT_ID<-seq(1:nrow(Unique_PFT_AV        ))
Unique_PFT_AV_plants $PFT_ID<-seq(1:nrow(Unique_PFT_AV_plants ))
Unique_PFT_AV_equal05$PFT_ID<-seq(1:nrow(Unique_PFT_AV_equal05))
Unique_PFT_AV_more05 $PFT_ID<-seq(1:nrow(Unique_PFT_AV_more05 ))

names(Unique_PFT_AV        )[1]<-"predicted"
names(Unique_PFT_AV_plants )[1]<-"predicted"
names(Unique_PFT_AV_equal05)[1]<-"predicted"
names(Unique_PFT_AV_more05 )[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_AV_PFT        <-merge(Results_AV_dat        ,Unique_PFT_AV        , by="predicted")%>% select(x,y,PFT_ID)
Results_AV_plants_PFT <-merge(Results_AV_plants_dat ,Unique_PFT_AV_plants , by="predicted")%>% select(x,y,PFT_ID)
Results_AV_equal05_PFT<-merge(Results_AV_equal05_dat,Unique_PFT_AV_equal05, by="predicted")%>% select(x,y,PFT_ID)
Results_AV_more05_PFT <-merge(Results_AV_more05_dat ,Unique_PFT_AV_more05 , by="predicted")%>% select(x,y,PFT_ID)

###This is another option however this combines all the layers into one dataframe to creat a rasterbrick
Results_AV_brick        <-merge(Results_AV_PFT         ,Clayton_AVIRIS_Tiff_df)
Results_AV_plants_brick <-merge(Results_AV_plants_PFT  ,Clayton_AVIRIS_Tiff_df)
Results_AV_equal05_brick<-merge(Results_AV_equal05_PFT ,Clayton_AVIRIS_Tiff_df)
Results_AV_more05_brick <-merge(Results_AV_more05_PFT  ,Clayton_AVIRIS_Tiff_df)

##Converts dataframe to a raster for predicted layer
Results_AV_raster        <-rasterFromXYZ(Results_AV_PFT        , crs = crs(Clayton_AVIRIS_Tiff_Spatial))
Results_AV_plants_raster <-rasterFromXYZ(Results_AV_plants_PFT , crs = crs(Clayton_AVIRIS_Tiff_Spatial))
Results_AV_equal05_raster<-rasterFromXYZ(Results_AV_equal05_PFT, crs = crs(Clayton_AVIRIS_Tiff_Spatial))
Results_AV_more05_raster <-rasterFromXYZ(Results_AV_more05_PFT , crs = crs(Clayton_AVIRIS_Tiff_Spatial))

#plot(Results_AV_raster        )
#plot(Results_AV_plants_raster )
#plot(Results_AV_equal05_raster)
#plot(Results_AV_more05_raster )


##Converts dataframe to a raster for all layers
Results_AV_raster_brick        <-rasterFromXYZ(Results_AV_brick        , crs = crs(Clayton_AVIRIS_Tiff_Spatial))
Results_AV_plants_raster_brick <-rasterFromXYZ(Results_AV_plants_brick , crs = crs(Clayton_AVIRIS_Tiff_Spatial))
Results_AV_equal05_raster_brick<-rasterFromXYZ(Results_AV_equal05_brick, crs = crs(Clayton_AVIRIS_Tiff_Spatial))
Results_AV_more05_raster_brick <-rasterFromXYZ(Results_AV_more05_brick , crs = crs(Clayton_AVIRIS_Tiff_Spatial))

#plot(Results_raster_brick        $PFT_ID )
#plot(Results_plants_raster_brick $PFT_ID )
#plot(Results_equal05_raster_brick$PFT_ID)
#plot(Results_more05_raster_brick $PFT_ID )

###writes out Rater layers
writeRaster(Results_AV_all_raster,
            filename ="processing imagery/processed_data/Model results/Results_AV_all_raster", 
            format="GTiff", overwrite=TRUE)

writeRaster(Results_AV_all_raster_brick,
            filename ="processing imagery/processed_data/Model results/Results_AV_all_raster", 
            format="GTiff", overwrite=TRUE)

