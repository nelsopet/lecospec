#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_VIs/Headwall_VIs
library(randomForest)
library(raster)
library(tidyverse)
library(rasterVis)

##Reads in test imagery
Clayton_test_HDW<-brick("Original_data/Test_imagery/Clayton_test_HDW")%>%rasterToPoints()%>%as.data.frame()

##Reads in VIs for specctral library each functional group has a total of 25 scans and imagery 
alaskaSpeclib_HDW_VIs_equal25<-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_VIs_equal25.csv")
Clayton_test_HDW_VIs <-read.csv("Outputs/2_HDW_Imagery/1_processing/Clayton_test_HDW_VIs.csv" )

##Remove unwanted metadata from spectral library
alaskaSpeclib_HDW_VIs_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_HDW_VIs         <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_VIs_equal25  ,mtry=3,ntree=500,importance=TRUE)

##uses model from spectral library to predict images
Results_HDW_VIs    <-predict(rf_HDW_VIs        ,Clayton_test_HDW_VIs[-1:-2])


##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW_VIs<-as.data.frame(Results_HDW_VIs)%>%'names<-'("predicted")


## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_VIs<-cbind(Results_HDW_VIs,Clayton_test_HDW[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW_VIs<-unique(as.data.frame(Results_HDW_VIs$predicted)) 
Unique_HDW_VIs$PFT_ID<-seq(1:nrow(Unique_HDW_VIs))
names(Unique_HDW_VIs)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_VIs<-merge(Results_HDW_VIs,Unique_HDW_VIs, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Results_HDW_VIs_raster<-rasterFromXYZ(Results_HDW_VIs, crs = crs(Clayton_test_HDW))%>%plot()


##Then I add an extra attribute that will be used for the legend
##tar<-levels(Results_HDW_VIs_raster)[[1]]
##tar[["Functional groups"]]<-Unique_HDW_VIs$predicted
##levels(Results_HDW_VIs_raster)<-tar


##Then I plot using levelplot from the rasterVis package
##levelplot(Results_HDW_VIs_raster,main = "Plot of VIs Model")

###writes out Rater layer created
writeRaster(Results_HDW_VIs_raster,
            filename ="Outputs/2_HDW_Imagery/2_Models/Results_HDW_VIs_raster", 
            format="GTiff", overwrite=TRUE)
###save plot as a jpeg
jpeg('Outputs/2_HDW_Imagery/2_Models/Plot of VIs Model.jpg',width=1000, height=700)
levelplot(Results_HDW_VIs_raster,main = "Plot of VIs Model")
dev.off()