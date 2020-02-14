#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var/Headwall
library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)
library(hsdar)

##Reads in imagery so we can get the crs to create our raster later
Test_IMG_AV<-brick("Original_data/Test_imagery_AVIRIS/Test_IMG_AVIRIS")

##Marks raster as unrotated
Test_IMG_AV@rotated<-FALSE
Test_IMG_AV_latlong<-Test_IMG_AV%>%rasterToPoints()%>%as.data.frame()

##This is a dataframe with all predictors to be used in model building
Test_IMG_data_AV<-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/Test_IMG_data_AV.csv")

##Reads in spectral library with all predictors
##Each functional group has a total of 25 scans
alaskaSpecLib_data_AV<-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpecLib_data_AV.csv")

##Remove unwanted metadata from spectral library
alaskaSpecLib_data_AV [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model from our spectral library
rf_AV_Test_IMG <-randomForest(PFT_3~.,data=alaskaSpecLib_data_AV  ,mtry=20,ntree=2001,importance=TRUE)

##Lets save the confusion Matrix for these models
Test_IMG_ConfusionMatrix<-rf_AV_Test_IMG$confusion%>%
  as.data.frame()

##Lets save this confusion Matrix
write.csv(Test_IMG_ConfusionMatrix,"Test_Outputs/3_AV_imagery/2_Models/Test_IMG_ConfusionMatrix.csv",row.names = F)

##This function uses model from spectral library to predict the observations of each pixel in the imagery
Results_AV    <-predict(rf_AV_Test_IMG,Test_IMG_data_AV[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_AV<-as.data.frame(Results_AV)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_AV<-cbind(Results_AV,Test_IMG_AV_latlong[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_AV<-unique(as.data.frame(Results_AV$predicted)) 
Unique_AV$PFT_ID<-seq(1:nrow(Unique_AV))
names(Unique_AV)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_AV<-merge(Results_AV,Unique_AV, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Test_IMG_raster<-rasterFromXYZ(Results_AV, crs = crs(Test_IMG_AV)) 

##################################################Raster #1#####################################################
##Get values for each PFT
Shrub     <-subset(Unique_AV,Unique_AV$predicted=="Shrub")     %>%as.data.frame()%>%dplyr::select("PFT_ID")
Moss      <-subset(Unique_AV,Unique_AV$predicted=="Moss")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
Tree      <-subset(Unique_AV,Unique_AV$predicted=="Tree")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
Graminoid <-subset(Unique_AV,Unique_AV$predicted=="Graminoid") %>%as.data.frame()%>%dplyr::select("PFT_ID")
Lichen    <-subset(Unique_AV,Unique_AV$predicted=="Lichen")    %>%as.data.frame()%>%dplyr::select("PFT_ID")
Forb      <-subset(Unique_AV,Unique_AV$predicted=="Forb")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
DwarfShrub<-subset(Unique_AV,Unique_AV$predicted=="Dwarf Shrub")%>%as.data.frame()%>%dplyr::select("PFT_ID")

###Filters the image on each functional group
Test_IMG_Shrub     <-Test_IMG_raster==Shrub     [1,1]
Test_IMG_Moss      <-Test_IMG_raster==Moss      [1,1]
Test_IMG_Tree      <-Test_IMG_raster==Tree      [1,1]
Test_IMG_Graminoid <-Test_IMG_raster==Graminoid [1,1]
Test_IMG_Lichen    <-Test_IMG_raster==Lichen    [1,1]
Test_IMG_Forb      <-Test_IMG_raster==Forb      [1,1]
Test_IMG_DwarfShrub<-Test_IMG_raster==DwarfShrub[1,1]

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","mediumvioletred","gold","deepskyblue","saddlebrown","orange2","ivory3")

jpeg('Test_Outputs/3_AV_Imagery/2_Models/Test_IMG_AVIRIS Plot Prediction.jpg',width=1200, height=700)
plot(
  Test_IMG_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  xlab="Longitude", 
  ylab="Latitude"
)
legend(
  "right",
  legend = c("Graminoid","Shrub", "Dwarf Shrub","Tree","Forb","Moss","Lichen"),
  fill =chm_colors,
  border = FALSE,
  bty = "n",
  cex=1.3,
  xjust =1,
  horiz = FALSE,
  inset = -0.007,
  par(cex=0.4)
  
)             
dev.off()

#####writes out Rater layer created
##writeRaster(Results_AV_raster,
##filename ="Test_Outputs/2_AV_Imagery/2_Models/Results_AV_raster_Test_IMG", 
##format="GTiff", overwrite=TRUE)
