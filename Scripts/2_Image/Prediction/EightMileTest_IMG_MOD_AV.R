#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var/Headwall
library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)
library(hsdar)
library(randomcoloR)
library(randomForestExplainer)

##Reads in imagery so we can get the crs to create our raster later
EightMileTest_AV<-brick("Original_data/AVIRIS/EightMile_TEST_AV")

##Marks raster as unrotated
EightMileTest_AV@rotated<-FALSE
EightMileTest_AV_latlong<-EightMileTest_AV%>%rasterToPoints()%>%as.data.frame()

##This is a dataframe with all predictors to be used in model building
EightMileTest_IMG_data_AV<-read.csv("Outputs/2_Imagery/AVIRIS/Processing/EightMileTest_IMG_data_AV.csv")

##Lets load our random Forest model with the 50 most important variables (PFT_3)
load("Outputs/1_Field_spec/2_Models/AVIRIS/rf_AV50.rda")
##rf_AV50

##This function uses model from spectral library to predict the observations of each pixel in the imagery
Results_AV    <-predict(rf_AV50,EightMileTest_IMG_data_AV[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_AV<-as.data.frame(Results_AV)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_AV<-cbind(Results_AV,EightMileTest_AV_latlong[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_AV<-unique(as.data.frame(Results_AV$predicted)) 
Unique_AV$PFT_ID<-seq(1:nrow(Unique_AV))
names(Unique_AV)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_AV<-merge(Results_AV,Unique_AV, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
EightMileTest_AV_raster<-rasterFromXYZ(Results_AV, crs = crs(EightMileTest_AV)) 

##################################################Raster #1#####################################################
Graminoid_Sedge   <-subset(Unique_AV,Unique_AV$predicted=="Graminoid_Sedge")   %>%as.data.frame()%>%dplyr::select("PFT_ID")
Lichen_Yellow     <-subset(Unique_AV,Unique_AV$predicted=="Lichen_Yellow")     %>%as.data.frame()%>%dplyr::select("PFT_ID")
Shrub_Other       <-subset(Unique_AV,Unique_AV$predicted=="Shrub_Other")       %>%as.data.frame()%>%dplyr::select("PFT_ID")
Dwarf_Shrub_Needl <-subset(Unique_AV,Unique_AV$predicted=="Dwarf_Shrub_Needl") %>%as.data.frame()%>%dplyr::select("PFT_ID")
Dwarf_Shrub_Broad5<-subset(Unique_AV,Unique_AV$predicted=="Dwarf_Shrub_Broad5")%>%as.data.frame()%>%dplyr::select("PFT_ID")
Lichen_Dark       <-subset(Unique_AV,Unique_AV$predicted=="Lichen_Dark")       %>%as.data.frame()%>%dplyr::select("PFT_ID")
Shrub_Salix       <-subset(Unique_AV,Unique_AV$predicted=="Shrub_Salix")       %>%as.data.frame()%>%dplyr::select("PFT_ID")
Forb              <-subset(Unique_AV,Unique_AV$predicted=="Forb")              %>%as.data.frame()%>%dplyr::select("PFT_ID")
Tree_Needle       <-subset(Unique_AV,Unique_AV$predicted=="Tree_Needle")       %>%as.data.frame()%>%dplyr::select("PFT_ID")
Moss_Pleurocarp   <-subset(Unique_AV,Unique_AV$predicted=="Moss_Pleurocarp")   %>%as.data.frame()%>%dplyr::select("PFT_ID")
Shrub_Alder       <-subset(Unique_AV,Unique_AV$predicted=="Shrub_Alder")       %>%as.data.frame()%>%dplyr::select("PFT_ID")
Graminoid_Grass   <-subset(Unique_AV,Unique_AV$predicted=="Graminoid_Grass")   %>%as.data.frame()%>%dplyr::select("PFT_ID")
Lichen_Light      <-subset(Unique_AV,Unique_AV$predicted=="Lichen_Light")      %>%as.data.frame()%>%dplyr::select("PFT_ID")

###Filters the image on each functional group
EightMileTest_Graminoid_Sedge   <-EightMileTest_AV_raster==Graminoid_Sedge   [1,1]
EightMileTest_Lichen_Yellow     <-EightMileTest_AV_raster==Lichen_Yellow     [1,1]
EightMileTest_Shrub_Other       <-EightMileTest_AV_raster==Shrub_Other       [1,1]
EightMileTest_Dwarf_Shrub_Needle<-EightMileTest_AV_raster==Dwarf_Shrub_Needl [1,1]
EightMileTest_Dwarf_Shrub_Broad5<-EightMileTest_AV_raster==Dwarf_Shrub_Broad5[1,1]
EightMileTest_Lichen_Dark       <-EightMileTest_AV_raster==Lichen_Dark       [1,1]
EightMileTest_Shrub_Salix       <-EightMileTest_AV_raster==Shrub_Salix       [1,1]
EightMileTest_Forb              <-EightMileTest_AV_raster==Forb              [1,1]
EightMileTest_Tree_Needle       <-EightMileTest_AV_raster==Tree_Needle       [1,1]
EightMileTest_Moss_Pleurocarp   <-EightMileTest_AV_raster==Moss_Pleurocarp   [1,1]
EightMileTest_Shrub_Alder       <-EightMileTest_AV_raster==Shrub_Alder       [1,1]
EightMileTest_Graminoid_Grass   <-EightMileTest_AV_raster==Graminoid_Grass   [1,1]
EightMileTest_Lichen_Light      <-EightMileTest_AV_raster==Lichen_Light      [1,1]

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <-distinctColorPalette(nrow(Unique_AV))
jpeg('Outputs/2_Imagery/AVIRIS/Prediction/EightMileTest_AV_Pred50.jpg',width=1200, height=700)
plot(
  EightMileTest_AV_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  xlab="Longitude", 
  ylab="Latitude"
)
legend(
  "right",
  legend = c(paste(Unique_AV$predicted)),
  fill =chm_colors,
  border = FALSE,
  bty = "n",
  cex=0.8,
  xjust =1,
  horiz = FALSE,
  inset = -0.0009,
  par(cex=0.4)
  
)             
dev.off()
