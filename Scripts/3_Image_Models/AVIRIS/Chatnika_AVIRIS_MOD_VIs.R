#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_VIs/Headwall_VIs
library(randomForest)
library(raster)
library(tidyverse)

#Reads in Imagery
Chatnika_clipped_AV<-brick("Original_data/Test_imagery_AVIRIS/Chatnika_clipped_AV")
Chatnika_clipped_HDW<-brick("Original_data/Test_imagery_HDW/Chatnika_clipped_HDW")

##MAKES SURE PROJECTIONS ARE THE SAME
crs(Chatnika_clipped_HDW)<-crs(Chatnika_clipped_AV)

##Marks raster as unrotated
Chatnika_clipped_AV@rotated<-FALSE

##Converts to a dataframe
Chatnika_clipped_AV<-rasterToPoints(Chatnika_clipped_AV)%>% as.data.frame()

##Reads in VIs for specctral library each functional group has a total of 25 scans and imagery 
alaskaSpeclib_VIs_equal25<-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_VIs_equal25.csv")

Chatnika_clipped_AV_VIs <-read.csv("Outputs/3_AV_Imagery/1_processing/Chatnika_clipped_AV_VIs.csv" )

##Remove unwanted metadata from spectral library
alaskaSpeclib_VIs_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_AV_VIs         <-randomForest(PFT_3~.,data=alaskaSpeclib_VIs_equal25  ,mtry=3,ntree=500,importance=TRUE)

##uses model from spectral library to predict images
Results_AV_VIs    <-predict(rf_AV_VIs        ,Chatnika_clipped_AV_VIs[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_AV_VIs<-as.data.frame(Results_AV_VIs)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_AV_VIs<-cbind(Results_AV_VIs,Chatnika_clipped_AV[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_AV_VIs<-unique(as.data.frame(Results_AV_VIs$predicted)) 
Unique_AV_VIs$PFT_ID<-seq(1:nrow(Unique_AV_VIs))
names(Unique_AV_VIs)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_AV_VIs<-merge(Results_AV_VIs,Unique_AV_VIs, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Chatnika_raster<-rasterFromXYZ(Results_AV_VIs, crs = crs(Chatnika_clipped_AV))

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown")

jpeg('Outputs/3_AV_Imagery/2_Models/AK_image/Chatnika Plot of vIs Model Prediction_AVIRIS.jpg',width=1200, height=700)
plot(
  Chatnika_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors,
  box= FALSE,
)
plot(extent(Chatnika_clipped_HDW),lwd=4,add=T)
legend(
  "top",
  legend = c("Dwarf Shrub","Shrub","moss","Forb","Graminoid"),
  fill =chm_colors,
  border = FALSE,
  bty = "n",
  cex=2,
  xjust =1,
  horiz = TRUE,
  inset = -0.005,
  par(cex=0.4)
)             
dev.off()




