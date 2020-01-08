#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var/Headwall
library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)
library(hsdar)

##Reads in imagery so we can get the crs to create our raster later
Test_IMG_HDW<-brick("Original_data/Test_imagery_HDW/Test_IMG_clipped_HDW")
Test_IMG_HDW_latlong<-Test_IMG_HDW%>%rasterToPoints()%>%as.data.frame()

##This is a dataframe with all predictors to be used in model building
Test_IMG_data_HDW<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/Test_IMG_data_HDW.csv")

##Reads in Shapefile for quadrat locations
Test_IMG_quadrats  <- readOGR("Original_data/Test_imagery_HDW","Test_IMG_quads"  )

##Reads in spectral library with all predictors
##Each functional group has a total of 25 scans
alaskaSpecLib_data_HDW<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpecLib_data_HDW.csv")

##Remove unwanted metadata from spectral library
alaskaSpecLib_data_HDW [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model from our spectral library
rf_HDW_Test_IMG <-randomForest(PFT_3~.,data=alaskaSpecLib_data_HDW  ,mtry=20,ntree=2001,importance=TRUE)

##Lets save the confusion Matrix for these models
Test_IMG_ConfusionMatrix<-rf_HDW_Test_IMG$confusion%>%
  as.data.frame()

##Lets save this confusion Matrix
write.csv(Test_IMG_ConfusionMatrix,"Test_Outputs/2_HDW_imagery/2_Models/AK_imagery/Test_IMG_ConfusionMatrix.csv",row.names = F)

##This function uses model from spectral library to predict the observations of each pixel in the imagery
Results_HDW    <-predict(rf_HDW_Test_IMG,Test_IMG_data_HDW[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW<-as.data.frame(Results_HDW)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW<-cbind(Results_HDW,Test_IMG_HDW_latlong[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW<-unique(as.data.frame(Results_HDW$predicted)) 
Unique_HDW$PFT_ID<-seq(1:nrow(Unique_HDW))
names(Unique_HDW)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW<-merge(Results_HDW,Unique_HDW, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Test_IMG_raster<-rasterFromXYZ(Results_HDW, crs = crs(Test_IMG_HDW)) 

##################################################Raster #1#####################################################
##Get values for each PFT
Shrub     <-subset(Unique_HDW,Unique_HDW$predicted=="Shrub")     %>%as.data.frame()%>%dplyr::select("PFT_ID")
Moss      <-subset(Unique_HDW,Unique_HDW$predicted=="Moss")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
Tree      <-subset(Unique_HDW,Unique_HDW$predicted=="Tree")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
Graminoid <-subset(Unique_HDW,Unique_HDW$predicted=="Graminoid") %>%as.data.frame()%>%dplyr::select("PFT_ID")
Lichen    <-subset(Unique_HDW,Unique_HDW$predicted=="Lichen")    %>%as.data.frame()%>%dplyr::select("PFT_ID")
Forb      <-subset(Unique_HDW,Unique_HDW$predicted=="Forb")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
DwarfShrub<-subset(Unique_HDW,Unique_HDW$predicted=="Dwarf Shrub")%>%as.data.frame()%>%dplyr::select("PFT_ID")

###Filters the image on each functional group
Test_IMG_Shrub     <-Test_IMG_raster==Shrub     [1,1]
Test_IMG_Moss      <-Test_IMG_raster==Moss      [1,1]
Test_IMG_Tree      <-Test_IMG_raster==Tree      [1,1]
Test_IMG_Graminoid <-Test_IMG_raster==Graminoid [1,1]
Test_IMG_Lichen    <-Test_IMG_raster==Lichen    [1,1]
Test_IMG_Forb      <-Test_IMG_raster==Forb      [1,1]
Test_IMG_DwarfShrub<-Test_IMG_raster==DwarfShrub[1,1]

##We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
Test_IMG_denom  <-Test_IMG_raster>=1

##DF OF METEDATA
Test_IMG_meta  <-Test_IMG_quadrats@data%>%as.data.frame()

#Creates object with the total Pixels for each quadrat
Test_IMG_Quad_totals  <-raster::extract(x=Test_IMG_denom  ,y=Test_IMG_quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")

#Creates object with the total Pixels for each Functional group
Test_IMG_Graminoid_sum <-raster::extract(x=Test_IMG_Graminoid ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_P" )
Test_IMG_DwarfShrub_sum<-raster::extract(x=Test_IMG_DwarfShrub,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub_P")
Test_IMG_Moss_sum      <-raster::extract(x=Test_IMG_Moss      ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_P"      )
Test_IMG_Forb_sum      <-raster::extract(x=Test_IMG_Forb      ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb_P"      )
Test_IMG_Lichen_sum    <-raster::extract(x=Test_IMG_Lichen    ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_P"    )
Test_IMG_Shrub_sum     <-raster::extract(x=Test_IMG_Shrub     ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_P"     )
Test_IMG_Tree_sum      <-raster::extract(x=Test_IMG_Tree      ,y=Test_IMG_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_P"      )

##Lets combine the datframes created above
Test_IMG_HDW_pixeltotals<-Reduce(cbind,list(Test_IMG_Quad_totals
                                            ,Test_IMG_Graminoid_sum
                                            ,Test_IMG_DwarfShrub_sum
                                            ,Test_IMG_Moss_sum      
                                            ,Test_IMG_Forb_sum      
                                            ,Test_IMG_Lichen_sum    
                                            ,Test_IMG_Shrub_sum     
                                            ,Test_IMG_Tree_sum      ))

##Now we want to calculate the % cover for each Functional group in each quadrat
Test_IMG_HDW_PercentCover<-Test_IMG_HDW_pixeltotals[,2:8]/(Test_IMG_HDW_pixeltotals[,1])*100
Test_IMG_HDW_PercentCover<-Test_IMG_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(Test_IMG_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())

##Lets merge the metadata with these new dataframes
Test_IMG_HDW_PercentCover <-merge(Test_IMG_meta,  Test_IMG_HDW_PercentCover  ,by="CLASS_ID")
Test_IMG_HDW_PercentCover<-Test_IMG_HDW_PercentCover%>%
  arrange(CLASS_NAME)%>%
  dplyr::select(-CLASS_CLRS,-CLASS_ID)%>%
  mutate(CLASS_ID=rownames(Test_IMG_HDW_PercentCover))%>%dplyr::select(CLASS_ID,everything())

##Import quadrat estimates made by jane and merge to estimates generated by models
##This was not doen for the test image
#Test_IMGEstimates<-read.csv("Original_data/Test_imagery_HDW/Test_IMGEstimates_Jane.csv")
#Test_IMGEstimate_Tab<-cbind(Test_IMG_HDW_PercentCover,Test_IMGEstimates)
#Test_IMGEstimate_Tab<-Test_IMGEstimate_Tab%>%dplyr::select(CLASS_ID,CLASS_NAME,Graminoid_P,
#                                                           Graminoid_A,DwarfShrub_P,DwarfShrub_A,Moss_P,Moss_A,Forb_P,
#                                                           Forb_A,Lichen_P,Lichen_A,Shrub_P,Shrub_A,Tree_P,Tree_A,Litter_A)
#
#write.csv(Test_IMGEstimate_Tab ,"Test_Outputs/2_HDW_imagery/2_Models/AK_imagery/Test_IMG_model_PercentCover.csv")

##Import quadrat estimates made by jane and merge to estimates generated by models
#Test_IMGEstimates<-read.csv("Original_data/Test_imagery_HDW/Test_IMGEstimates_Jane.csv")
#Test_IMGEstimate_Tab<-merge(Test_IMG_HDW_PercentCover,Test_IMGEstimates,by="CLASS_NAME")

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","mediumvioletred","gold","deepskyblue","saddlebrown","orange2","ivory3")

jpeg('Test_Outputs/2_HDW_Imagery/2_Models/Test_IMG Plot Prediction.jpg',width=1200, height=700)
plot(
  Test_IMG_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  xlab="Longitude", 
  ylab="Latitude"
)
plot(Test_IMG_quadrats,border="white",lwd=2,add=TRUE)
legend(
  "right",
  legend = c("Graminoid","Tree", "Dwarf Shrub","Shrub","Forb","Moss","Lichen"),
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
##writeRaster(Results_HDW_raster,
            ##filename ="Test_Outputs/2_HDW_Imagery/2_Models/Results_HDW_raster_Test_IMG", 
            ##format="GTiff", overwrite=TRUE)
