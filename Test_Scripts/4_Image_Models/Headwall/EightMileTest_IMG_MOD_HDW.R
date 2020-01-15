#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var/Headwall
library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)
library(hsdar)

##Reads in imagery so we can get the crs to create our raster later
EightMileTest_HDW<-brick("Original_data/Test_imagery_HDW/EightMile_TSTIMG")
EightMileTest_HDW_latlong<-EightMileTest_HDW%>%rasterToPoints()%>%as.data.frame()

##This is a dataframe with all predictors to be used in model building
EightMileTest_data_HDW<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/EightMileTest_data_HDW.csv")

##Reads in Shapefile for quadrat locations
EightMileTest_quadrats  <- readOGR("Original_data/Test_imagery_HDW","EightMile_TESTQUADS"  )

##Reads in spectral library with all predictors
##Each functional group has a total of 25 scans
alaskaSpecLib_data_HDW<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpecLib_data_HDW.csv")

##Remove unwanted metadata from spectral library
alaskaSpecLib_data_HDW [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model from our spectral library
rf_HDW_EightMileTest <-randomForest(PFT_3~.,data=alaskaSpecLib_data_HDW  ,mtry=20,ntree=2001,importance=TRUE)

##Lets save the confusion Matrix for these models
EightMileTest_ConfusionMatrix<-rf_HDW_EightMileTest$confusion%>%
  as.data.frame()

##Lets save this confusion Matrix
write.csv(EightMileTest_ConfusionMatrix,"Test_Outputs/2_HDW_imagery/2_Models/EightMileTest_ConfusionMatrix.csv",row.names = F)

##This function uses model from spectral library to predict the observations of each pixel in the imagery
Results_HDW    <-predict(rf_HDW_EightMileTest,EightMileTest_data_HDW[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW<-as.data.frame(Results_HDW)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW<-cbind(Results_HDW,EightMileTest_HDW_latlong[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW<-unique(as.data.frame(Results_HDW$predicted)) 
Unique_HDW$PFT_ID<-seq(1:nrow(Unique_HDW))
names(Unique_HDW)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW<-merge(Results_HDW,Unique_HDW, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
EightMileTest_raster<-rasterFromXYZ(Results_HDW, crs = crs(EightMileTest_HDW)) 

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
EightMileTest_Shrub     <-EightMileTest_raster==Shrub     [1,1]
EightMileTest_Moss      <-EightMileTest_raster==Moss      [1,1]
EightMileTest_Tree      <-EightMileTest_raster==Tree      [1,1]
EightMileTest_Graminoid <-EightMileTest_raster==Graminoid [1,1]
EightMileTest_Lichen    <-EightMileTest_raster==Lichen    [1,1]
EightMileTest_Forb      <-EightMileTest_raster==Forb      [1,1]
EightMileTest_DwarfShrub<-EightMileTest_raster==DwarfShrub[1,1]

##We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
EightMileTest_denom  <-EightMileTest_raster>=1

##DF OF METEDATA
EightMileTest_meta  <-EightMileTest_quadrats@data%>%as.data.frame()

#Creates object with the total Pixels for each quadrat
EightMileTest_Quad_totals  <-raster::extract(x=EightMileTest_denom  ,y=EightMileTest_quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")

#Creates object with the total Pixels for each Functional group
EightMileTest_Graminoid_sum <-raster::extract(x=EightMileTest_Graminoid ,y=EightMileTest_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_P" )
EightMileTest_DwarfShrub_sum<-raster::extract(x=EightMileTest_DwarfShrub,y=EightMileTest_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub_P")
EightMileTest_Moss_sum      <-raster::extract(x=EightMileTest_Moss      ,y=EightMileTest_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_P"      )
EightMileTest_Forb_sum      <-raster::extract(x=EightMileTest_Forb      ,y=EightMileTest_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb_P"      )
EightMileTest_Lichen_sum    <-raster::extract(x=EightMileTest_Lichen    ,y=EightMileTest_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_P"    )
EightMileTest_Shrub_sum     <-raster::extract(x=EightMileTest_Shrub     ,y=EightMileTest_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_P"     )
EightMileTest_Tree_sum      <-raster::extract(x=EightMileTest_Tree      ,y=EightMileTest_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_P"      )

##Lets combine the datframes created above
EightMileTest_HDW_pixeltotals<-Reduce(cbind,list(EightMileTest_Quad_totals
                                            ,EightMileTest_Graminoid_sum
                                            ,EightMileTest_DwarfShrub_sum
                                            ,EightMileTest_Moss_sum      
                                            ,EightMileTest_Forb_sum      
                                            ,EightMileTest_Lichen_sum    
                                            ,EightMileTest_Shrub_sum     
                                            ,EightMileTest_Tree_sum      ))

##Now we want to calculate the % cover for each Functional group in each quadrat
EightMileTest_HDW_PercentCover<-EightMileTest_HDW_pixeltotals[,2:8]/(EightMileTest_HDW_pixeltotals[,1])*100
EightMileTest_HDW_PercentCover<-EightMileTest_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(EightMileTest_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())

##Lets merge the metadata with these new dataframes
EightMileTest_HDW_PercentCover <-merge(EightMileTest_meta,  EightMileTest_HDW_PercentCover  ,by="CLASS_ID")
EightMileTest_HDW_PercentCover<-EightMileTest_HDW_PercentCover%>%
  arrange(CLASS_NAME)%>%
  dplyr::select(-CLASS_CLRS,-CLASS_ID)%>%
  mutate(CLASS_ID=rownames(EightMileTest_HDW_PercentCover))%>%dplyr::select(CLASS_ID,everything())

write.csv(EightMileTest_HDW_PercentCover ,"Test_Outputs/2_HDW_imagery/2_Models/EightMileTest_HDW_PercentCover_TST.csv")

##Import quadrat estimates made by jane and merge to estimates generated by models
##This was not doen for the test image
#EightMileTestEstimates<-read.csv("Original_data/Test_imagery_HDW/EightMileTestEstimates_Jane.csv")
#EightMileTestEstimate_Tab<-cbind(EightMileTest_HDW_PercentCover,EightMileTestEstimates)
#EightMileTestEstimate_Tab<-EightMileTestEstimate_Tab%>%dplyr::select(CLASS_ID,CLASS_NAME,Graminoid_P,
#                                                           Graminoid_A,DwarfShrub_P,DwarfShrub_A,Moss_P,Moss_A,Forb_P,
#                                                           Forb_A,Lichen_P,Lichen_A,Shrub_P,Shrub_A,Tree_P,Tree_A,Litter_A)
#
#write.csv(EightMileTestEstimate_Tab ,"Test_Outputs/2_HDW_imagery/2_Models/AK_imagery/EightMileTest_model_PercentCover.csv")

##Import quadrat estimates made by jane and merge to estimates generated by models
#EightMileTestEstimates<-read.csv("Original_data/Test_imagery_HDW/EightMileTestEstimates_Jane.csv")
#EightMileTestEstimate_Tab<-merge(EightMileTest_HDW_PercentCover,EightMileTestEstimates,by="CLASS_NAME")

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","mediumvioletred","gold","deepskyblue","saddlebrown","orange2","ivory3")

jpeg('Test_Outputs/2_HDW_Imagery/2_Models/EightMileTest Plot Prediction.jpg',width=1200, height=700)
plot(
  EightMileTest_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  xlab="Longitude", 
  ylab="Latitude"
)
plot(EightMileTest_quadrats,border="white",lwd=2,add=TRUE)
legend(
  "right",
  legend = c("Graminoid","Moss","Dwarf Shrub","Shrub","Lichen","Forb","Tree"),
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
            ##filename ="Test_Outputs/2_HDW_Imagery/2_Models/Results_HDW_raster_EightMileTest", 
            ##format="GTiff", overwrite=TRUE)
