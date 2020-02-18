#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var/Headwall
library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)

##Reads in imagery and all predictors
Clayton_Test_HDW<-brick("Original_data/Test_imagery_HDW/Clayton_Test_clipped_HDW")
Clayton_Test_HDW_df<-brick("Original_data/Test_imagery_HDW/Clayton_Test_clipped_HDW")%>%rasterToPoints()%>%as.data.frame()

Clayton_Test_data_HDW<-read.csv("Outputs/2_HDW_Imagery/1_processing/Clayton_Test_data_HDW.csv")

##Reads in Shapefile for quadrat locations
Clayton_Test_quadrats  <- readOGR("Original_data/Test_imagery_HDW","Clayton_Test_quadrats"  )

##Reads in specctral library witrh all predictors
##Each functional group has a total of 25 scans and imagery 
alaskaSpecLib_data_HDW<-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpecLib_data_HDW.csv")

##Remove unwanted metadata from spectral library
alaskaSpecLib_data_HDW [c("ScanID","PFT","PFT_2","area","Freq1","Freq2","X")] = NULL

##We can build randomforest model
rf_HDW_Clayton_Test <-randomForest(PFT_3~.,data=alaskaSpecLib_data_HDW  ,mtry=20,ntree=2001,importance=TRUE)

##Lets save the confusion Matrix for these models
Clayton_Test_ConfusionMatrix<-rf_HDW_Clayton_Test$confusion%>%
  as.data.frame()

##Lets save this confusion Matrix
write.csv(Clayton_Test_ConfusionMatrix,"Outputs/2_HDW_imagery/2_Models/Clayton_Test_ConfusionMatrix.csv")

##uses model from spectral library to predict images
Results_HDW    <-predict(rf_HDW_Clayton_Test,Clayton_Test_data_HDW[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW<-as.data.frame(Results_HDW)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW<-cbind(Results_HDW,Clayton_Test_HDW_df[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW<-unique(as.data.frame(Results_HDW$predicted)) 
Unique_HDW$PFT_ID<-seq(1:nrow(Unique_HDW))
names(Unique_HDW)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW<-merge(Results_HDW,Unique_HDW, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Clayton_Test_raster<-rasterFromXYZ(Results_HDW, crs = crs(Clayton_Test_HDW))

##################################################Raster #1#####################################################
##Get values for each PFT
Shrub     <-subset(Unique_HDW,Unique_HDW$predicted=="Shrub")     %>%as.data.frame()%>%dplyr::select("PFT_ID")
Moss      <-subset(Unique_HDW,Unique_HDW$predicted=="Moss")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
Tree      <-subset(Unique_HDW,Unique_HDW$predicted=="Tree")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
Graminoid <-subset(Unique_HDW,Unique_HDW$predicted=="Graminoid") %>%as.data.frame()%>%dplyr::select("PFT_ID")
Lichen    <-subset(Unique_HDW,Unique_HDW$predicted=="Lichen")    %>%as.data.frame()%>%dplyr::select("PFT_ID")
Forb      <-subset(Unique_HDW,Unique_HDW$predicted=="Forb")      %>%as.data.frame()%>%dplyr::select("PFT_ID")
DwarfShrub<-subset(Unique_HDW,Unique_HDW$predicted=="DwarfShrub")%>%as.data.frame()%>%dplyr::select("PFT_ID")

###Filters the image on each functional group
Clayton_Test_Shrub     <-Clayton_Test_raster==Shrub     [1,1]
Clayton_Test_Moss      <-Clayton_Test_raster==Moss      [1,1]
Clayton_Test_Tree      <-Clayton_Test_raster==Tree      [1,1]
Clayton_Test_Graminoid <-Clayton_Test_raster==Graminoid [1,1]
Clayton_Test_Lichen    <-Clayton_Test_raster==Lichen    [1,1]
Clayton_Test_Forb      <-Clayton_Test_raster==Forb      [1,1]
Clayton_Test_DwarfShrub<-Clayton_Test_raster==DwarfShrub[1,1]

##We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
Clayton_Test_denom  <-Clayton_Test_raster>=1

##DF OF METEDATA
Clayton_Test_meta  <-Clayton_Test_quadrats@data%>%as.data.frame()

#Creates object with the total Pixels for each quadrat
Clayton_Test_Quad_totals  <-raster::extract(x=Clayton_Test_denom  ,y=Clayton_Test_quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")

#Creates object with the total Pixels for each Functional group
Clayton_Test_Graminoid_sum <-raster::extract(x=Clayton_Test_Graminoid ,y=Clayton_Test_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_P" )
Clayton_Test_DwarfShrub_sum<-raster::extract(x=Clayton_Test_DwarfShrub,y=Clayton_Test_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub_P")
Clayton_Test_Moss_sum      <-raster::extract(x=Clayton_Test_Moss      ,y=Clayton_Test_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_P"      )
Clayton_Test_Forb_sum      <-raster::extract(x=Clayton_Test_Forb      ,y=Clayton_Test_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb_P"      )
Clayton_Test_Lichen_sum    <-raster::extract(x=Clayton_Test_Lichen    ,y=Clayton_Test_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_P"    )
Clayton_Test_Shrub_sum     <-raster::extract(x=Clayton_Test_Shrub     ,y=Clayton_Test_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_P"     )
Clayton_Test_Tree_sum      <-raster::extract(x=Clayton_Test_Tree      ,y=Clayton_Test_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_P"      )

##Lets combine the datframes created above
Clayton_Test_HDW_pixeltotals<-Reduce(cbind,list(Clayton_Test_Quad_totals
                                             ,Clayton_Test_Graminoid_sum
                                             ,Clayton_Test_Moss_sum      
                                             ,Clayton_Test_Forb_sum      
                                             ,Clayton_Test_Lichen_sum    
                                             ,Clayton_Test_Shrub_sum     
                                             ,Clayton_Test_Tree_sum      ))

##Now we want to calculate the % cover for each Functional group in each quadrat
Clayton_Test_HDW_PercentCover<-Clayton_Test_HDW_pixeltotals[,2:7]/(Clayton_Test_HDW_pixeltotals[,1])*100
Clayton_Test_HDW_PercentCover<-Clayton_Test_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(Clayton_Test_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())

##Lets merge the metadata with these new dataframes
Clayton_Test_HDW_PercentCover <-merge(Clayton_Test_meta,  Clayton_Test_HDW_PercentCover  ,by="CLASS_ID")
Clayton_Test_HDW_PercentCover<-Clayton_Test_HDW_PercentCover%>%
  arrange(CLASS_NAME)%>%
  dplyr::select(-CLASS_CLRS,-CLASS_ID)%>%
  mutate(CLASS_ID=rownames(Clayton_Test_HDW_PercentCover))%>%dplyr::select(CLASS_ID,everything())

##Import quadrat estimates made by jane and merge to estimates generated by models
Clayton_TestEstimates<-read.csv("Original_data/Test_imagery_HDW/Clayton_TestEstimates_Jane.csv")
Clayton_TestEstimate_Tab<-cbind(Clayton_Test_HDW_PercentCover,Clayton_TestEstimates)
Clayton_TestEstimate_Tab<-Clayton_TestEstimate_Tab%>%dplyr::select(CLASS_ID,CLASS_NAME,Graminoid_P,
                                                             Graminoid_A,Dwarf.Shrub_A,Moss_P,Moss_A,Forb_P,
                                                             Forb_A,Lichen_P,Lichen_A,Shrub_P,Shrub_A,Tree_P,Tree_A )

write.csv(Clayton_Test_HDW_PercentCover,"Outputs/2_HDW_imagery/2_Models/Clayton_Test_model_PercentCover.csv")

##Import quadrat estimates made by jane and merge to estimates generated by models
#Clayton_TestEstimates<-read.csv("Original_data/Test_imagery_HDW/Clayton_TestEstimates_Jane.csv")
#Clayton_TestEstimate_Tab<-merge(Clayton_Test_HDW_PercentCover,Clayton_TestEstimates,by="CLASS_NAME")

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown","orange2","wheat1","red")

jpeg('Outputs/2_HDW_Imagery/2_Models/Clayton_Test Plot of VIs Model Prediction.jpg',width=1200, height=700)
plot(
  Clayton_Test_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  xlab="Longitude", 
  ylab="Latitude"
)
plot(Clayton_Test_quadrats,border="red",lwd=2,add=TRUE)
legend(
  "topright",
  legend = c("Dwarf Shrub","Shrub","moss","Lichen","Tree","Forb","Graminoid","Quadrats"),
  fill =chm_colors,
  border = FALSE,
  bty = "n",
  cex=1,
  xjust =1,
  horiz = FALSE,
  inset = -0.005,
  par(cex=0.4)
  
)             
dev.off()

#####writes out Rater layer created
##writeRaster(Results_HDW_raster,
filename ="Outputs/2_HDW_Imagery/2_Models/Results_HDW_raster_Clayton_Test", 
format="GTiff", overwrite=TRUE)



