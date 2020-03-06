#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_VIs/Headwall_VIs
library(randomForest)
library(raster)
library(tidyverse)

##Reads in imagery and all predictors
EightMile_HDW<-brick("Original_data/Test_imagery_HDW/EightMile_clipped_HDW")
EightMile_HDW_raw<-brick("Original_data/Test_imagery_HDW/EightMile_clipped_HDW")%>%rasterToPoints()%>%as.data.frame()
EightMile_HDW_05nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_05nm.csv")
EightMile_HDW_10nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_10nm.csv")
EightMile_HDW_VIs <-read.csv("Outputs/2_HDW_Imagery/1_processing/EightMile_HDW_VIs.csv" )

##Let's merge these dataframes


##Reads in Shapefile for quadrat locations
EightMile_quadrats  <- readOGR("Original_data/Test_imagery_HDW","EightMile_quadrats"  )

##Reads in specctral library witrh all predictors
##Each functional group has a total of 25 scans and imagery 
alaskaSpecLib_smooth_equal25  <-read.csv("Outputs/1_Field_spec/1_Processing/alaskaSpecLib_smooth_equal25.csv")
alaskaSpeclib_HDW_05nm_equal25<-read.csv("Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_05nm_equal25.csv")
alaskaSpeclib_HDW_10nm_equal25<-read.csv("Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_10nm_equal25.csv")
alaskaSpeclib_HDW_VIs_equal25 <-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_VIs_equal25.csv")


##Remove unwanted metadata from spectral library
alaskaSpeclib_HDW_VIs_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_HDW_VIs_EightMile         <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_VIs_equal25  ,mtry=3,ntree=500,importance=TRUE)

##Lets save the confusion Matrix for these models
EightMile_ConfusionMatrix_VIs<-rf_HDW_VIs_EightMile$confusion%>%
  as.data.frame()

##Lets save this confusion Matrix
write.csv(EightMile_ConfusionMatrix,"Outputs/2_HDW_imagery/2_Models/EightMile_ConfusionMatrix.csv")

##uses model from spectral library to predict images
Results_HDW_VIs    <-predict(rf_HDW_VIs_EightMile,EightMile_HDW_VIs[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW_VIs<-as.data.frame(Results_HDW_VIs)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_VIs<-cbind(Results_HDW_VIs,EightMile_HDW_VIs[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW_VIs<-unique(as.data.frame(Results_HDW_VIs$predicted)) 
Unique_HDW_VIs$PFT_ID<-seq(1:nrow(Unique_HDW_VIs))
names(Unique_HDW_VIs)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_VIs<-merge(Results_HDW_VIs,Unique_HDW_VIs, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
EightMile_raster<-rasterFromXYZ(Results_HDW_VIs, crs = crs(EightMile_HDW))

##################################################Raster #1#####################################################
###Filters the image on each functional group
EightMile_Graminoid <-EightMile_raster==1
EightMile_DwarfShrub<-EightMile_raster==2
EightMile_Moss      <-EightMile_raster==3
EightMile_Forb      <-EightMile_raster==4
EightMile_Lichen    <-EightMile_raster==5
EightMile_Shrub     <-EightMile_raster==6
EightMile_Tree      <-EightMile_raster==7

##We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
EightMile_denom  <-EightMile_raster>=1

##DF OF METEDATA
EightMile_meta  <-EightMile_quadrats@data%>%as.data.frame()

#Creates object with the total Pixels for each quadrat
EightMile_Quad_totals  <-raster::extract(x=EightMile_denom  ,y=EightMile_quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")

#Creates object with the total Pixels for each Functional group
EightMile_Graminoid_sum <-raster::extract(x=EightMile_Graminoid ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_P" )
EightMile_DwarfShrub_sum<-raster::extract(x=EightMile_DwarfShrub,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub_P")
EightMile_Moss_sum      <-raster::extract(x=EightMile_Moss      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_P"      )
EightMile_Forb_sum      <-raster::extract(x=EightMile_Forb      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb_P"      )
EightMile_Lichen_sum    <-raster::extract(x=EightMile_Lichen    ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_P"    )
EightMile_Shrub_sum     <-raster::extract(x=EightMile_Shrub     ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_P"     )
EightMile_Tree_sum      <-raster::extract(x=EightMile_Tree      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_P"      )

##Lets combine the datframes created above
EightMile_HDW_pixeltotals<-Reduce(cbind,list(EightMile_Quad_totals
                                            ,EightMile_Graminoid_sum 
                                            ,EightMile_DwarfShrub_sum
                                            ,EightMile_Moss_sum      
                                            ,EightMile_Forb_sum      
                                            ,EightMile_Lichen_sum    
                                            ,EightMile_Shrub_sum     
                                            ,EightMile_Tree_sum      ))

##Now we want to calculate the % cover for each Functional group in each quadrat
EightMile_HDW_PercentCover<-EightMile_HDW_pixeltotals[,2:8]/(EightMile_HDW_pixeltotals[,1])*100
EightMile_HDW_PercentCover<-EightMile_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(EightMile_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())

##Lets merge the metadata with these new dataframes
EightMile_HDW_PercentCover <-merge(EightMile_meta,  EightMile_HDW_PercentCover  ,by="CLASS_ID")
EightMile_HDW_PercentCover<-EightMile_HDW_PercentCover%>%
  arrange(CLASS_NAME)%>%
  dplyr::select(-CLASS_CLRS,-CLASS_ID)%>%
  mutate(CLASS_ID=rownames(EightMile_HDW_PercentCover))%>%dplyr::select(CLASS_ID,everything())

##Import quadrat estimates made by jane and merge to estimates generated by models
EightMileEstimates<-read.csv("Original_data/Test_imagery_HDW/EightMileEstimates_Jane.csv")
EightMileEstimate_Tab<-cbind(EightMile_HDW_PercentCover,EightMileEstimates)
EightMileEstimate_Tab<-EightMileEstimate_Tab%>%dplyr::select(CLASS_ID,CLASS_NAME,Graminoid_P,
                                                           Graminoid_A,DwarfShrub_P,Dwarf.Shrub_A,Moss_P,Moss_A,Forb_P,
                                                           Forb_A,Lichen_P,Lichen_A,Shrub_P,Shrub_A,Tree_P,Tree_A )

write.csv(EightMile_HDW_PercentCover,"Outputs/2_HDW_imagery/2_Models/EightMile_model_PercentCover.csv")

##Import quadrat estimates made by jane and merge to estimates generated by models
#EightMileEstimates<-read.csv("Original_data/Test_imagery_HDW/EightMileEstimates_Jane.csv")
#EightMileEstimate_Tab<-merge(EightMile_HDW_PercentCover,EightMileEstimates,by="CLASS_NAME")

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown","orange2","wheat1","red")

jpeg('Outputs/2_HDW_Imagery/2_Models/EightMile Plot of VIs Model Prediction.jpg',width=1200, height=700)
plot(
  EightMile_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  xlab="Longitude", 
  ylab="Latitude"
)
plot(EightMile_quadrats,border="red",lwd=2,add=TRUE)
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
##writeRaster(Results_HDW_VIs_raster,
            filename ="Outputs/2_HDW_Imagery/2_Models/Results_HDW_VIs_raster_EightMile", 
            format="GTiff", overwrite=TRUE)



