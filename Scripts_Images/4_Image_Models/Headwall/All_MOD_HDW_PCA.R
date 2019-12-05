#################################PCA modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_pca/Headwall_pca
library(randomForest)
library(raster)
library(tidyverse)
library(rgdal)

##Reads in image as dataframe
Chatnika_HDW  <-brick("Original_data/Test_imagery_HDW/Chatnika_clipped_HDW"      )
EightMile_HDW <-brick("Original_data/Test_imagery_HDW/EightMile_clipped_HDW"     )
MurphyDome_HDW<-brick("Original_data/Test_imagery_HDW/MurphyDome_clipped_HDW"    )

##Reads in Shapefile for quadrat locations
Chatnika_quadrats  <- readOGR("Original_data/Test_imagery_HDW","Chatnika_quadrats"  )
EightMile_quadrats <- readOGR("Original_data/Test_imagery_HDW","EightMile_quadrats" )
MurphyDome_quadrats<- readOGR("Original_data/Test_imagery_HDW","MurphyDome_quadrats")

##Reads in PCA for specctral library each functional group has a total of 25 scans
HDW_PCAspeclib<-read.csv("Outputs/2_HDW_Imagery/1_Processing/HDW_PCAspeclib.csv")

##Reads in PCA for imagery
Chatnika_HDW_PCA     <- read.csv("Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_PCA.csv"  )
EightMile_HDW_PCA    <- read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_PCA.csv" )
MurphyDome_HDW_PCA   <- read.csv("Outputs/2_HDW_Imagery/1_Processing/MurphyDome_HDW_PCA.csv")

##Remove unwanted metadata from spectral library
HDW_PCAspeclib [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_HDW_PCA <-randomForest(PFT_3~.,data=HDW_PCAspeclib  ,mtry=3,ntree=301,importance=TRUE)


##Uses model from spectral library to predict images
Results_Chatnika_HDW_PCA  <-predict(rf_HDW_PCA       ,Chatnika_HDW_PCA  [-(1:2)])
Results_EightMile_HDW_PCA <-predict(rf_HDW_PCA       ,EightMile_HDW_PCA [-(1:2)])
Results_MurphyDome_HDW_PCA<-predict(rf_HDW_PCA       ,MurphyDome_HDW_PCA[-(1:2)])

##Converts prediction from rf model to dataframe and changes column name to predicted
Results_Chatnika_HDW_PCA  <-as.data.frame(Results_Chatnika_HDW_PCA  )%>%'names<-'("predicted")
Results_EightMile_HDW_PCA <-as.data.frame(Results_EightMile_HDW_PCA )%>%'names<-'("predicted")
Results_MurphyDome_HDW_PCA<-as.data.frame(Results_MurphyDome_HDW_PCA)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_Chatnika_HDW_PCA  <-cbind(Results_Chatnika_HDW_PCA  ,Chatnika_HDW_PCA  [1:2]) %>% dplyr::select(predicted,x,y)
Results_EightMile_HDW_PCA <-cbind(Results_EightMile_HDW_PCA ,EightMile_HDW_PCA [1:2]) %>% dplyr::select(predicted,x,y)
Results_MurphyDome_HDW_PCA<-cbind(Results_MurphyDome_HDW_PCA,MurphyDome_HDW_PCA[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Chatnika_Unique_HDW_PCA<-unique(as.data.frame(Results_Chatnika_HDW_PCA$predicted)) 
Chatnika_Unique_HDW_PCA$PFT_ID<-seq(1:nrow(Chatnika_Unique_HDW_PCA))
names(Chatnika_Unique_HDW_PCA)[1]<-"predicted"

EightMile_Unique_HDW_PCA<-unique(as.data.frame(Results_EightMile_HDW_PCA $predicted)) 
EightMile_Unique_HDW_PCA$PFT_ID<-seq(1:nrow(EightMile_Unique_HDW_PCA))
names(EightMile_Unique_HDW_PCA)[1]<-"predicted"

MurphyDome_Unique_HDW_PCA<-unique(as.data.frame(Results_MurphyDome_HDW_PCA$predicted)) 
MurphyDome_Unique_HDW_PCA$PFT_ID<-seq(1:nrow(MurphyDome_Unique_HDW_PCA))
names(MurphyDome_Unique_HDW_PCA)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_Chatnika_HDW_PCA  <-merge(Results_Chatnika_HDW_PCA  ,Chatnika_Unique_HDW_PCA  , by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_EightMile_HDW_PCA <-merge(Results_EightMile_HDW_PCA ,EightMile_Unique_HDW_PCA , by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_MurphyDome_HDW_PCA<-merge(Results_MurphyDome_HDW_PCA,MurphyDome_Unique_HDW_PCA, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Chatnika_raster   <-rasterFromXYZ(Results_Chatnika_HDW_PCA  , crs = crs(Chatnika_HDW  ))
EightMile_raster  <-rasterFromXYZ(Results_EightMile_HDW_PCA , crs = crs(EightMile_HDW ))
MurphyDome_raster<-rasterFromXYZ(Results_MurphyDome_HDW_PCA , crs = crs(MurphyDome_HDW))

##################################################Raster #1#####################################################
###Filters the image on each functional group
Chatnika_Graminoid <-Chatnika_raster==1
Chatnika_DwarfShrub<-Chatnika_raster==2
Chatnika_Moss      <-Chatnika_raster==3
Chatnika_Forb      <-Chatnika_raster==4
Chatnika_Lichen    <-Chatnika_raster==5
Chatnika_Shrub     <-Chatnika_raster==6
Chatnika_Tree      <-Chatnika_raster==7

EightMile_Graminoid <-EightMile_raster==1
EightMile_DwarfShrub<-EightMile_raster==2
EightMile_Moss      <-EightMile_raster==3
EightMile_Forb      <-EightMile_raster==4
EightMile_Lichen    <-EightMile_raster==5
EightMile_Shrub     <-EightMile_raster==6
EightMile_Tree      <-EightMile_raster==7

MurphyDome_Graminoid <-MurphyDome_raster==1
MurphyDome_DwarfShrub<-MurphyDome_raster==2
MurphyDome_Moss      <-MurphyDome_raster==3
MurphyDome_Forb      <-MurphyDome_raster==4
MurphyDome_Lichen    <-MurphyDome_raster==5
MurphyDome_Shrub     <-MurphyDome_raster==6
MurphyDome_Tree      <-MurphyDome_raster==7

##We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
Chatnika_denom  <-Chatnika_raster>=1
EightMile_denom <-EightMile_raster>=1
MurphyDome_denom<-MurphyDome_raster>=1

##Creates object with the total Pixels for each quadrat
Chatnika_Quad_totals  <-raster::extract(x=Chatnika_denom  ,y=Chatnika_quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")
EightMile_Quad_totals <-raster::extract(x=EightMile_denom ,y=EightMile_quadrats ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")
MurphyDome_Quad_totals<-raster::extract(x=MurphyDome_denom,y=MurphyDome_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")

##DF OF METEDATA
Chatnika_meta  <-Chatnika_quadrats@data%>%as.data.frame()
EightMile_meta <-EightMile_quadrats@data%>%as.data.frame()
MurphyDome_meta<-MurphyDome_quadrats@data%>%as.data.frame()

##Creates object with the total Pixels for each Functional group
Chatnika_Graminoid_sum <-raster::extract(x=Chatnika_Graminoid ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid" )
Chatnika_DwarfShrub_sum<-raster::extract(x=Chatnika_DwarfShrub,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub")
Chatnika_Moss_sum      <-raster::extract(x=Chatnika_Moss      ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss"      )
Chatnika_Forb_sum      <-raster::extract(x=Chatnika_Forb      ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb"      )
Chatnika_Lichen_sum    <-raster::extract(x=Chatnika_Lichen    ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen"    )
Chatnika_Shrub_sum     <-raster::extract(x=Chatnika_Shrub     ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub"     )
Chatnika_Tree_sum      <-raster::extract(x=Chatnika_Tree      ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree"      )

EightMile_Graminoid_sum <-raster::extract(x=EightMile_Graminoid ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid" )
EightMile_DwarfShrub_sum<-raster::extract(x=EightMile_DwarfShrub,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub")
EightMile_Moss_sum      <-raster::extract(x=EightMile_Moss      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss"      )
EightMile_Forb_sum      <-raster::extract(x=EightMile_Forb      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb"      )
EightMile_Lichen_sum    <-raster::extract(x=EightMile_Lichen    ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen"    )
EightMile_Shrub_sum     <-raster::extract(x=EightMile_Shrub     ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub"    )
EightMile_Tree_sum      <-raster::extract(x=EightMile_Tree      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree"      )

MurphyDome_Graminoid_sum <-raster::extract(x=MurphyDome_Graminoid ,y=MurphyDome_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid" )
MurphyDome_DwarfShrub_sum<-raster::extract(x=MurphyDome_DwarfShrub,y=MurphyDome_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub")
MurphyDome_Moss_sum      <-raster::extract(x=MurphyDome_Moss      ,y=MurphyDome_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss"      )
MurphyDome_Forb_sum      <-raster::extract(x=MurphyDome_Forb      ,y=MurphyDome_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb"      )
MurphyDome_Lichen_sum    <-raster::extract(x=MurphyDome_Lichen    ,y=MurphyDome_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen"    )
MurphyDome_Shrub_sum     <-raster::extract(x=MurphyDome_Shrub     ,y=MurphyDome_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub"    )
MurphyDome_Tree_sum      <-raster::extract(x=MurphyDome_Tree      ,y=MurphyDome_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree"      )

##Lets combine the datframes created above
Chatnika_HDW_pixeltotals<-Reduce(cbind,list(Chatnika_Quad_totals
                                           ,Chatnika_Graminoid_sum 
                                           ,Chatnika_DwarfShrub_sum
                                           ,Chatnika_Moss_sum      
                                           ,Chatnika_Forb_sum      
                                           ,Chatnika_Lichen_sum    
                                           ,Chatnika_Shrub_sum     
                                           ,Chatnika_Tree_sum      ))

EightMile_HDW_pixeltotals<-Reduce(cbind,list(EightMile_Quad_totals
                                            ,EightMile_Graminoid_sum 
                                            ,EightMile_DwarfShrub_sum
                                            ,EightMile_Moss_sum      
                                            ,EightMile_Forb_sum      
                                            ,EightMile_Lichen_sum    
                                            ,EightMile_Shrub_sum     
                                            ,EightMile_Tree_sum      ))

MurphyDome_HDW_pixeltotals<-Reduce(cbind,list(MurphyDome_Quad_totals
                                             ,MurphyDome_Graminoid_sum 
                                             ,MurphyDome_DwarfShrub_sum
                                             ,MurphyDome_Moss_sum      
                                             ,MurphyDome_Forb_sum      
                                             ,MurphyDome_Lichen_sum    
                                             ,MurphyDome_Shrub_sum     
                                             ,MurphyDome_Tree_sum      ))

##Now we want to calculate the % cover for each Functional group in each quadrat
Chatnika_HDW_PercentCover<-Chatnika_HDW_pixeltotals[,2:8]/(Chatnika_HDW_pixeltotals[,1])*100
Chatnika_HDW_PercentCover<-Chatnika_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(Chatnika_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())

EightMile_HDW_PercentCover<-EightMile_HDW_pixeltotals[,2:6]/(EightMile_HDW_pixeltotals[,1])*100
EightMile_HDW_PercentCover<-EightMile_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(EightMile_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())


MurphyDome_HDW_PercentCover<-MurphyDome_HDW_pixeltotals[,2:6]/(MurphyDome_HDW_pixeltotals[,1])*100
MurphyDome_HDW_PercentCover<-MurphyDome_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(MurphyDome_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())

##Lets merge the metadata with these new dataframes
Chatnika_HDW_PercentCover  <-merge(Chatnika_meta,  Chatnika_HDW_PercentCover  ,by="CLASS_ID")%>%dplyr::select(-CLASS_CLRS)
EightMile_HDW_PercentCover <-merge(EightMile_meta, EightMile_HDW_PercentCover ,by="CLASS_ID")%>%dplyr::select(-CLASS_CLRS)
MurphyDome_HDW_PercentCover<-merge(MurphyDome_meta,MurphyDome_HDW_PercentCover,by="CLASS_ID")%>%dplyr::select(-CLASS_CLRS)

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown","orange2","wheat1","red")

jpeg('Outputs/2_HDW_Imagery/2_Models/Chatnika Plot of PCA Model Prediction_HDW.jpg',width=1200, height=700)
plot(
  Chatnika_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  main = "Classified Functional Group Model using PCA 
  Location: Chatnika, AK",
  xlab="Longitude", 
  ylab="Latitude"
)
plot(Chatnika_quadrats,border="red",lwd=2,add=TRUE)
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

###########################################Plot 2############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown","orange2","wheat1","red")

jpeg('Outputs/2_HDW_Imagery/2_Models/EightMile Plot of PCA Model Prediction_HDW.jpg',width=1200, height=700)
plot(
  EightMile_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  main = "Classified Functional Group Model using PCA 
  Location: EightMile, AK",
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

###########################################Plot 3############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown","orange2","wheat1","red")

jpeg('Outputs/2_HDW_Imagery/2_Models/MurphyDome Plot of PCA Model Prediction_HDW.jpg',width=1200, height=700)
plot(
  MurphyDome_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  main = "Classified Functional Group Model using PCA 
  Location: MurphyDome, AK",
  xlab="Longitude", 
  ylab="Latitude"
)
plot(MurphyDome_quadrats,border="red",lwd=2,add=TRUE)
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

###writes out Rater layer created
writeRaster(Results_HDW_PCA_raster,
            filename ="Outputs/2_HDW_Imagery/2_Models/Clayton_test_HDW_PCA_raster", 
            format="GTiff", overwrite=TRUE)
