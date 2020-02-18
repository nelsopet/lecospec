#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var/Headwall
library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)
library(hsdar)
library(randomcoloR)

##Reads in imagery so we can get the crs to create our raster later
EightMile_HDW<-brick("Original_data/EightMile/EightMile_clippedHalf")
EightMile_HDW_latlong<-EightMile_HDW%>%rasterToPoints()%>%as.data.frame()

##lets remove all those bads that had noise
EightMile_HDW_latlong[275:328]<-NULL

##Remove all pixels with NA values
EightMile_HDW_latlong<-na.omit(EightMile_HDW_latlong)

##Lets remove these two rows because they have negative values
EightMile_HDW_latlong<-EightMile_HDW_latlong[-c(449905, 521215), ]

##This is a dataframe with all predictors to be used in model building
EightMile_data_HDW<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_data_HDW.csv")

##Reads in Shapefile for quadrat locations
EightMile_quadrats  <- readOGR("Original_data/EightMile/EightMileHalf_Quads.shp")

##Reads in spectral library with all predictors
##Each functional group has a total of 25 scans
alaskaSpecLib_data_HDW<-read.csv("Outputs/2_HDW_Imagery/1_Processing/Field_Spec/alaskaSpecLib_data_HDW.csv")

##Remove unwanted metadata from spectral library
alaskaSpecLib_data_HDW [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model from our spectral library
rf_HDW_EightMile <-randomForest(PFT_3~.,data=alaskaSpecLib_data_HDW  ,mtry=20,ntree=2001,importance=TRUE)

##Lets save the confusion Matrix for these models
EightMile_ConfusionMatrix<-rf_HDW_EightMile$confusion%>%
  as.data.frame()

##Lets save this confusion Matrix
write.csv(EightMile_ConfusionMatrix,"Outputs/2_HDW_imagery/2_Models/EightMile_IMG/EightMile_ConfusionMatrix.csv",row.names = F)

##This function uses model from spectral library to predict the observations of each pixel in the imagery
Results_HDW    <-predict(rf_HDW_EightMile,EightMile_data_HDW[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW_1<-as.data.frame(Results_HDW)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_2<-cbind(Results_HDW_1,EightMile_HDW_latlong[1:2])%>%dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW<-unique(as.data.frame(Results_HDW_2$predicted)) 
Unique_HDW$PFT_ID<-seq(1:nrow(Unique_HDW))
names(Unique_HDW)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_3<-merge(Results_HDW_2,Unique_HDW, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Lets get those regular x and y values from the image
Results_HDW_4<-inner_join(Results_HDW_3,EightMile_HDW_latlong)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
EightMile_raster<-rasterize(Results_HDW_3, crs = crs(EightMile_HDW)) 
plot(EightMile_raster)

##################################################Raster #1#####################################################
Abiotic_Litter    <-subset(Unique_HDW,Unique_HDW$predicted=="Abiotic_Litter"    ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Abiotic_Rock      <-subset(Unique_HDW,Unique_HDW$predicted=="Abiotic_Rock"      ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Abiotic_Soil      <-subset(Unique_HDW,Unique_HDW$predicted=="Abiotic_Soil"      ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Dwarf_Shrub_Broad <-subset(Unique_HDW,Unique_HDW$predicted=="Dwarf_Shrub_Broad" ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Dwarf_Shrub_Needle<-subset(Unique_HDW,Unique_HDW$predicted=="Dwarf_Shrub_Needle") %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Forb              <-subset(Unique_HDW,Unique_HDW$predicted=="Forb"              ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Graminoid_Grass   <-subset(Unique_HDW,Unique_HDW$predicted=="Graminoid_Grass"   ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Graminoid_Sedge   <-subset(Unique_HDW,Unique_HDW$predicted=="Graminoid_Sedge"   ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Lichen_Dark       <-subset(Unique_HDW,Unique_HDW$predicted=="Lichen_Dark"       ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Lichen_Light      <-subset(Unique_HDW,Unique_HDW$predicted=="Lichen_Light"      ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Lichen_Yellow     <-subset(Unique_HDW,Unique_HDW$predicted=="Lichen_Yellow"     ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Moss_Acrocarp     <-subset(Unique_HDW,Unique_HDW$predicted=="Moss_Acrocarp"     ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Moss_Pleurocarp   <-subset(Unique_HDW,Unique_HDW$predicted=="Moss_Pleurocarp"   ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Moss_Sphagnum     <-subset(Unique_HDW,Unique_HDW$predicted=="Moss_Sphagnum  "   ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Shrub_Alder       <-subset(Unique_HDW,Unique_HDW$predicted=="Shrub_Alder"       ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Shrub_Other       <-subset(Unique_HDW,Unique_HDW$predicted=="Shrub_Other"       ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Shrub_Salix       <-subset(Unique_HDW,Unique_HDW$predicted=="Shrub_Salix"       ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Tree_Broad        <-subset(Unique_HDW,Unique_HDW$predicted=="Tree_Broad"        ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 
Tree_Needle       <-subset(Unique_HDW,Unique_HDW$predicted=="Tree_Needle"       ) %>%as.data.frame()%>%dplyr::select("PFT_ID") 

###Filters the image on each functional group
EightMile_Abiotic_Litter    <-EightMile_raster==Abiotic_Litter    [1,1]
EightMile_Abiotic_Rock      <-EightMile_raster==Abiotic_Rock      [1,1]
EightMile_Abiotic_Soil      <-EightMile_raster==Abiotic_Soil      [1,1]
EightMile_Dwarf_Shrub_Broad <-EightMile_raster==Dwarf_Shrub_Broad [1,1]
EightMile_Dwarf_Shrub_Needle<-EightMile_raster==Dwarf_Shrub_Needle[1,1]
EightMile_Forb              <-EightMile_raster==Forb              [1,1]
EightMile_Graminoid_Grass   <-EightMile_raster==Graminoid_Grass   [1,1]
EightMile_Graminoid_Sedge   <-EightMile_raster==Graminoid_Sedge   [1,1]
EightMile_Lichen_Dark       <-EightMile_raster==Lichen_Dark       [1,1]
EightMile_Lichen_Light      <-EightMile_raster==Lichen_Light      [1,1]
EightMile_Lichen_Yellow     <-EightMile_raster==Lichen_Yellow     [1,1]
EightMile_Moss_Acrocarp     <-EightMile_raster==Moss_Acrocarp     [1,1]
EightMile_Moss_Pleurocarp   <-EightMile_raster==Moss_Pleurocarp   [1,1]
EightMile_Moss_Sphagnum     <-EightMile_raster==Moss_Sphagnum     [1,1]
EightMile_Shrub_Alder       <-EightMile_raster==Shrub_Alder       [1,1]
EightMile_Shrub_Other       <-EightMile_raster==Shrub_Other       [1,1]
EightMile_Shrub_Salix       <-EightMile_raster==Shrub_Salix       [1,1]
EightMile_Tree_Broad        <-EightMile_raster==Tree_Broad        [1,1]
EightMile_Tree_Needle       <-EightMile_raster==Tree_Needle       [1,1]

#We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
EightMile_denom  <-EightMile_raster>=1

##DF OF METEDATA
EightMile_meta  <-EightMile_quadrats@data%>%as.data.frame()

#Creates object with the total Pixels for each quadrat
EightMile_Quad_totals  <-raster::extract(x=EightMile_denom  ,y=EightMile_quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")

#Creates object with the total Pixels for each Functional group
EightMile_Abiotic_Litter_sum    <-raster::extract(x=EightMile_Abiotic_Litter    ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Abiotic_Litter_p"    )
EightMile_Abiotic_Rock_sum      <-raster::extract(x=EightMile_Abiotic_Rock      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Abiotic_Rock_p"      )
EightMile_Abiotic_Soil_sum      <-raster::extract(x=EightMile_Abiotic_Soil      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Abiotic_Soil_p"      )
EightMile_Dwarf_Shrub_Broad_sum <-raster::extract(x=EightMile_Dwarf_Shrub_Broad ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Dwarf_Shrub_Broad_p ")
EightMile_Dwarf_Shrub_Needle_sum<-raster::extract(x=EightMile_Dwarf_Shrub_Needle,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Dwarf_Shrub_Needle_p")
EightMile_Forb_sum              <-raster::extract(x=EightMile_Forb              ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb_p"              )
EightMile_Graminoid_Grass_sum   <-raster::extract(x=EightMile_Graminoid_Grass   ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_Grass_p"   )
EightMile_Graminoid_Sedge_sum   <-raster::extract(x=EightMile_Graminoid_Sedge   ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_Sedge_p"   )
EightMile_Lichen_Dark_sum       <-raster::extract(x=EightMile_Lichen_Dark       ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_Dark_p"       )
EightMile_Lichen_Light_sum      <-raster::extract(x=EightMile_Lichen_Light      ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_Light_p"      )
EightMile_Lichen_Yellow_sum     <-raster::extract(x=EightMile_Lichen_Yellow     ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_Yellow_p"     )
EightMile_Moss_Acrocarp_sum     <-raster::extract(x=EightMile_Moss_Acrocarp     ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_Acrocarp_p"     )
EightMile_Moss_Pleurocarp_sum   <-raster::extract(x=EightMile_Moss_Pleurocarp   ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_Pleurocarp_p"   )
EightMile_Moss_Sphagnum_sum     <-raster::extract(x=EightMile_Moss_Sphagnum     ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_Sphagnum_p"     )
EightMile_Shrub_Alder_sum       <-raster::extract(x=EightMile_Shrub_Alder       ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_Alder_p"       )
EightMile_Shrub_Other_sum       <-raster::extract(x=EightMile_Shrub_Other       ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_Other_p"       )
EightMile_Shrub_Salix_sum       <-raster::extract(x=EightMile_Shrub_Salix       ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_Salix_p"       )
EightMile_Tree_Broad_sum        <-raster::extract(x=EightMile_Tree_Broad        ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_Broad_p"        )
EightMile_Tree_Needle_sum       <-raster::extract(x=EightMile_Tree_Needle       ,y=EightMile_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_Needle_p"       )



##Lets combine the datframes created above
EightMile_HDW_pixeltotals<-Reduce(cbind,list(EightMile_Quad_totals
                                            ,EightMile_Abiotic_Litter_sum    
                                            ,EightMile_Abiotic_Rock_sum      
                                            ,EightMile_Abiotic_Soil_sum      
                                            ,EightMile_Dwarf_Shrub_Broad_sum 
                                            ,EightMile_Dwarf_Shrub_Needle_sum
                                            ,EightMile_Forb_sum              
                                            ,EightMile_Graminoid_Grass_sum   
                                            ,EightMile_Graminoid_Sedge_sum   
                                            ,EightMile_Lichen_Dark_sum       
                                            ,EightMile_Lichen_Light_sum      
                                            ,EightMile_Lichen_Yellow_sum     
                                            ,EightMile_Moss_Acrocarp_sum     
                                            ,EightMile_Moss_Pleurocarp_sum   
                                            ,EightMile_Moss_Sphagnum_sum     
                                            ,EightMile_Shrub_Alder_sum       
                                            ,EightMile_Shrub_Other_sum       
                                            ,EightMile_Shrub_Salix_sum       
                                            ,EightMile_Tree_Broad_sum        
                                            ,EightMile_Tree_Needle_sum       ))

##Now we want to calculate the % cover for each Functional group in each quadrat
EightMile_HDW_PercentCover<-EightMile_HDW_pixeltotals[,2:14]/(EightMile_HDW_pixeltotals[,1])*100
EightMile_HDW_PercentCover<-EightMile_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(EightMile_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())

##Lets merge the metadata with these new dataframes
EightMile_HDW_PercentCover <-merge(EightMile_meta,  EightMile_HDW_PercentCover  ,by="CLASS_ID")
EightMile_HDW_PercentCover<-EightMile_HDW_PercentCover%>%
  arrange(CLASS_NAME)%>%
  dplyr::select(-CLASS_CLRS,-CLASS_ID)%>%
  mutate(CLASS_ID=rownames(EightMile_HDW_PercentCover))%>%dplyr::select(CLASS_ID,everything())

write.csv(EightMile_HDW_PercentCover ,"Outputs/2_HDW_imagery/2_Models/EightMile_IMG/EightMile_HDW_PercentCover.csv")

##Import quadrat estimates made by jane and merge to estimates generated by models
##This was not doen for the test image
#EightMileEstimates<-read.csv("Original_data/Test_imagery_HDW/EightMileEstimates_Jane.csv")
#EightMileEstimate_Tab<-cbind(EightMile_HDW_PercentCover,EightMileEstimates)
#EightMileEstimate_Tab<-EightMileEstimate_Tab%>%dplyr::select(CLASS_ID,CLASS_NAME,Graminoid_P,
#                                                           Graminoid_A,DwarfShrub_P,DwarfShrub_A,Moss_P,Moss_A,Forb_P,
#                                                           Forb_A,Lichen_P,Lichen_A,Shrub_P,Shrub_A,Tree_P,Tree_A,Litter_A)
#
#write.csv(EightMileEstimate_Tab ,"Test_Outputs/2_HDW_imagery/2_Models/AK_imagery/EightMile_model_PercentCover.csv")

##Import quadrat estimates made by jane and merge to estimates generated by models
#EightMileEstimates<-read.csv("Original_data/Test_imagery_HDW/EightMileEstimates_Jane.csv")
#EightMileEstimate_Tab<-merge(EightMile_HDW_PercentCover,EightMileEstimates,by="CLASS_NAME")

###########################################Plot 1############################################################
###save plot as a jpeg
##chm_colors <- c("darkgreen","mediumvioletred","gold","deepskyblue","saddlebrown","orange2","ivory3","darkorange4","khaki1","lightcyan1","mediumorchid3","yellow1","slateblue2")
chm_colors <-distinctColorPalette(nrow(Unique_HDW))
jpeg('Outputs/2_HDW_imagery/2_Models/EightMile_IMG/EightMile Plot Prediction.jpg',width=1200, height=700,res=200)
plot(
  EightMile_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  xlab="Longitude", 
  ylab="Latitude"
)
plot(EightMile_quadrats,border="white",lwd=2,add=TRUE)
legend(
  "topright",
  legend = c(paste(Unique_HDW$predicted)),
  fill =chm_colors,
  border = FALSE,
  bty = "n",
  cex=0.4,
  xjust =1,
  horiz = FALSE,
  inset = c(-0.04,0),
  par(cex=0.4)
  
)             
dev.off()
