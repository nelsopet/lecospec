#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var/Headwall
library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)
library(hsdar)

##Reads in imagery so we can get the crs to create our raster later
Chatnika_HDW<-brick("Original_data/Test_imagery_HDW/Chatnika_clipped_HDW")

##This is a dataframe with all predictors to be used in model building
Chatnika_data_HDW<-read.csv("Outputs/2_HDW_Imagery/1_processing/Chatnika_data_HDW.csv")

##Reads in Shapefile for quadrat locations
Chatnika_quadrats  <- readOGR("Original_data/Test_imagery_HDW","Chatnika_quadrats"  )

##Reads in specctral library with all predictors
##Each functional group has a total of 25 scans and imagery 
alaskaSpecLib_data_HDW<-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpecLib_data_HDW.csv")

##Remove unwanted metadata from spectral library
alaskaSpecLib_data_HDW [c("ScanID","PFT","PFT_2","area","Freq1","Freq2","X")] = NULL

##We can build randomforest model
rf_HDW_Chatnika <-randomForest(PFT_3~.,data=alaskaSpecLib_data_HDW  ,mtry=20,ntree=2001,importance=TRUE)

##Lets save the confusion Matrix for these models
Chatnika_ConfusionMatrix<-rf_HDW_Chatnika$confusion%>%
  as.data.frame()

##Lets save this confusion Matrix
write.csv(Chatnika_ConfusionMatrix,"Outputs/2_HDW_imagery/2_Models/AK_imagery/Chatnika_ConfusionMatrix.csv")

##uses model from spectral library to predict images
Results_HDW    <-predict(rf_HDW_Chatnika,Chatnika_data_HDW[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW<-as.data.frame(Results_HDW)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW<-cbind(Results_HDW,Chatnika_data_HDW[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW<-unique(as.data.frame(Results_HDW$predicted)) 
Unique_HDW$PFT_ID<-seq(1:nrow(Unique_HDW))
names(Unique_HDW)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW<-merge(Results_HDW,Unique_HDW, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Chatnika_raster<-rasterFromXYZ(Results_HDW, crs = crs(Chatnika_HDW)) 

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
Chatnika_Shrub     <-Chatnika_raster==Shrub     [1,1]
Chatnika_Moss      <-Chatnika_raster==Moss      [1,1]
Chatnika_Tree      <-Chatnika_raster==Tree      [1,1]
Chatnika_Graminoid <-Chatnika_raster==Graminoid [1,1]
Chatnika_Lichen    <-Chatnika_raster==Lichen    [1,1]
Chatnika_Forb      <-Chatnika_raster==Forb      [1,1]
Chatnika_DwarfShrub<-Chatnika_raster==DwarfShrub[1,1]

##We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
Chatnika_denom  <-Chatnika_raster>=1

##DF OF METEDATA
Chatnika_meta  <-Chatnika_quadrats@data%>%as.data.frame()

#Creates object with the total Pixels for each quadrat
Chatnika_Quad_totals  <-raster::extract(x=Chatnika_denom  ,y=Chatnika_quadrats  ,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")

#Creates object with the total Pixels for each Functional group
Chatnika_Graminoid_sum <-raster::extract(x=Chatnika_Graminoid ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid_P" )
Chatnika_DwarfShrub_sum<-raster::extract(x=Chatnika_DwarfShrub,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub_P")
Chatnika_Moss_sum      <-raster::extract(x=Chatnika_Moss      ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Moss_P"      )
Chatnika_Forb_sum      <-raster::extract(x=Chatnika_Forb      ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Forb_P"      )
Chatnika_Lichen_sum    <-raster::extract(x=Chatnika_Lichen    ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen_P"    )
Chatnika_Shrub_sum     <-raster::extract(x=Chatnika_Shrub     ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub_P"     )
Chatnika_Tree_sum      <-raster::extract(x=Chatnika_Tree      ,y=Chatnika_quadrats,fun=sum)%>%as.data.frame()%>%'names<-'("Tree_P"      )

##Lets combine the datframes created above
Chatnika_HDW_pixeltotals<-Reduce(cbind,list(Chatnika_Quad_totals
                                            ,Chatnika_Graminoid_sum
                                            ,Chatnika_DwarfShrub_sum
                                            ,Chatnika_Moss_sum      
                                            ,Chatnika_Forb_sum      
                                            ,Chatnika_Lichen_sum    
                                            ,Chatnika_Shrub_sum     
                                            ,Chatnika_Tree_sum      ))

##Now we want to calculate the % cover for each Functional group in each quadrat
Chatnika_HDW_PercentCover<-Chatnika_HDW_pixeltotals[,2:8]/(Chatnika_HDW_pixeltotals[,1])*100
Chatnika_HDW_PercentCover<-Chatnika_HDW_PercentCover%>%
  mutate(CLASS_ID=rownames(Chatnika_HDW_PercentCover))%>%
  dplyr::select(CLASS_ID,everything())

##Lets merge the metadata with these new dataframes
Chatnika_HDW_PercentCover <-merge(Chatnika_meta,  Chatnika_HDW_PercentCover  ,by="CLASS_ID")
Chatnika_HDW_PercentCover<-Chatnika_HDW_PercentCover%>%
  arrange(CLASS_NAME)%>%
  dplyr::select(-CLASS_CLRS,-CLASS_ID)%>%
  mutate(CLASS_ID=rownames(Chatnika_HDW_PercentCover))%>%dplyr::select(CLASS_ID,everything())

##Import quadrat estimates made by jane and merge to estimates generated by models
ChatnikaEstimates<-read.csv("Original_data/Test_imagery_HDW/ChatnikaEstimates_Jane.csv")
ChatnikaEstimate_Tab<-cbind(Chatnika_HDW_PercentCover,ChatnikaEstimates)
ChatnikaEstimate_Tab<-ChatnikaEstimate_Tab%>%dplyr::select(CLASS_ID,CLASS_NAME,Graminoid_P,
                                                           Graminoid_A,DwarfShrub_P,DwarfShrub_A,Moss_P,Moss_A,Forb_P,
                                                           Forb_A,Lichen_P,Lichen_A,Shrub_P,Shrub_A,Tree_P,Tree_A,Litter_A)

write.csv(ChatnikaEstimate_Tab ,"Outputs/2_HDW_imagery/2_Models/AK_imagery/Chatnika_model_PercentCover.csv")

##Import quadrat estimates made by jane and merge to estimates generated by models
#ChatnikaEstimates<-read.csv("Original_data/Test_imagery_HDW/ChatnikaEstimates_Jane.csv")
#ChatnikaEstimate_Tab<-merge(Chatnika_HDW_PercentCover,ChatnikaEstimates,by="CLASS_NAME")

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","mediumvioletred","gold","deepskyblue","saddlebrown","orange2","ivory3")

jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/Chatnika Plot of VIs Model Prediction.jpg',width=1200, height=700)
plot(
  Chatnika_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  xlab="Longitude", 
  ylab="Latitude"
)
plot(Chatnika_quadrats,border="white",lwd=2,add=TRUE)
legend(
  "top",
  legend = c("Graminoid","Tree", "Dwarf Shrub","Shrub","Forb","Moss","Lichen"),
  fill =chm_colors,
  border = FALSE,
  bty = "n",
  cex=1.3,
  xjust =1,
  horiz = TRUE,
  inset = -0.01,
  par(cex=0.4)
  
)             
dev.off()

#####writes out Rater layer created
##writeRaster(Results_HDW_raster,
            ##filename ="Outputs/2_HDW_Imagery/2_Models/Results_HDW_raster_Chatnika", 
            ##format="GTiff", overwrite=TRUE)
