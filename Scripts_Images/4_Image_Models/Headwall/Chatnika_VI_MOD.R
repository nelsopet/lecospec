#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_VIs/Headwall_VIs
library(randomForest)
library(raster)
library(tidyverse)

##Reads in test imagery
Chatnika_HDW<-brick("Original_data/Test_imagery_HDW/Chatnika_clipped_HDW")

##Reads in Shapefile for quadrat locations
Chatnika_quadrats  <- readOGR("Original_data/Test_imagery_HDW","Chatnika_quadrats"  )

##Reads in VIs for specctral library each functional group has a total of 25 scans and imagery 
alaskaSpeclib_HDW_VIs_equal25<-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_VIs_equal25.csv")
Chatnika_HDW_VIs <-read.csv("Outputs/2_HDW_Imagery/1_processing/Chatnika_HDW_VIs.csv" )

##Remove unwanted metadata from spectral library
alaskaSpeclib_HDW_VIs_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_HDW_VIs_Chatnika         <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_VIs_equal25  ,mtry=3,ntree=500,importance=TRUE)

##uses model from spectral library to predict images
Results_HDW_VIs    <-predict(rf_HDW_VIs_Chatnika,Chatnika_HDW_VIs[-1:-2])

##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW_VIs<-as.data.frame(Results_HDW_VIs)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_VIs<-cbind(Results_HDW_VIs,Chatnika_HDW_VIs[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW_VIs<-unique(as.data.frame(Results_HDW_VIs$predicted)) 
Unique_HDW_VIs$PFT_ID<-seq(1:nrow(Unique_HDW_VIs))
names(Unique_HDW_VIs)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_VIs<-merge(Results_HDW_VIs,Unique_HDW_VIs, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Chatnika_raster<-rasterFromXYZ(Results_HDW_VIs, crs = crs(Chatnika_HDW))

##################################################Raster #1#####################################################
###Filters the image on each functional group
Chatnika_Graminoid <-Chatnika_raster==1
Chatnika_DwarfShrub<-Chatnika_raster==2
Chatnika_Moss      <-Chatnika_raster==3
Chatnika_Forb      <-Chatnika_raster==4
Chatnika_Lichen    <-Chatnika_raster==5
Chatnika_Shrub     <-Chatnika_raster==6
Chatnika_Tree      <-Chatnika_raster==7

##We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
Chatnika_denom  <-Chatnika_raster>=1

##DF OF METEDATA
Chatnika_meta<-Chatnika_quadrats@data%>%as.data.frame()

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
                                                             Graminoid_A,DwarfShrub_P,Dwarf_Shrub_A,Moss_P,Moss_A,Forb_P,
                                                             Forb_A,Lichen_P,Lichen_A,Shrub_P,Shrub_A,Tree_P,Tree_A )

write.csv(ChatnikaEstimate_Tab,"Outputs/2_HDW_imagery/2_Models/AK_imagery/ChatnikaEstimate_Tab.csv")

###########################################Plot 1############################################################
###save plot as a jpeg
chm_colors <- c("darkgreen","chartreuse3","gold","deepskyblue","saddlebrown","orange2","wheat1","red")

jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/Chatnika Plot of VIs Model Prediction.jpg',width=1200, height=700)
plot(
  Chatnika_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE
)
plot(Chatnika_quadrats,border="red",lwd=2,add=TRUE)
legend(
  "topright",
  legend = c("Dwarf Shrub","Shrub","moss","Lichen","Tree","Forb","Graminoid","Quadrats"),
  ncol=2,
  fill =chm_colors,
  border = FALSE,
  bty = "n",
  cex=1.5,
  xjust =1,
  horiz = FALSE,
  inset = -0.01,
  par(cex=0.4)
  
)             
dev.off()


