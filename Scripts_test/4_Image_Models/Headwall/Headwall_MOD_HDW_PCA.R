#################################PCA modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_pca/Headwall_pca
library(randomForest)
library(raster)
library(rgdal)
library(tidyverse)

##Reads in test imagery
Clayton_test_HDW<-brick("Original_data/Test_imagery_HDW/Clayton_test_HDW")

##Reads in Shapefile for quadrat locations
Clayton_Test_quads<- readOGR("Original_data/Test_imagery_HDW","Clayton_Test_quads")

##Reads in PCA for specctral library each functional group has a total of 25 scans and imagery 
HDW_PCAspeclib<-read.csv("Outputs/2_HDW_Imagery/1_Processing/HDW_PCAspeclib.csv")
HDW_PCAimage  <-read.csv("Outputs/2_HDW_Imagery/1_Processing/HDW_PCAimage.csv"  )

##Remove unwanted metadata from spectral library
HDW_PCAspeclib [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_HDW_PCA         <-randomForest(PFT_3~.,data=HDW_PCAspeclib  ,mtry=3,ntree=301,importance=TRUE)

###Lets create a dataframe that shows the importance of each variable
#feat_imp_df <- importance(rf_HDW_PCA) %>% 
#  data.frame() %>% 
#  mutate(feature = row.names(.)) 
#
##now let's visualize that dataframe plot dataframe
#jpeg('Outputs/2_HDW_Imagery/2_Models/PC Feature importance_HDW.jpg',width=1000, height=700)
#ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
#                        y = MeanDecreaseGini)) +
#  geom_bar(stat='identity') +
#  coord_flip() +
#  theme_classic() +
#  labs(
#    x     = "Feature",
#    y     = "Importance",
#    title = "Feature Importance: HDW Randomforest_MOD using PCs as predictors"
#  )
#dev.off()

##Uses model from spectral library to predict images
Results_HDW_PCA    <-predict(rf_HDW_PCA        ,HDW_PCAimage[-(1:2)])

##Converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW_PCA<-as.data.frame(Results_HDW_PCA)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_PCA<-cbind(Results_HDW_PCA,HDW_PCAimage) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW_PCA<-unique(as.data.frame(Results_HDW_PCA$predicted)) 
Unique_HDW_PCA$PFT_ID<-seq(1:nrow(Unique_HDW_PCA))
names(Unique_HDW_PCA)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_PCA<-merge(Results_HDW_PCA,Unique_HDW_PCA, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Results_HDW_PCA_raster<-rasterFromXYZ(Results_HDW_PCA, crs = crs(Clayton_test_HDW))

##Please remeber that the unique values represents each Functional group
##   predicted PFT_ID
##   Graminoid      1
## Dwarf Shrub      2
##        Moss      3
##        Forb      4
##      Lichen      5
##       Shrub      6
##        Tree      7

###Filters the image on each functional group
Graminoid <-Results_HDW_PCA_raster==1
DwarfShrub<-Results_HDW_PCA_raster==2
Moss      <-Results_HDW_PCA_raster==3
Forb      <-Results_HDW_PCA_raster==4
Lichen    <-Results_HDW_PCA_raster==5
Shrub     <-Results_HDW_PCA_raster==6
Tree      <-Results_HDW_PCA_raster==7

##We need to change all those values within the raster to 1, 
##so the sum of all the pixels in each quadrat can be calculated later
denom <-Results_HDW_PCA_raster>=1

##Creates object with the total Pixels for each quadrat
Quad_totals <-raster::extract(x=denom,y=Clayton_Test_quads,fun=sum)%>%as.data.frame()%>%'names<-'("Quad Sum")

##Creates object with the total Pixels for each Functional group
Graminoid_sum <-raster::extract(x=Graminoid ,y=Clayton_Test_quads,fun=sum)%>%as.data.frame()%>%'names<-'("Graminoid Sum" )
DwarfShrub_sum<-raster::extract(x=DwarfShrub,y=Clayton_Test_quads,fun=sum)%>%as.data.frame()%>%'names<-'("DwarfShrub Sum")
Moss_sum      <-raster::extract(x=Moss      ,y=Clayton_Test_quads,fun=sum)%>%as.data.frame()%>%'names<-'("Moss Sum"      )
Forb_sum      <-raster::extract(x=Forb      ,y=Clayton_Test_quads,fun=sum)%>%as.data.frame()%>%'names<-'("Forb Sum"      )
Lichen_sum    <-raster::extract(x=Lichen    ,y=Clayton_Test_quads,fun=sum)%>%as.data.frame()%>%'names<-'("Lichen Sum"    )
Shrub_sum     <-raster::extract(x=Shrub     ,y=Clayton_Test_quads,fun=sum)%>%as.data.frame()%>%'names<-'("Shrub  Sum"    )
Tree_sum      <-raster::extract(x=Tree      ,y=Clayton_Test_quads,fun=sum)%>%as.data.frame()%>%'names<-'("Tree Sum"      )

##Lets combine the datframes created above
Clayton_test_HDW_pixeltotals<-Reduce(cbind,list(Quad_totals
                                                ,Graminoid_sum 
                                                ,DwarfShrub_sum
                                                ,Moss_sum      
                                                ,Forb_sum      
                                                ,Lichen_sum    
                                                ,Shrub_sum     
                                                ,Tree_sum      ))

##Now we want to calculate the % cover for each Functional group in each quadrat
Clayton_test_HDW_PercentCover<-Clayton_test_HDW_pixeltotals[,2:6]/(Clayton_test_HDW_pixeltotals[,1])*100
Clayton_test_HDW_PercentCover<-Clayton_test_HDW_PercentCover%>%
  mutate(quads=rownames(Clayton_test_HDW_PercentCover))%>%
  dplyr::select(quads,everything())

###save plot as a jpeg
chm_colors <- c("darkgreen","chartreuse3","gold","gray69","saddlebrown","orange2","wheat1","black")

jpeg('Outputs/2_HDW_Imagery/2_Models/Plot of PCA Model Prediction_HDW.jpg',width=1200, height=700)
plot(
  Results_HDW_PCA_raster,
  legend = FALSE,
  axes=FALSE,
  col = chm_colors[-8],
  box= FALSE,
  main = "Classified Functional Group Model using PCA 
  Location: Clayton Lake, AK",
  xlab="Longitude", 
  ylab="Latitude"
)
plot(Clayton_Test_quads,border="black",lwd=4,add=TRUE)
legend(
  "right",
  legend = c("Dwarf Shrub","Shrub","moss","Lichen","Tree","Forb","Graminoid","Quadrats"),
  fill =chm_colors,
  border = FALSE,
  bty = "n",
  cex=1.5,
  xjust =1,
  horiz = FALSE,
  inset = -0.006,
  par(cex=0.4)
  
)             
dev.off()

###writes out Rater layer created
writeRaster(Results_HDW_PCA_raster,
            filename ="Outputs/2_HDW_Imagery/2_Models/Clayton_test_HDW_PCA_raster", 
            format="GTiff", overwrite=TRUE)

