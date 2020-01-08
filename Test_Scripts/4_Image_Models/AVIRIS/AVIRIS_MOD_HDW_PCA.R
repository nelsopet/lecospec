#################################PCA modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_pca/Headwall_pca
library(randomForest)
library(raster)
library(tidyverse)

##Reads in Imagery
Clayton_test_AV<-brick("Original_data/Test_imagery_AVIRIS/Clayton_test_AVIRIS")

##Marks raster as unrotated
Clayton_test_AV@rotated<-FALSE

##Converts to a dataframe
Clayton_test_AV<-rasterToPoints(Clayton_test_AV)%>% as.data.frame()

##Reads in PCA for specctral library each functional group has a total of 25 scans and imagery 
AV_PCAspeclib<-read.csv("Outputs/3_AV_Imagery/1_Processing/AV_PCAspeclib.csv")
AV_PCAimage  <-read.csv("Outputs/3_AV_Imagery/1_Processing/AV_PCAimage.csv"  )

##Remove unwanted metadata from spectral library
AV_PCAspeclib [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_AV_PCA         <-randomForest(PFT_3~.,data=AV_PCAspeclib  ,mtry=3,ntree=301,importance=TRUE)

##Lets create a dataframe that shows the importance of each variable
feat_imp_df <- importance(rf_AV_PCA) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

#now let's visualize that dataframe plot dataframe
jpeg('Outputs/3_AV_Imagery/2_Models/PC Feature importance_AVIRIS.jpg',width=1000, height=700)
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: AVIRIS Randomforest_MOD using PCs as predictors"
  )
dev.off()

##Uses model from spectral library to predict images
Results_AV_PCA    <-predict(rf_AV_PCA        ,AV_PCAimage[-(1:2)])

##Converts prediction from rf model to dataframe and changes column name to predicted
Results_AV_PCA<-as.data.frame(Results_AV_PCA)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_AV_PCA<-cbind(Results_AV_PCA,AV_PCAimage) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_AV_PCA<-unique(as.data.frame(Results_AV_PCA$predicted)) 
Unique_AV_PCA$PFT_ID<-seq(1:nrow(Unique_AV_PCA))
names(Unique_AV_PCA)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_AV_PCA<-merge(Results_AV_PCA,Unique_AV_PCA, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Results_AV_PCA_raster<-rasterFromXYZ(Results_AV_PCA, crs = crs(Clayton_test_AV))

###save plot as a jpeg
jpeg('Outputs/3_AV_Imagery/2_Models/Plot of PCA Model Prediction_AVIRIS.jpg',width=1000, height=700)
par(xpd = FALSE)
plot(
  Results_AV_PCA_raster,
  legend = FALSE,
  col = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow",
    "orange"
  ),
  xaxt = 'n',
  yaxt = 'n',
  main = "Plot of PCA Model Prediction - AVIRIS"
)

par(xpd = TRUE)
legend(
  "right",
  legend = c("Dwarf Shrub","Shrub","moss","Lichen","Tree","Forb","Graminoid"),
  fill = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow",
    "orange"
  ),
  cex=1.5,
  xjust =1,
  horiz = FALSE,
  inset = -0.09,
  par(cex=0.4)
  
)             
dev.off()

###writes out Rater layer created
writeRaster(Results_AV_PCA_raster,
            filename ="Outputs/3_AV_Imagery/2_Models/Results_AV_PCA_raster", 
            format="GTiff", overwrite=TRUE)

