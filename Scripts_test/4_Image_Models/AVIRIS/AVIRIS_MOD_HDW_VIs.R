#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_VIs/Headwall_VIs
library(randomForest)
library(raster)
library(tidyverse)

#Reads in Imagery
Clayton_test_AV<-brick("Original_data/Test_imagery_AVIRIS/Clayton_test_AVIRIS")

##Marks raster as unrotated
Clayton_test_AV@rotated<-FALSE

##Converts to a dataframe
Clayton_test_AV<-rasterToPoints(Clayton_test_AV)%>% as.data.frame()

##Reads in VIs for specctral library each functional group has a total of 25 scans and imagery 
alaskaSpeclib_AV_VIs_equal25<-read.csv("Outputs/3_AV_Imagery/1_processing/alaskaSpeclib_AV_VIs_equal25.csv")
Clayton_test_AV_VIs <-read.csv("Outputs/3_AV_Imagery/1_processing/Clayton_test_AV_VIs.csv" )

##Remove unwanted metadata from spectral library
alaskaSpeclib_AV_VIs_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_AV_VIs         <-randomForest(PFT_3~.,data=alaskaSpeclib_AV_VIs_equal25  ,mtry=3,ntree=500,importance=TRUE)

##Lets create a dataframe that shows the importance of each variable
feat_imp_df <- importance(rf_AV_VIs)[1:15,] %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

#now let's visualize that dataframe plot dataframe
jpeg('Outputs/3_AV_Imagery/2_Models/VIs Feature importance_AVIRIS.jpg',width=800, height=600)
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: Randomforest_MOD using Veg Index as predictors"
  )
dev.off()

##uses model from spectral library to predict images
Results_AV_VIs    <-predict(rf_AV_VIs        ,Clayton_test_AV_VIs[-1:-2])


##converts prediction from rf model to dataframe and changes column name to predicted
Results_AV_VIs<-as.data.frame(Results_AV_VIs)%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_AV_VIs<-cbind(Results_AV_VIs,Clayton_test_AV[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_AV_VIs<-unique(as.data.frame(Results_AV_VIs$predicted)) 
Unique_AV_VIs$PFT_ID<-seq(1:nrow(Unique_AV_VIs))
names(Unique_AV_VIs)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_AV_VIs<-merge(Results_AV_VIs,Unique_AV_VIs, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Results_AV_VIs_raster<-rasterFromXYZ(Results_AV_VIs, crs = crs(Clayton_test_AV))

##Creates a plot showing the output of the predicted ratser layer
jpeg('Outputs/3_AV_Imagery/2_Models/Plot of Vegiation Index Model Prediction_AVIRIS.jpg',width=1000, height=700)
par(xpd = FALSE)
plot(
  Results_AV_VIs_raster,
  legend = FALSE,
  col = c(
    "lightcyan3",
    "royalblue"
  ),
  xaxt = 'n',
  yaxt = 'n',
  main = "Plot of Vegiation Index Model Prediction - AVIRIS"
)

par(xpd = TRUE)
legend(
  "right",
  legend = c("Shrub","Tree"),
  fill = c(
    "lightcyan3",
    "royalblue"
  ),
  cex=1.5,
  xjust =1,
  horiz = FALSE,
  inset = -0.09,
  par(cex=0.3)
  
)             
dev.off()

###writes out Rater layer created
writeRaster(Results_AV_VIs_raster,
            filename ="Outputs/3_AV_Imagery/2_Models/Results_AV_VIs_raster", 
            format="GTiff", overwrite=TRUE)



