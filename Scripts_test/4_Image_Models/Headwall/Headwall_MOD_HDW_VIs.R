#################################VIs modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_VIs/Headwall_VIs
library(randomForest)
library(raster)
library(tidyverse)

##Reads in test imagery
Clayton_test_HDW<-brick("Original_data/Test_imagery_HDW/Clayton_test_HDW")%>%rasterToPoints()%>%as.data.frame()

##Reads in VIs for specctral library each functional group has a total of 25 scans and imagery 
alaskaSpeclib_HDW_VIs_equal25<-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_VIs_equal25.csv")
Clayton_test_HDW_VIs <-read.csv("Outputs/2_HDW_Imagery/1_processing/Clayton_test_HDW_VIs.csv" )

##Remove unwanted metadata from spectral library
alaskaSpeclib_HDW_VIs_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_HDW_VIs         <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_VIs_equal25  ,mtry=3,ntree=500,importance=TRUE)

##Lets create a dataframe that shows the importance of each variable
feat_imp_df <- importance(rf_HDW_VIs)[1:15,] %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

#now let's visualize that dataframe plot dataframe
jpeg('Outputs/2_HDW_Imagery/2_Models/VIs Feature importance_HDW.jpg',width=800, height=600)
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: HDW Randomforest_MOD using Veg Index as predictors"
  )
dev.off()

##uses model from spectral library to predict images
Results_HDW_VIs    <-predict(rf_HDW_VIs        ,Clayton_test_HDW_VIs[-1:-2])


##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW_VIs<-as.data.frame(Results_HDW_VIs)%>%'names<-'("predicted")


## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_VIs<-cbind(Results_HDW_VIs,Clayton_test_HDW[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW_VIs<-unique(as.data.frame(Results_HDW_VIs$predicted)) 
Unique_HDW_VIs$PFT_ID<-seq(1:nrow(Unique_HDW_VIs))
names(Unique_HDW_VIs)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_VIs<-merge(Results_HDW_VIs,Unique_HDW_VIs, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Results_HDW_VIs_raster<-rasterFromXYZ(Results_HDW_VIs, crs = crs(Clayton_test_HDW))

##Creates a plot showing the output of the predicted ratser layer
jpeg('Outputs/2_HDW_Imagery/2_Models/Plot of Vegiation Index Model Prediction_HDW.jpg',width=1000, height=700)
par(xpd = FALSE)
plot(
  Results_HDW_VIs_raster,
  legend = FALSE,
  col = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow"
  ),
  xaxt = 'n',
  yaxt = 'n',
  main = "Plot of Vegiation Index Model Prediction - HDW"
)

par(xpd = TRUE)
legend(
  "right",
  legend = c("Dwarf Shrub","Shrub","moss","Tree","Forb","Graminoid"),
  fill = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow"
  ),
  cex=1,
  xjust =1,
  horiz = FALSE,
  inset = -0.07,
  par(cex=0.3)
  
)             
dev.off()

###writes out Rater layer created
writeRaster(Results_HDW_VIs_raster,
            filename ="Outputs/2_HDW_Imagery/2_Models/Results_HDW_VIs_raster", 
            format="GTiff", overwrite=TRUE)



