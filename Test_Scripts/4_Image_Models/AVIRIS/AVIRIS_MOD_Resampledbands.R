#################################Resampled band models for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_resampled/Headwall_05nm
library(randomForest)
library(raster)
library(tidyverse)

##Reads in test imagery
Clayton_test_HDW<-brick("Original_data/Test_imagery_HDW/Clayton_test_HDW")%>%rasterToPoints()%>%as.data.frame()

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Headwall_wv", numeric())

##Now lets change the column names of the df created from thge image, we want these column names to match the bandpasses
colnames(Clayton_test_HDW)[-1:-2]<-HDW_ng_wv

##Reads in resampled spectral library, 5nm, 10nm bands  
alaskaSpeclib_HDW_05nm_equal25<-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_05nm_equal25.csv")
alaskaSpeclib_HDW_10nm_equal25<-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_10nm_equal25.csv")

###reads in resampled imagery, 5nm, 10nm bands
Clayton_test_HDW_05nm<-read.csv("Outputs/2_HDW_Imagery/1_processing/Clayton_test_HDW_05nm.csv")
Clayton_test_HDW_10nm<-read.csv("Outputs/2_HDW_Imagery/1_processing/Clayton_test_HDW_10nm.csv")

##Remove unwanted metadata from spectral library
alaskaSpeclib_HDW_05nm_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL
alaskaSpeclib_HDW_10nm_equal25 [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_HDW_resampled_05nm <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_05nm_equal25  ,mtry=3,ntree=500,importance=TRUE)
rf_HDW_resampled_10nm <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_10nm_equal25  ,mtry=3,ntree=500,importance=TRUE)

##Lets create a dataframe that shows the importance of each variable
feat_imp_df <- importance(rf_HDW_resampled_05nm)[1:15,] %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

#now let's visualize that dataframe plot dataframe
jpeg('Outputs/2_HDW_Imagery/2_Models/05nm_bands Feature importance.jpg',width=800, height=600)
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: Randomforest_MOD using 5nm_bands as predictors"
  )
dev.off()

##Lets create a dataframe that shows the importance of each variable
feat_imp_df <- importance(rf_HDW_resampled_10nm)[1:15,] %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

#now let's visualize that dataframe plot dataframe
jpeg('Outputs/2_HDW_Imagery/2_Models/10nm_bands Feature importance.jpg',width=800, height=600)
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: Randomforest_MOD using 10nm_bands as predictors"
  )
dev.off()

###I should rebuild models with these importat variables
##but I will leave that for another day

##Uses model from spectral library to predict images
##converts prediction from rf model to dataframe and changes column name to predicted
Results_HDW_05nm    <-predict(rf_HDW_resampled_05nm        ,Clayton_test_HDW_05nm[-1:-2])%>%as.data.frame()%>%'names<-'("predicted")
Results_HDW_10nm    <-predict(rf_HDW_resampled_10nm        ,Clayton_test_HDW_10nm[-1:-2])%>%as.data.frame()%>%'names<-'("predicted")

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_05nm<-cbind(Results_HDW_05nm,Clayton_test_HDW[1:2]) %>% dplyr::select(predicted,x,y)
Results_HDW_10nm<-cbind(Results_HDW_10nm,Clayton_test_HDW[1:2]) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_HDW_05nm<-unique(as.data.frame(Results_HDW_05nm$predicted)) 
Unique_HDW_05nm$PFT_ID<-seq(1:nrow(Unique_HDW_05nm))
names(Unique_HDW_05nm)[1]<-"predicted"

Unique_HDW_10nm<-unique(as.data.frame(Results_HDW_10nm$predicted)) 
Unique_HDW_10nm$PFT_ID<-seq(1:nrow(Unique_HDW_10nm))
names(Unique_HDW_10nm)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_05nm<-merge(Results_HDW_05nm,Unique_HDW_05nm, by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_HDW_10nm<-merge(Results_HDW_10nm,Unique_HDW_10nm, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer....and use as.factor to arrange my original raster layer
Results_HDW_05nm_raster<-rasterFromXYZ(Results_HDW_05nm, crs = crs(Clayton_test_HDW))
Results_HDW_10nm_raster<-rasterFromXYZ(Results_HDW_10nm, crs = crs(Clayton_test_HDW))

##Creates a plot showing the output of the predicted ratser layer
jpeg('Outputs/2_HDW_Imagery/2_Models/Plot of 05nm Model.jpg',width=1000, height=700)
par(xpd = FALSE)
plot(
  Results_HDW_05nm_raster,
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
  main = "Plot of 05nm Model"
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
  cex=0.75,
  xjust =1,
  horiz = FALSE,
  inset = -0.1,
  par(cex=0.3)
  
)             
dev.off()

jpeg('Outputs/2_HDW_Imagery/2_Models/Plot of 10nm Model.jpg',width=1000, height=700)
par(xpd = FALSE)
plot(
  Results_HDW_10nm_raster,
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
  main = "Plot of 10nm Model"
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
  cex=0.75,
  xjust =1,
  horiz = FALSE,
  inset = -0.1,
  par(cex=0.3)
  
)             
dev.off()


###writes out Rater layer created
writeRaster(Results_HDW_05nm_raster,
            filename ="Outputs/2_HDW_Imagery/2_Models/Results_HDW_05nm_raster", 
            format="GTiff", overwrite=TRUE)

###writes out Rater layer created
writeRaster(Results_HDW_10nm_raster,
            filename ="Outputs/2_HDW_Imagery/2_Models/Results_HDW_10nm_raster", 
            format="GTiff", overwrite=TRUE)
