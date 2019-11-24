#################################PCA modesl for headwall imagery######################################################
###Inputs from this model were made in Scripts/2_Image_processing/2_var_pca/Headwall_pca
library(randomForest)
library(raster)
library(tidyverse)
library(rasterVis)

##Reads in test imagery
Clayton_test_HDW<-brick("Original_data/Test_imagery/Clayton_test_HDW")

##Reads in PCA for specctral library each functional group has a total of 25 scans and imagery 
HDW_PCAspeclib<-read.csv("Outputs/2_HDW_Imagery/1_Processing/HDW_PCAspeclib.csv")
HDW_PCAimage  <-read.csv("Outputs/2_HDW_Imagery/1_Processing/HDW_PCAimage.csv"  )

##Remove unwanted metadata from spectral library
HDW_PCAspeclib [c("ScanID","PFT","PFT_2","area","Freq1","Freq2")] = NULL

##We can build randomforest model
rf_HDW_PCA         <-randomForest(PFT_3~.,data=HDW_PCAspeclib  ,mtry=3,ntree=301,importance=TRUE)

##Lets create a dataframe that shows the importance of each variable
feat_imp_df <- importance(rf_HDW_PCA) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

#now let's visualize that dataframe plot dataframe
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: Randomforest_MOD using PCs as predictors"
  )

##uses model from spectral library to predict images
Results_HDW_PCA    <-predict(rf_HDW_PCA        ,HDW_PCAimage[-(1:2)])


##converts prediction from rf model to dataframe and changes column name to predicted
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
Results_HDW_PCA_raster<-rasterFromXYZ(Results_HDW_PCA, crs = crs(Clayton_test_HDW))%>%as.factor()

##Then I add an extra attribute that will be used for the legend
tar<-levels(Results_HDW_PCA_raster)[[1]]
tar[["Functional groups"]]<-Unique_HDW_PCA$predicted
levels(Results_HDW_PCA_raster)<-tar


##Then I plot using levelplot from the rasterVis package
levelplot(Results_HDW_PCA_raster,main = "Plot of PCA Model")

###writes out Rater layer created
writeRaster(Results_HDW_PCA_raster,
            filename ="Outputs/2_HDW_Imagery/2_Models/Results_HDW_PCA_raster", 
            format="GTiff", overwrite=TRUE)
###save plot as a jpeg
jpeg('Outputs/2_HDW_Imagery/2_Models/Plot of PCA Model.jpg',width=1000, height=700)
levelplot(Results_HDW_PCA_raster,main = "Plot of PCA Model")
dev.off()
