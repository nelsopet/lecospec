# Creates a Predicted outpput for Imagery

library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)
library(hsdar)
library(randomcoloR)
library(randomForestExplainer)

# Lets create names for output and input folders
# Input folder is the dir path to where yor data is stored
# output folder is the dir path to where you want your processed data to be stored
# Replace these before running 
outputs_folder<-"OutputsIMG/Prediction/"
input_folder  <-"OutputsPSR/Processing/Sensors/"
input_folder2 <-"OutputsIMG/Processing/"

# Import names of Spectral libraries and thier predictors
# For now we'll work with headwall and AVIRIS
names_SpecLibPreds = list.files(input_folder, pattern="PredsDF",full.names = T)

# Reads in spectral library and their predictors for each sensor
SpecLibs_Preds<-lapply(names_SpecLibPreds,read.csv)%>% 
  
  # Removes dir path from the name
  setNames(gsub(input_folder,"",names_SpecLibPreds)) 

# Import names of Hyperspectral images and thier predictors
# For now we'll work with headwall and AVIRIS
names_HyperspecPreds = list.files(input_folder2, pattern="PredsDF",full.names = T)

# Reads in spectral library and their predictors for each sensor
HyperspecPreds<-lapply(names_HyperspecPreds,read.csv)%>% 
  
  # Removes dir path from the name
  setNames(gsub(input_folder,"",names_HyperspecPreds)) 


# Loads the function that builds a model on the 50 most importnat variables
source("Spectral_classifier.R")

# Creates a randomForest model on the 50 most importnat varible
# Need to optimize
RF_mod<-Spectral_classifier(SpecLibs_Preds[[2]])

SpecLibs_Preds[[2]][c("ScanID","PFT","PFT_2","PFT_4","Area","PFT2_Freq"
                      ,"PFT3_Freq","PFT4_Freq","Tree_numbe","x","y")] = NULL

rfmod<-randomForest(PFT_3~.,data=SpecLibs_Preds[[2]],      
                                  mtry=sqrt(ncol(SpecLibs_Preds[[2]])),
                                  ntree=1001,localImp = TRUE)

a<-raster::predict(RF_mod,HyperspecPreds[[2]][-1:-2])

# Make prediction of the image and convert it to a raster
Hyperspec_results<-function(x){
  
  # Predicts the observations of each pixel in the imagery
  results<-predict(RF_mod,x[-1:-2])
  return(results)}
  
  # Converts prediction from rf model to dataframe and changes column name to predicte
  Results_df<-as.data.frame(results)%>%
    'names<-'("predicted")
  
  # Creates Unique IDs for classes/categories 
  Unique_class<-unique(as.data.frame(Results_df$predicted)) 
  Unique_class$Category<-seq(1:nrow(Unique_class))
  names(Unique_class)[1]<-"predicted"
  
  # Create dataframe with unique class IDs and location info
  Results_final<-merge(Results_df,
                       Unique_class, 
                       by="predicted")%>% 
    dplyr::select(x,y,PFT_ID)
  
  data<-rasterFromXYZ(Results_final, crs = crs("need to find a projection to use"))}
# TEST_CASE<-Hyperspec_results(HyperspecPreds[[2]])
  
  # Create plots of outputs
  lapply(1:length(data),function(x){
    randomcolours<-distinctColorPalette(nrow(Unique_class))
    jpeg(paste(outputs_folder,gsub("PredsDF","plot",names(RF_mod[x])),
               ".jpg",sep=""), width = 1200, height = 700)
    plot(
      x,
      legend = FALSE,
      axes=FALSE,
      col = chm_colors,
      box= FALSE,
      xlab="Longitude", 
      ylab="Latitude"
    )
    legend(
      "right",
      legend = c(paste(Unique_class$predicted)),
      fill = chm_colors,
      border = FALSE,
      bty = "n",
      cex=1.5,
      xjust =1,
      horiz = FALSE,
      inset = -0.009,
      par(cex=0.4)
      
    )             
    dev.off()
  })
  
})







