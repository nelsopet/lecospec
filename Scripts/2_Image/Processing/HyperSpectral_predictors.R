## Scripts calculates the predictors for hyperspectral images
## Calculates reflectance values for bands resampled every 5, 10 , 50 and 100nm (Only headwall sensor)
## Calculates the vegitation index

library(spectrolab)
library(tidyverse)
library(hsdar)

# Lets create names for output and input folders
# Input folder is the dir path to where yor data is stored
# output folder is the dir path to where you want your processed data to be stored
# Replace these before running 
outputs_folder<-"OutputsIMG/Processing"
input_folder  <-"Original_data/Sensors/"

# Import names of bandpasses into character list
names_bandpasses = list.files(input_folder, pattern="bandpass",full.names = T)[2]

# Reads in bandpasses from different sensors
sensor_Bandpasses<-lapply(names_bandpasses,function(x){
  scan(x,numeric())
})%>% 
  
  # Removes dir path from the name
  setNames(gsub("Original_data/Sensors/","",names_bandpasses))

# Import names of Spectral libraries into a list
# For now we'll work with headwall and AVIRIS
names_HyperSpecImage = list.files(input_folder, 
                                  pattern = "envi$",full.names = T)

# Reads in spectral library for each sensor
HyperSpecImages <-lapply(names_HyperSpecImage,function(x){
  df<-brick(x)%>%
    rasterToPoints()%>%
    as.data.frame()
  df[275:328]<-NULL
  colnames(df)[-1:-2]<-sensor_Bandpasses[[1]]
  
  # converts NAs to zeros
  df[,-1:-2][is.na(df[,-1:-2])] <- 0
  
  # Converts negative values to 0s
  df[-1:-2][df[-1:-2] < 0] <- 0
  return(df)
})%>% 
  #Removes dir path from the name
  setNames(gsub(input_folder,"",names_HyperSpecImage)) 

# Reads in Spectral Predictor function that will create the predictors for the hyperspectral image
source("HypIMGPredictor_generator.R")

# Creates a list of dataframe objects that have the outputs from band resampling and 
# Vegitation index calculation
# test1<-ImagePredictor_generator(HyperSpecImages[[2]])
Image_preds<-lapply(HyperSpecImages,ImagePredictor_generator)

# Writes out each dataframe as a .csv file
lapply(1:length(Image_preds), function (x) 
  write.csv(Image_preds[[x]],
            file = paste("OutputsIMG/Processing/",
                         gsub("_envi","_PredsDF",names (Image_preds[x])),
                         '.csv',sep=""), row.names = F))








