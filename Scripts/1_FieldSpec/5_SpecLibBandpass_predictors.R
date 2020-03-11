## Calculates reflectance values for bands resampled every 5, 10 , 50 and 100nm (Only headwall sensor)
## Calculates the vegitation index
## Need to run scripts in 1_By_site_PSR, 2 and 3 in folder "1_FieldSpec/" prior this script if the data is not present in the output folder below
## 'OutputsPSR/Processing/Sensors/'
## At the end of the script memeroy will be cleaned (to stop this put a # infront the last line of code)
library(spectrolab)
library(tidyverse)
library(hsdar)

source("Functions/SpeclibPredictor_generator.R")

# Lets create names for output and input folders
# Input folder is the dir path to where yor data is stored (you can have more than one input folders)
# output folder is the dir path to where you want your processed data to be stored
# Replace these before running 
outputs_folder<-"OutputsPSR/Processing/Sensors/"
input_folder  <-"OutputsPSR/Processing/Sensors/"

# Import names of Spectral libraries into a list
# For now we'll work with headwall and AVIRIS
names_SpecLibs = list.files(input_folder, 
                            pattern="SpecLib",full.names = T)[2] 

SpeclibPreds<-lapply(names_SpecLibs,Spectral_Predictors)%>%
  
  # Rename objects in list
  setNames(gsub(input_folder,"",names_SpecLibs)) 

# Writes out each dataframe as a .csv file
lapply(1:length(SpeclibPreds), function (x) 
  write.csv(SpeclibPreds[[x]],
            file = paste("OutputsPSR/Processing/Sensors/",
                         gsub("_SpecLib.csv","_PredsDF",names (SpeclibPreds[x])),
                         '.csv',sep=""), row.names = F))

# Cleans up R memeory
#rm(list=ls())




