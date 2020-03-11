## Builds a random forest model from PSR data
## Need to run scripts 1_By_site_PSR, 2, 3, 4 in folder "1_FieldSpec/" prior this script if the data is not present in the output folder below
## 'OutputsPSR/Processing/Sensors/'
## At the end of the script memeroy will be cleaned (to stop this put a # infront the last line of code)
library(tidyverse)
library(randomForest)
library(randomForestExplainer)

# Reads in random forest classifier
# The model classifies the data based on the 50 most important variables
source("Functions/Spectral_classifier.R")

# Lets create names for output and input folders
# Input folder is the dir path to where yor data is stored
# output folder is the dir path to where you want your processed data to be stored
# Replace these before running 
outputs_folder<-"Outputs/PSR/Models/"
input_folder  <-"OutputsPSR/Processing/Sensors/"

# Import names of Spectral libraries and thier predictors
# For now we'll work with headwall
names_SpecLibPreds = list.files(input_folder, pattern="PredsDF",full.names = T)

# Reads in spectral library and their predictors for each sensor
SpecLibs_Preds<-lapply(names_SpecLibPreds,Spectral_classifier)%>% 
  
  # Removes dir path from the name
  setNames(gsub(input_folder,"",names_SpecLibPreds))

# Unlist models to the environment
list2env(SpecLibs_Preds ,.GlobalEnv)

save(AVIRIS_PredsDF.csv  ,file = "OutputsPSR/Models/AVIRIS_models.rds" )
save(Headwall_PredsDF.csv,file = "OutputsPSR/Models/Headwall_model.rda")















