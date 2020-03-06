## Builds a random forest model from PSR data
## Need to run scripts 1, 2, 3, 4 in folder "1_Field_spec_processing/2_AK_SpecLib" prior this script if the data is not present in the output folder below
#'Outputs/Field_spec/Processing/Sensors/'
library(tidyverse)
library(randomForest)
library(randomForestExplainer)

# Import names of Spectral libraries and thier predictors
# For now we'll work with headwall and AVIRIS
names_SpecLibPreds = list.files("Outputs/Field_spec/Processing/Sensors/", pattern="PredsDF",full.names = T)

# Reads in spectral library and their predictors for each sensor
SpecLibs_Preds<-lapply(names_SpecLibPreds,read.csv)%>% 
  
  # Removes dir path from the name
  setNames(gsub("Outputs/Field_spec/Processing/Sensors/","",names_SpecLibPreds)) 

# Function Builds a random forest Model and selects the 50 most important variables
# This takes a while to run because it calculates distribution of minimal depth 
# The result of the function will be a plot
ImportantVars<-function(x){
  
  # Removes unwanted metadata from dataframe 
  # (the only columns we need are the PFT_3 column and all the predictors)
  x[c("ScanID","PFT","PFT_2","PFT_4","area","PFT2_Freq","PFT3_Freq","PFT4_Freq")] = NULL
  
  # Creates Random forest Model
  rf_mod<-randomForest(PFT_3~.,data = x,
                       mtry = sqrt(ncol(x)),
                       ntree = 1001,
                       localImp = TRUE)
  
  # Selects the 50 most important variables
  # This is achieved by calculating distribution of minimal depth 
  ImportantVars<-plot_min_depth_distribution(
    min_depth_distribution(rf_mod),
    min_no_of_trees = 200,
    mean_sample = "relevant_trees",
    k = 50)
  return(ImportantVars)
}

# Test
# ImportantVars(Put a dataframe of your spectral library here)
# ImportantVars(SpecLibs_Preds[["Headwall_PredsDF.csv"]])

# Function takes the Most importand variables, rebuilds and save a new model

Randomforest_mod<-function(x){
  
  # Grabs the 50 most important variables from predictors dataframe
  ImpVars<-unique(ImportantVars(x)$data$varible)
}
