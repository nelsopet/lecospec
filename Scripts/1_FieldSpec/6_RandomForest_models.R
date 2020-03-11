## Builds a random forest model from PSR data
## Need to run scripts 1_By_site_PSR, 2, 3, 4 in folder "1_FieldSpec/" prior this script if the data is not present in the output folder below
## 'OutputsPSR/Processing/Sensors/'
## At the end of the script memeroy will be cleaned (to stop this put a # infront the last line of code)
library(tidyverse)
library(randomForest)
library(randomForestExplainer)

# Lets create names for output and input folders
# Input folder is the dir path to where yor data is stored
# output folder is the dir path to where you want your processed data to be stored
# Replace these before running 
outputs_folder<-"Outputs/PSR/Models"
input_folder  <-"OutputsPSR/Processing/Sensors/"

# Import names of Spectral libraries and thier predictors
# For now we'll work with headwall and AVIRIS
names_SpecLibPreds = list.files(input_folder, pattern="PredsDF",full.names = T)[2]

# Reads in spectral library and their predictors for each sensor
SpecLibs_Preds<-lapply(names_SpecLibPreds,read.csv)%>% 
  
  # Removes dir path from the name
  setNames(gsub(input_folder,"",names_SpecLibPreds)) 


# Function Builds a random forest Model and selects the 50 most important variables
# This takes a while to run because it calculates distribution of minimal depth (need to optimize this) 
# The result of the function will be a plot
ImportantVars<-function(x){
  
  # Creates a string of possible names that will be removed
  remove_names<-c("ScanID","PFT","PFT_2","PFT_4","Area","PFT2_Freq"
                  ,"PFT3_Freq","PFT4_Freq","Tree_numbe","x","y")
  
  # Removes unwanted metadata from dataframe 
  # (the only columns we need are the PFT_3 column and all the predictors)
  x[remove_names] = NULL
  name<-colnames(x[,1])
  # Creates Random forest Model
  set.seed(2017)
  rf_mod<-randomForest(name ~.,data = df,
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
}# Function ends

# Test Function
# Test_case1<-ImportantVars(SpecLibs_Preds[[1]])

# Function takes the most important variables, rebuilds and save a new model
Randomforest_mod<-function(x){
  # creates a dataframe with the Important varibles
  mostImportantVars<-ImportantVars(x)
  
  # Grabs the 50 most important variables from predictors dataframe
  ImpVars<-unique(mostImportantVars$data$variable)%>%as.character()%>%
    as.character()
  
  # Creates a new model built on important variables
  # Need to change PFT_3 to classes
  # Classes should be the column you're trying to predict in a classification problem
  newdf<-x%>%
    dplyr::select(PFT_3,ImpVars)
  
  rfNew<-randomForest(PFT_3 ~.,data = newdf,
                      mtry = sqrt(ncol(newdf)),
                      ntree = 1001,
                      localImp = TRUE)
  # You could write out the confusion matrix
  # You could save the model to desired folder
  return(rfNew)
}

# Test function below
# Test_case2<-Randomforest_mod(SpecLibs_Preds[[2]])































