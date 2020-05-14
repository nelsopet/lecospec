# ------------------------------     Classify Image ------------------------------------------
# List of packages to install
library(spectrolab)
library(tidyverse)
library(raster)
library(SpaDES)
library(doParallel)
library(parallel)
library(hsdar)
library(caret)
library(ranger)
library(tools)
library(randomForest)

# Calls the function that will classify image
source("Functions/LandCoverEstimator.R")

# Classify Image using the HyperSpec_DerivGenerator function
# filename = file location to datacube
# out_file = output location for the predicted layer
# Classif_Model = calssification model built using spectral library
# datatype = is you data is a raster file or a .csv file
# extension = does your input file has a extension associated with it? e.g (.tif,.csv, .dat)

system.time(PredLayer<-LandCoverEstimator(filename = "Data/SubsetDatacube",
                                          out_file = "Output/",
                                          Classif_Model = "Output/E_003_Best_Model_RandomForest.rda",
                                          datatype = "raster",
                                          extension = FALSE))




system.time(PredLayer<-HyperSpec_DerivGeneratorRF(filename = "Data/FullDatacube",
                           out_file = "Output/",
                           Classif_Model = "Output/E_003_Best_Model_RandomForest.rda"))
