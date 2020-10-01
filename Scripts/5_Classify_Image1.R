# ------------------------------     Classify Image ------------------------------------------
# List of packages to install
# Need to find a way to easily install these
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

system.time(PredLayer<-LandCoverEstimator(filename = "Data/Little_LakeTest",
                                          out_file = "Output/",
                                          Classif_Model = "Output/E_003_Best_Model_RandomForest_86vars.rda",
                                          datatype = "raster",
                                          extension = FALSE))

