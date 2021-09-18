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

# increase allocated memory to approx to appropriate amount 
# (leaving 2GB reserved for OS)
system_ram_gb <- 32
malloc_size <- (system_ram_gb - 2) * 8000

memory.limit(size = malloc_size)

# Calls the function that will classify image
source("Functions/LandCoverEstimator.R")

# Classify Image using the HyperSpec_DerivGenerator function
# filename = file location to datacube
# out_file = output location for the predicted layer
# Classif_Model = calssification model built using spectral library
# datatype = is you data is a raster file or a .csv file
# extension = does your input file has a extension associated with it?
# e.g (.tif,.csv, .dat)



system.time(PredLayer <- LandCoverEstimator(

#    filename = "M:/Alaska_Datacubes/Raw_files/WickershamDome_2019_08_08_19_31_51_2000_rd_rf_or",
#    filename = "Data/SubsetDatacube",
    filename = "./Data/WickershamDome_2019_08_08_19_31_51_2000_rd_rf_or",
    out_file = "Output/",
    #Classif_Model = "Output/E_003_Best_Model_RandomForest_86vars.rda",
    #Classif_Model = "Output/E_004_Best_Model_Ranger_86vars.rda",
    Classif_Model = "Output/E_004_Best_Model_Ranger.rda",
    datatype = "raster",
    extension = FALSE,
    output_filename = "WickershamDomeOutput"))

#write.csv(PredLayer, "predicted_layer.csv")
raster::writeRaster(PredLayer, "predlayer.tif")
plot(PredLayer)
