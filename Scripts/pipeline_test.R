library(tidyverse)
library(plyr)
library(raster)
library(useful)
library(SpaDES.tools)
library(spectrolab)



# set the paths to the necessary files
data_path <- "Data/SubsetDatacube"
interim_path <- "Data/Preprocessed_raster"
output_dir <- "Output/Test/results.grd"
tile_save_path <- "Output/Test/Tiles/Intermediate/"
tile_output_path <- "Output/Test/Tiles/Predictions/"
classifier_path <- "Output/E_004_Best_Model_Ranger.rda"

tile_size <- 50
# load the classifier
test_model <- get(load(classifier_path))

# load the functions and pipeline
source("Functions/LandCoverEstimaror_RF.R")
source("Scripts/pipeline.R")
source("Functions/slice_raster_brick.R")

# run LecoSpec!
preprocess_image(
    data_path,
    interim_path, 
    test_model
)

create_tiles(
    interim_path, 
    tile_save_path, 
    tile_size = tile_size
)

results <- process_tiles(
    test_model,
    tile_save_path,
    tile_output_path)

raster::writeRaster(results, output_dir)