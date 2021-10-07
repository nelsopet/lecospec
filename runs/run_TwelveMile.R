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

print("Loading Source Files")
source("/home/kbundy/lecospec-master/Functions/LandCoverEstimator.R")

lecospec_base_path <- "/home/kbundy/lecospec-master/"
setwd(lecospec_base_path)

print("Setting up I/O")
input_file <- "TwelveMile"
input_folder <- "/home/kbundy/lsData/alaska/"

output_parent_dir <- "/home/kbundy/lsData/Outputs/"
output_folder <- paste0(output_parent_dir, input_file, "/")
output_filename <- paste0("species_level_", input_file)

species_model <-  paste0(
    lecospec_base_path,
    "E_003_Pred_Model_RandomForest_species_1000trees.rda")
fg1_model <- paste0(
    lecospec_base_path,
    "E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda")
fg2_model <- paste0(
    lecospec_base_path,
    "E_003_Pred_Model_RandomForest_FncGrp2_1000trees.rda")

print("Running species level predictions")

species_predictions <- LandCoverEstimator(
    filename = paste0(input_folder, input_file),
    out_file = output_folder,
    #Classif_Model = "Output/E_003_Best_Model_RandomForest_86vars.rda",
    #Classif_Model = "Output/E_004_Best_Model_Ranger_86vars.rda",
    Classif_Model = species_model,
    datatype = "raster",
    extension = FALSE,
    output_filename = output_filename
)

gc()

print("Running functional group level predictions")
fg1_predications <- LandCoverEstimator(
    filename = paste0(input_folder, input_file),
    out_file = output_folder,
    #Classif_Model = "Output/E_003_Best_Model_RandomForest_86vars.rda",
    #Classif_Model = "Output/E_004_Best_Model_Ranger_86vars.rda",
    Classif_Model = fg1_model,
    datatype = "raster",
    extension = FALSE,
    output_filename = output_filename
)

gc()

print("Running coarse group predictions")
fg2_predication <- LandCoverEstimator(
    filename = paste0(input_folder, input_file),
    out_file = output_folder,
    #Classif_Model = "Output/E_003_Best_Model_RandomForest_86vars.rda",
    #Classif_Model = "Output/E_004_Best_Model_Ranger_86vars.rda",
    Classif_Model = fg2_model,
    datatype = "raster",
    extension = FALSE,
    output_filename = output_filename
)

gc()

# move tree data to ACG
# Alaska data to NASA
# 
