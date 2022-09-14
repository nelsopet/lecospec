library(future)
library(terra)
library(ranger)
library(caret)
library(magrittr)
library(doFuture)
library(stringr)
library(foreach)

source("Functions/lecospectR.R")

classify_image <- function(path, model){
    # verify if the file exists
    if(!file.exists(path)){
        stop(paste0("Specified file was not found: ", path))
    }


    # autodetect the system data
    system_information <- Sys.info()
    print(system_information)

    file_metadata <- file.info(path)
    print(file_metadata$size)
    
    # split into 1gb tiles if needed
    num_chunks <- ceiling(file_metadata$size / (1024 * 1024 * 1024))
    print(num_chunks)

    if(stringr::str_to_lower(system_information[[1]]) == "windows"){
        print("Detected windows... WHY U NO FORK PROCESS!?")
        future::plan(multisession)
    } else {
        future::plan(multicore)
    }

    doFuture::registerDoFuture()

    img <- terra::rast(path)

    foreach(tile_index = seq(num_chunks)){
        
    }
    

}

classify_image("Data/Quadrats/BisonGulchQuads.envi")
