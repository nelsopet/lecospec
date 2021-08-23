source("Functions/LandCoverEstimaror_RF.R")
source("Functions/slice_raster_brick.R")

<<<<<<< Updated upstream
process_image <- function(filename, classifier, options = NULL) {
    # detect file type based on extension and load it
    data <- load_file(filename)

    data_tiles <- split_data(data)

    pipeline_functions <- c(
        func_VI,

    )

    # processes tiles through the entire pipeline in parallel.
    # The pipeline is currently hard-coded, but it can be easily modified.

    # Random Forest: 
    # output is dataframe with same size as input (not probability)
    # 

    # Set up the parallel infrastructure
    cores<- parallel::detectCores()-1
    c1 <- parallel::makeCluster(cores, setup_timeout = 0.5)
    doParallel::registerDoParallel(c1)

    # apply the pipeline to the tiles
    tile_derivs <- parallel::parLapply(
        c1,
        seq_len(length(data_tiles)),
        apply_pipeline(x, pipeline_functions)
        )
=======
preprocess_image <- function(filename,output_dir, classifier) {
    data_raster <- raster::brick(filename)
    data_df <- raster_to_dataframe(data_raster)
    #data_df <- correct_variable_names(data_df, classifier)
    
    print("data load complete")

    

    # impute missing data in the input
    imputed_df <- impute_spectra(data_df)
    #rm(data)
    #print(summary(as.data.frame(imputed_data)))

    

    preprocessed_raster <- dataframe_to_raster(imputed_df)
    
    raster::writeRaster(preprocessed_raster, output_dir, overwrite = TRUE)
}

create_tiles <- function(source_dir, tile_dir, tile_size) {
    input_brick <- raster::brick(source_dir)
    data_type <- raster::dataType(input_brick)
    result <- coolit.train::slice_raster_brick(
        input_brick,
        tile_w_px = tile_size,
        tile_h_px = tile_size,
        path = tile_dir,
        d_type = data_type,
        write_only = TRUE
        )
    return(result)
}

process_tiles <- function(
    classifier,
    input_dir,
    output_dir
    ) {
   
    # get the rqeuired veg indices
    used_indices <- get_required_veg_indices(classifier)
   

   # create wrappers for functions with more than one argument
    apply_model <- function(x) {
        predictions <- raster::predict(
            x,
            classifier,
            na.rm = TRUE,
            type='response',
            progress='text',
            fun = function(classifier, ...) {
                return(predict(classifier, ...)$predictions)
            })
        return(predictions)
    }

    set_variable_names_from_model <- function(df) {
        corrected_names_df <- correct_variable_names(df, classifier)
        print(colnames(corrected_names_df))
        return(corrected_names_df)
    }

    adjoin_model_veg_indices <- function(df) {
        return(adjoin_veg_index(df, used_indices))
    }

    

    # define the pipeline functions. Easily exended and modified
    pipeline_functions <- c(
        raster_to_dataframe,
        adjoin_veg_index,
        set_variable_names_from_model,
        apply_model,
        dataframe_to_raster
    )
>>>>>>> Stashed changes

    # stop the parallel stuff
    parallel::stopCluster(c1)

<<<<<<< Updated upstream
    # aggregate the data
    output_raster <- combine_tiles(tile_predictions, data)
=======
    # use a for loop to keep only one tile in memory at a time 
    # this is a significant slowdown compared to parLapply but solves memory issues
    tile_filenames <- list.files(
        input_dir,
        pattern = "*.tif",
        full.names = TRUE
        )
    for (i in seq_len(length(tile_filenames))) {
        tile <- raster::brick(tile_filenames[[i]])
        tile_output <- apply_pipeline(tile, pipeline_functions)
        save_tile(tile_output, tile_dir, index = i)
    }
    gc()
>>>>>>> Stashed changes

    return(output_raster)

}

