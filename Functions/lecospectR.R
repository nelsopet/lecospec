require(tidyverse)
require(compiler)
require(raster)
require(hsdar)
require(spectrolab)
require(ranger)
require(stringr)
require(stringi)
require(rjson)
require(snow)
require(doSNOW)
require(stats)
require(rasterVis)


source("Functions/spectral_operations.R")
source("Functions/raster_operations.R")
source("Functions/dataframe_operations.R")
source("Functions/model_support.R")
source("Functions/utilities.R")
source("Functions/validation.R")
source("Functions/visualization.R")
source("Functions/pfts.R")
source("Functions/type_conversion.R")
source("Functions/training_utilities.R")


#################################################################
###     Main Function for tiled processing 
#################################################################



#' equivalent to the old LandCoverEstimator()
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
estimate_land_cover <- function(
    input_filepath,
    config_path = "./config.json",
    cache_filepath = "./",
    output_filepath =  paste(
        "output-",
        format(Sys.time(), "%a-%b-%d-%H-%M-%S-%Y"), ".envi", sep=""),
    use_external_bands = TRUE
) {

    path <- getwd()
    #write terminal output to the log file

    # Read in the configuration file
    config <- rjson::fromJSON(file = config_path)

    # determine the number of cores to use
    num_cores <- parallel::detectCores() - 1#detect cores on system
    # see if the number of cores to use is specified in the config
    if(is.integer(config$clusterCores)){
        num_cores <- config$clusterCores
    }
    # set up the parallel cluster
    raster::beginCluster(num_cores)
    cl <- raster::getCluster()
    print(cl)


    print(paste0(parallel::detectCores(), " Cores Detected for processing..."))
    print(paste0("Cluster initialized with ", num_cores, " processes"))
    background_blas_threads <- RhpcBLASctl::get_num_procs()
    background_omp_threads <- RhpcBLASctl::omp_get_max_threads()

    # Load the model
    model <- load_model(config$model_path)

    # load the input datacube and split into tiles
    input_raster <- raster::brick(input_filepath)
    input_crs <- raster::crs(input_raster)
    input_extent <- raster::extent(input_raster)

    if(is.na(input_crs)){
        warning("The input raster does not have a CRS specified.")
    }

    # save the band names since they will be lost now that we are using .envi tiles 
    bandnames <- names(input_raster)
    if(use_external_bands){
        band_count <- raster::nlayers(input_raster)
        bandnames <- read.csv(config$external_bands)$x[1:band_count] %>% as.vector()
        names(input_raster) <- bandnames
    }

    num_tiles_x <- config$x_tiles
    num_tiles_y <- config$y_tiles

    if(config$automatic_tiling){
        num_tiles_x <- calc_num_tiles(
            input_filepath,
            max_size = config$max_size)
        num_tiles_y <- calc_num_tiles(
            input_filepath,
            max_size = config$max_size)
    }

    tile_filenames <- make_tiles(
        input_raster,
        num_x = num_tiles_x,
        num_y = num_tiles_y,
        save_path = config$tile_path,
        cluster = cl,
        verbose = FALSE
    )


    rm(input_raster)
    gc()

    prediction_filenames <- lapply(
        tile_filenames,
        function(tile_filename){
            return(.convert_tile_filename(tile_filename))
    }) %>% as.vector()

    # initialize the variable for the tilewise results
    tile_results <- vector("list", length = length(tile_filenames))
    #edge artifacts?


    # exports <- c()



    if(config$parallelize_by_tiles){
        #doSNOW::registerDoSNOW(cl)
        doParallel::registerDoParallel(cl)
        tile_results <- foreach::foreach(
            i = seq_along(tile_filenames),
            .export = as.vector(ls(.GlobalEnv))
        ) %dopar% {
            gc()
            sink(get_log_filename(tile_filenames[[i]]))
            tile_result <- process_tile(
                tile_filename = tile_filenames[[i]],
                ml_model = model, 
                aggregation = config$aggregation,
                cluster = NULL,
                return_raster = TRUE,
                band_names = bandnames,
                return_filename = TRUE,
                save_path = prediction_filenames[[i]],
                suppress_output = TRUE)
            sink(NULL)
            return(tile_result)
        }
    } else {
        tile_results <- foreach::foreach(
            i=seq_along(tile_filenames)
        ) %do% {
            gc()
            sink(get_log_filename(tile_filenames[[i]]))
            tile_result <- process_tile(
                tile_filename = tile_filenames[[i]],
                ml_model = model, 
                aggregation = config$aggregation,
                cluster = cl,
                return_raster = TRUE,
                band_names = bandnames,
                return_filename = TRUE,
                save_path = prediction_filenames[[i]],
                suppress_output = TRUE)
        sink(NULL)
        return(tile_result)
        }
    }
    gc() #clean up

    

    print("Tile based processing complete")
    raster::endCluster()
    print(tile_results)

    # return the background thread configuration to its initial state
    if(config$parallelize_by_tiles){
        RhpcBLASctl::blas_set_num_threads(background_blas_threads)
        RhpcBLASctl::omp_set_num_threads(background_omp_threads)
    }

    # merge and save the results.
    results <- merge_tiles(prediction_filenames, output_path = output_filepath)
    # load the results from disk to correct data type issues from float/INT2U (C++ uint16_t) conversion
    results <- raster::raster(output_filepath)

    return(results)
}



#################################################################
###     Main Function for single-file processing 
#################################################################

#' processes a small raster imarge
#'
#' processes the given image in-memory.  This assumes that the image is small enough that tiling is not required.  
#' Functions include data munging, imputation, automated vegetation index calculation, model inference, and data type conversion. 
#' The outputs are optionally saved to disk as well.  
#' Parallization is used if a connection to a parallel (or raster) package cluster is provided
#'
#' @return 
#' @param tile_filename: A string specifying the location of the target raster on the disk
#' @param ml_model: the machine learning model for prediction.  
#' @param cluster: a cluster, from the raster::beginCluster(); raster::getCluster() or parallel::makeCluster().  Default is NULL (no parallelism).
#' @param return_raster: (default: TRUE) returns a rasterLayer object if true, or a data.frame if FALSE.
#' @param save_path: the path to save the output.  If NULL (default) no file is saved.  Otherwise it attempts to save the file to the location specified.
#' @param suppress_output: if TRUE, returns the save location of the output, rather than the output itself.  
#' If FALSE (default), the function returns a raster::rasterLayer or base::data.frame as determined by return_raster parameter.
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
process_tile <- function(
    tile_filename,
    ml_model, 
    aggregation,
    cluster = NULL,
    return_raster = TRUE,
    band_names=NULL,
    standardize_input = FALSE,
    normalize_input = FALSE,
    scale_input = FALSE,
    robust_scale_input = FALSE,
    return_filename = FALSE,
    save_path = NULL,
    suppress_output = FALSE
    ) {
    raster_obj <- raster::brick(tile_filename)
    input_crs <- raster::crs(raster_obj)
    print(paste0("preprocessing raster at ", tile_filename))
    base_df <- preprocess_raster_to_df(raster_obj, ml_model, band_names=band_names)
    #print(input_crs)
    
    if(nrow(base_df) < 2){
        #print("The tile has no rows!")
        #print(dim(base_df))
        handle_empty_tile(
            raster_obj,
            save_path = save_path,
            target_crs = input_crs)

        if(!suppress_output){
            if(return_raster){
                return(raster_obj)
            } else {
                return(base_df)
            } 

        } 
        #print(save_path)
        return(unlist(save_path))
        # add return value if output is suppressed
    } else {
        # this runs if and only if there is sufficient data
    

            #if there is no data, return the empty tile in the specified format

        rm(raster_obj)
        gc()

        cleaned_df <- drop_zero_rows(base_df)
        rm(base_df)
        gc()


        cleaned_df_no_empty_cols <- drop_empty_columns(cleaned_df)
        
        imputed_df <- impute_spectra(cleaned_df_no_empty_cols, cluster = cluster)

        veg_indices <- get_vegetation_indices(imputed_df, ml_model, cluster = cluster)

        
    
        try(
            rm(cleaned_df)
        )# sometimes garbage collection gets there first, which is fine
        gc()

        # drop rows that are uniformly zero
      
        resampled_df <- resample_df(imputed_df, normalize = normalize_input, max_wavelength = 995.716)
        gc()



        #print("Resampled Dataframe Dimensions:")
        #print(dim(resampled_df))
        #print("Index Dataframe Dimensions:")
        #print(dim(veg_indices))

        df <- cbind(resampled_df, veg_indices)
        #print("Input Data Columns")
        #print(colnames(df))
        #df <- df %>% dplyr::select(x, y, dplyr::all_of(target_model_cols)) 
        # above line should not be needed, testing then deleting
        rm(veg_indices)
        rm(resampled_df)
        gc()

        imputed_df_full <- impute_spectra(df, method="median")

        if(scale_input){
            imputed_df_full <- columnwise_min_max_scale(
                imputed_df_full, 
                ignore_cols = c("x", "y")) %>%
                as.data.frame()
        }
        if(robust_scale_input){
            imputed_df_full <- columnwise_robust_scale(
                imputed_df_full, 
                ignore_cols = c("x", "y")
                ) %>% as.data.frame()
        }
        
        if(standardize_input){
            imputed_df_full <- standardize_df(imputed_df_full, ignore_cols = c("x", "y"))
        }


        
        prediction <- apply_model(imputed_df_full, ml_model)
        rm(df)
        gc()
        
        prediction <- postprocess_prediction(prediction, imputed_df_full)

        prediction <- convert_and_save_output(
            prediction,
            aggregation,
            save_path = save_path,
            return_raster = return_raster,
            target_crs = input_crs)

        
        raster::crs(prediction) <- input_crs

        if(suppress_output){
            #print(save_path)
            return(unlist(save_path))
        }
        return(prediction)
    }
}
