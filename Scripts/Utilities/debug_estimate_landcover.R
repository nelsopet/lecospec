source("./Functions/lecospectR.R")


#Test estimate land cover on a single quadrat image
#set.seed(1234)
test_path <- "./Data/Ground_Validation/Imagery/BisonGulchQuads.envi"

    input_filepath = test_path
    #model = NULL #Defined in config
    config_path = "./config.json"
    #outlier_processing = NULL,
    #transform_type = NULL,
    cache_filepath = "./"
    output_filepath =  paste(
        "output-",
        format(Sys.time(), "%a-%b-%d-%H-%M-%S-%Y"), ".envi", sep="")
    use_external_bands = TRUE
    is_classifier = NULL


    path <- getwd()
    #write terminal output to the log file

    # Read in the configuration file
    config <- rjson::fromJSON(file = config_path)


    # set the raster datatype from the arguments (if provided),
    # or if none is provided, load it from the config file
    #raster_datatype <- NULL
    #if(!is.null(is_classifier)){
    #    if(is_classifier) {
            raster_datatype <- "INT2U"
    #    } else {
    #        raster_datatype <- "FLT4S"
    #    }
    #} else {
    #    raster_datatype <- config$is_classifier
    #}


    # Load the model
    #if(is.null(model)){
    model <- load_model(config$model_path)
    #}

    # load the input datacube and split into tiles
    input_raster <- raster::brick(input_filepath)
    input_crs <- raster::crs(input_raster)
    input_extent <- raster::extent(input_raster)

    #if(is.na(input_crs)){
    #    warning("The input raster does not have a CRS specified.")
    #}

    # save the band names since they will be lost using .envi tiles
    bandnaimz <- names(input_raster)
    #if(use_external_bands){
        band_count <- raster::nlayers(input_raster)
        bandnaimz <- read.csv(config$external_bands)$x[1:band_count] %>%
            as.vector()
        names(input_raster) <- bandnaimz
    #}

    num_tiles_x <- config$x_tiles
    num_tiles_y <- config$y_tiles

    #if(config$automatic_tiling){
    #    num_tiles_x <- calc_num_tiles(
    #        input_filepath,
    #        max_size = config$max_size)
    #    num_tiles_y <- calc_num_tiles(
    #        input_filepath,
    #        max_size = config$max_size)
    #}

    tile_filenames <- make_tiles(
        input_raster,
        num_x = num_tiles_x,
        num_y = num_tiles_y,
        save_path = config$tile_path,
        verbose = FALSE
    )

    # determine the number of cores to use
    num_cores <- parallel::detectCores() - 1#detect cores on system
    # see if the number of cores to use is specified in the config
    #if(is.integer(config$clusterCores)){
        num_cores <- config$clusterCores
    #}
    # set up the parallel cluster
    raster::beginCluster(num_cores)
    cl <- raster::getCluster()
    print(cl)


    #print(paste0(parallel::detectCores(), " Cores Detected for processing..."))
    #print(paste0("Cluster initialized with ", num_cores, " processes"))
    background_blas_threads <- RhpcBLASctl::get_num_procs()
    background_omp_threads <- RhpcBLASctl::omp_get_max_threads()

    # load the outlier processing method if none is specified by user
    outlier_processing_cfg <- outlier_processing
    #if(is.null(outlier_processing)){
        outlier_processing_cfg <- config$outlier_processing
    #}

    # load transform type if none is specified
    #transform_type_cfg <- transform_type
    #if(is.null(transform_type)){
        transform_type_cfg <- config$transform_type
    #}



    print("removing raster from memory")
    rm(input_raster)
    gc()

    prediction_filenames <- lapply(
        tile_filenames,
        function(tile_filename){
            return(.convert_tile_filename(tile_filename))
    }) %>% as.vector()
    #print(paste0("here is a list of tile names for predictions ",prediction_filenames))
    # initialize the variable for the tilewise results
    tile_results <- vector("list", length = length(tile_filenames))
    #edge artifacts?
    #print(paste0("here is that same list of tiles as a vector ",tile_results))
    #print(paste0("here is length of that list of tile ",length(tile_filenames)))


    # exports <- c()



    #if(config$parallelize_by_tiles){
    #    #doSNOW::registerDoSNOW(cl)
    #    doFuture::registerDoFuture(cl)
    #    tile_results <- foreach::foreach(
    #        i = seq_along(tile_filenames),
    #        .export = as.vector(ls(.GlobalEnv))
    #    ) %dopar% {
    #        gc()
    #        sink(get_log_filename(tile_filenames[[i]]))
    #        tile_result <- process_tile(
    #            tile_filename = tile_filenames[[i]],
    #            ml_model = model, 
    #            aggregation = config$aggregation,
    #            cluster = NULL,
    #            return_raster = TRUE,
    #            band_names = bandnaimz,
    #            bandwidth = config$bandwidth,
    #            outlier_processing = outlier_processing_cfg,
    #            transform_type = transform_type_cfg,
    #            return_filename = TRUE,
    #            save_path = prediction_filenames[[i]],
    #            suppress_output = TRUE,
    #            raster_datatype = raster_datatype)
    #        sink(NULL)
    #        return(tile_result)
    #    }
    #} else {

##########Move to debug_process_tile.R from here after running lines above this one
        print("serial tile processing starts now")
        tile_results <- foreach::foreach(
            i=seq_along(tile_filenames)
        ) %do% {
            gc()
            #sink(get_log_filename(tile_filenames[[i]]))
            tile_result <- process_tile(
                tile_filename = tile_filenames[[i]],
                ml_model = model, 
                aggregation = config$aggregation,
                cluster = cl,
                return_raster = TRUE,
                band_names = bandnaimz,
                bandwidth = config$bandwidth,
                outlier_processing = outlier_processing_cfg,
                transform_type = transform_type_cfg,
                return_filename = TRUE,
                save_path = prediction_filenames[[i]],
                suppress_output = TRUE,
                raster_datatype = raster_datatype)
        sink(NULL)
        return(tile_result)
        }
    }
    gc() #clean up

    

    #print("Tile based processing complete")
    raster::endCluster()
    #print(tile_results)

    # return the background thread configuration to its initial state
    if(config$parallelize_by_tiles){
        RhpcBLASctl::blas_set_num_threads(background_blas_threads)
        RhpcBLASctl::omp_set_num_threads(background_omp_threads)
    }

    # merge and save the results.
    results <- merge_tiles(
        prediction_filenames, 
        output_path = output_filepath, 
        raster_datatype = raster_datatype)
    # load the results from disk to correct data type issues from float/INT2U (C++ uint16_t) conversion
    results <- raster::raster(output_filepath)

    return(results)
}