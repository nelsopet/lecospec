source("Functions/LandCoverEstimaror_RF.R")

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

    # stop the parallel stuff
    parallel::stopCluster(c1)

    # aggregate the data
    output_raster <- combine_tiles(tile_predictions, data)

    return(output_raster)

}

