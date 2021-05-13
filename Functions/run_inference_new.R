source("Functions/LandCoverEstimator_RF.R")

target_filepath <- "Data/SubsetDatacube"


# initialize parallel processing 
# Get amount of cores to use
cores <- parallel::detectCores()-1

# prepare for parallel process
cluster <- parallel::makeCluster(cores, setup_timeout = 0.5)
doParallel::registerDoParallel(cluster)
# calculate vegetation indices (once only!)
base_vegetation_indices <- hsdar::vegindex()
headwall_veg_index <- calc_headwall_veg_index(
    cluster = cluster,
    base_index = base_vegetation_indices
    )

aviris_veg_index <- calc_aviris_veg_index(
    cluster = cluster,
    base_index = base_vegetation_indices
)

# load the bandpasses

# Load the model (classifier)
model_path <- "Output/model"




# Load the data
image_raster <- raster::brick(target_filepath)

# convert to data frame
data_df <- speclib_to_df(image_raster)
rm(image_raster)

# impute the data (RF model)

imputed_df <- impute_spectra(data_df, cluster = cluster)
rm(data_df)

# calculate the spectral derivatives
deriv_df <- calculate_veg_index(imputed_df)
rm()

# run model inference
colnames(deriv_df) <- clean_df_colnames(deriv_df)

tiles <- make_tiles(deriv_df)



pipeline_functions <- c(
    speclib_to_df,
    run_precdiction,
)


tile_results <- apply_pipeline_by_tile(deriv_df, pipeline_functions)
results_df <- aggregate_results(tile_results)
results_raster <- df_to_speclib(results_df)


# stop cluster
parallel::stopCluster(cluster)