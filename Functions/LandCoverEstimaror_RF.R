#' Functions returns columns that are bandpasses
#' 
#' @inheritParams None
#' @return a dataframe without non-bandpass columns
#' @param x: a dataframe
#' @seealso remove_band_column
#' @export 
#' @examples Not Yet Implmented
remove_meta_column <- function(x) {
    meta <- c(grep("^[0-9][0-9][0-9]", colnames(x)))
    colremove <- x[, meta]
    return(colremove)
}
  
#' Functions returns columns that are NOT bandpasses
#' 
#' Long Description here
#' 
#' @inheritParams None
#' @return explanation
#' @param x: a dataframe
#' @seealso remove_meta_column
#' @export 
#' @examples Not Yet Implmented
#' 
remove_band_column <- function(x) {   
    meta <- c(grep("[a-z A-Z]", colnames(x)))
    colremove <- x[, meta]
    return(colremove)
}

#' Resamples the given dataframe to every 'wavelength' nanometers
#'
#' @inheritParams None
#' @return A dataframe with the spectral compaonents
#' with the specified wavelength separation between bands
#' @param df: a dataframe
#' @param wavelength: The wavelength separation for columns, in nanometers.
#' @seealso None
#' @export
#' @examples Not Yet Implmented
resample_spectral_dataframe <- function(df, wavelength=5) {
    #Separate out data columns & convert to spectal object
    df_no_metadata <- remove_meta_column(df)
    speclib_df <- spectrolab::as_spectra(df_no_metadata)

    # resample to new data frame
    resampled_df_no_metadata <- spectrolab::resample(
        speclib_df, 
        seq(397.593, 899.424, wavelength)) %>%
        as.data.frame() %>%
        dplyr::select(-sample_name)

    # rename columns and add metadata
    colnames(resampleddf_no_metadata) <- paste(
        colnames(resampled_df_no_metadata),
        "5nm",
        sep = "_")
    resampled_df <- cbind(remove_band_column(df), resampled_df_no_metadata)

    return(resampled_df)
}

#' Extracts indices from data frame column names
#' 
#' Long Description here
#' 
#' @inheritParams None
#' @return explanation
#' @param df: A dataframe containing spectral data
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
extract_bands <- function(df){
    bands <- remove_band_column(df) %>%
        colnames() %>%
        as.numeric()
    return(bands)
}


#' Converts Data Frame to a spaectral library
#' 
#' @inheritDotParams 
#' @return A spectral library
#' @param df: A dataframe to convert
#' @export
#'
df_to_speclib <- function(df) {
    # Convert to a spectral library
    df_no_metadata <- remove_meta_column(df)
    spectral_matrix <- as.matrix(df_no_metadata)
    bands <- extract_bands(df)
    spectral_lib <- hsdar::speclib(spectral_matrix, bands)
    return(spectral_lib)
}

#' Converts a spaectral library to a dataframe.  This function is not yet implemented
#' 
#' @inheritDotParams 
#' @return A dataframe
#' @param speclib: A spectral library to convert
#' @export
#' 
speclib_to_df <- function(speclib) {
    df <- speclib %>%
        rasterToPoints() %>%
        as.data.frame()

        return(df)
}


#' Calculates the Vegetation Index (via hsdar package)
#' 
#' Long Description here
#' 
#' @inheritParams None
#' @return 
#' @param df: A dataframe of spectral data 
#' @param indices: (optional) base vegetation index for the calculations.
#' If none is supplied, one will be calculated using the hsdar package
#' @param aviris: The spectral indices of interest from the AVIRIS data
#' (default list of only -58)
#' @param headwall: The headwall indeices (bands) of interest.
#' Defaults to NULL, in which case the default bands will be used:
#' -{3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105}
#' @seealso hsdar::vegindex()
#' @export 
#' @examples Not Yet Implmented
.calculate_veg_index_from_df <- function(df,
    indices = hsdar::vegindex(),
    aviris = c(-58),
    headwall = NULL
    ) {
        headwall_bands <- headwall
        if(is.null(headwall)) {
            headwall_bands <- -c(
                3, 26, 27, 31, 32, 33,
                35, 48, 49, 58, 60, 66, 67,
                71, 82, 99, 102, 103, 104, 105)
        }
    

        
    calculatedVegIndex <- foreach()
}

#' One Line
#' 
#' Long Description here
#' 
#' @inheritParams None
#' @return explanation
#' @param
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#' 
.calculate_veg_index_from_speclib <- function(speclib) {

}

#' Calculates the vegetation indices for the input dataframe
#' 
#' Long Description here
#' 
#' @inheritParams None
#' @return explanation
#' @param
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#' 
calculate_veg_index <- function(x){
    if(is.data.frame(x)){
        return(.calculate_veg_index_from_df(x))
    } else {
       return(.calculate_veg_index_from_speclib(x))
    }
}


#' Applies a list of functions to the given input in order
#'
#' Long Description here
#'
#' @return explanation
#' @param data: the input data.  
#' @param functions: a vector or list of functions.
#' Each must take exactly one argument.
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#' 
apply_pipeline <- function(data, functions) {
    pipeline_length <- length(functions)
    x <- functions[1](data)

    for (f in functions[2:pipeline_length]) {
        x <- f(x)
    }

    return(x)
}

#' Calculates Headwall Vegetation Indices
#'
#' Long Description here
#'
#' @return 
#' @param cluster (optional): A parallel computing framework  
#' @param base_index (optional): vegetation indices for computations.  
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#' 
calc_headwall_veg_index <- function(
    cluster = NULL, 
    base_index = hsdar::vegindex() ){
    
    Headwall_VI <- base_index[
        -c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)
        ]
    
    c1 <- cluster
    if (is.null(cluster)) {
        # Get amount of cores to use
        cores <- parallel::detectCores()-1
        
        # prepare for parallel process
        c1<- parallel::makeCluster(cores, setup_timeout = 0.5) 
    }
    doParallel::registerDoParallel(c1)

    veg_indices <- foreach(
        i=1:length(Headwall_VI), 
        .combine = cbind, 
        .packages = 'hsdar') %dopar% {
        a<-hsdar::vegindex(
            spec_library, 
            index = Headwall_VI[[i]], 
            weighted = FALSE)}

    if(is.null(cluster)) {
        parallel::stopCluster(c1)
    }
    return(veg_indices)
}

#' Calculates AVIRIS Vegetation Indices
#'
#' Long Description here
#'
#' @return 
#' @param cluster (optional): A parallel computing framework  
#' @param base_index (optional): vegetation indices for computations.  
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#' 
calc_aviris_veg_index <- function(
    cluster = NULL,
    base_index = hsdar::vegindex()
    ) {

    AVIRIS_VI <- base_index[-58]
    c1 <- cluster
    if (is.null(cluster)){
        # Get amount of cores to use
        cores <- parallel::detectCores()-1
        
        # prepare for parallel process
        c1<- parallel::makeCluster(cores, setup_timeout = 0.5) 
    }
    doParallel::registerDoParallel(c1)

    veg_indices <- foreach(i=1:length(AVIRIS_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
        a<-hsdar::vegindex(spec_library, index=AVIRIS_VI[[i]], weighted = FALSE)}

    if(is.null(cluster)){
        parallel::stopCluster(c1)
    }

    return(veg_indices)
}



#' Resample the data frame/spectra every distance=5 units
#'
#' Long Description here
#'
#' @return 
#' @param df: a dataframe of spectral data
#' @param distance (default: 5nm): distance to resample bands
#' @return data frame of resampled data
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
resample_



#' COmbines derivatives (veg indices) and predictions
#'
#' Long Description here
#'
#' @return 
#' @param filepath (optional): A parallel computing framework
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
deriv_combine <- function(x, veg_index){

# Resampling Dataset
resampled_data <- resample_spectral_dataframe(x)
output_df <- cbind(veg_index,remove_meta_column(resampled_data))

return(output_df)
} # deriv_combine ends


#' Cleans the column names of all spaces
#' and other punctuation
#'
#' Long Description here
#'
#' @return a dataframe with the same contents 
#' but modified column neams
#' @param df: data.frame to have column names modified
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
clean_df_colnames <- function(df) {
    new_names <- stringr::str_remove_all(
        names(df),
        "[[:punct:]]| "
        )
    return(new_names)
}

#' Lone line explanation
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
load_csv <- function(filepath, output_type = "df") {
     Spectral_lib <- read.csv(filepath, check.names = F)
    
    Spectral_lib <- deriv_combine(Spectral_lib)
    
    write.csv(
        Spectral_lib,
        paste(out_file,"D_002_SpecLib_Derivs",".csv", sep=""),
        row.names = F
        )
    
    # Normalize Values here
    return(Spectral_lib)
}

# function to fill in missing values using partial mean matching
#' Lone line explanation
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
impute_spectra <- function(x, parallelize = NULL) {

    df <- x

    if (! is.data.frame(x)) {
        df <- x %>%
        rasterToPoints() %>%
        as.data.frame()
    }

    print("Imputing...")
    if (is.null(cluster)) {
        missForest::missForest(df, maxiter = 3)
    } else {
        missForest::missForest(df, maxiter = 3, parallelize = "forests")
    }
        
    if (! is.data.frame(x)) {
        print("Converting to Matrix")
        spectral_matrix <- as.matrix(df)
        print("Converting to Spectral Library")
        output_data <- raster::rasterFromXYZ(
            spectral_matrix,
            crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        raster::extent(output_data) <- raster::extent(x)
    }
    return(output_data)
}#end impute_spectra

#' creates tiles from 
#'
#' Long Description here
#'
#' @return 
#' @param data: 
#' @param num_tiles (default 100): 
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
make_tiles <- function(data, num_tiles = 100) {
    if (is.data.frame(data)) {
        raster <- df_to_speclib(data)
        tiles <- SpaDES.tools::splitRaster(raster, num_tiles)
        return(tiles)
    } else {
        tiles <- SpaDES.tools::splitRaster(, num_tiles)
        return(tiles)
    }

}

#' Lone line explanation
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
apply_pipeline_by_tile <- function(df, functions, cluster) {
    tiles <- make_tiles(df)
    tile_outputs <- foreach::foreach(index=seq_len(tiles)) %dopar% 
        apply_pipeline(tile, functions)
}

#' Lone line explanation
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
aggregate_results_df <- function(predictions, x, y) {

    dfx <- data.frame(x)
    colnames(dfx) <- c("x")
    dfxy <- cbind(dfx, y)
    z <- c()

    for (my_prediction in predictions) {
        prediction_df <- as.data.frame(
        raster::levels(my_prediction)[[1]], xy = TRUE)
        num_pixels <- nrow(prediction_df)
        print("Number of pixels in prediction")
        print(num_pixels)
        for (iter_idx in seq_len(num_pixels)) {
        if (!is.na(prediction_df[iter_idx, 2])) {
            z <- append(z, prediction_df[iter_idx, 2])
            }
        }
    }

    if (length(z) > nrow(dfxy)) {
        z <- z[seq_len(dfxy)]
    } else if (length(z) < nrow(dfxy)) {
        dfxy <- head(dfxy, n = length(z))
    } 
    df <- cbind(dfxy, z)
    return(df)
}

#' Lone line explanation
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
save_to_raster <- function(df, filepath) {
    rasterized_data <- df_to_speclib(df)
    raster::writeRaster(
        rasterized_data,
        filename = filepath,
        overwrite = TRUE
    )
}

#' Lone line explanation
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
plot_agg_results <- function(df, save_file = "Output/results.jpeg") {
    my_plot <- ggplot2::ggplot(data = df) +
        geom_point(aes(df$x, df$y, color=df$z))
    print(my_plot)
    return(my_plot)
}

# 
#' Function Reads in the data and replace/removes weird values
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
make_speclib_derivs<- function(filename)  {  
    # Reads in spectral libray as .csv
    # Right now your spectral library would have already have 
    # weird values removed/replaced
    Spectral_lib <- read.csv(
        filename,
        check.names = F)

    Spectral_lib <- Deriv_combine(Spectral_lib)

    write.csv(
        Spectral_lib,
        paste(
            out_file,
            "D_002_SpecLib_Derivs",
            ".csv",
            sep = ""),
        row.names = F)

    # Normalize Values here
    return(Spectral_lib)
}

#' Function Reads in the data and replace/removes weird values
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
load_model <- function(filepath) {

}