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
    print(colnames(df))
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

calc_veg_index <- function(spec_library, subset = NA, use_nearest = TRUE) {
      av <- sort(
                c(
                    "NDVI","OSAVI","SAVI","MTVI","NDWI","PWI",
                    "MSI", "SRWI","GMI1","GMI2","MCARI","TVI",
                    "Vogelmann4","Boochs","Boochs2",
                    "CARI","CI","Carter","Carter2","Carter3","Carter4",
                    "Carter5","Carter6","Datt","Datt2","Datt3","Datt4",
                    "Datt5","Datt6","DD","DDn","D1","D2","EVI","EGFR","EGFN",
                    "GI","Gitelson","Gitelson2","Green NDVI","MCARI/OSAVI",
                    "MCARI2","MCARI2/OSAVI2","mNDVI","mND705","Maccioni",
                    "mREIP","MSAVI","mSR","mSR705","mSR2","MTCI","NDVI2",
                    "NDVI3","NPCI","OSAVI2","RDVI","REP_LE","REP_Li",
                    "SIPI","SPVI","SR","SR1","SR2","SR3","SR4","SR5","SR6",
                    "SR7", "SR8","SRPI","Sum_Dr1","Sum_Dr2","TCARI","TCARI2",
                    "TCARI/OSAVI","TCARI2/OSAVI2","Vogelmann","NDLI",
                    "Vogelmann2","Vogelmann3","PRI","CAI","NDNI",
                    "PSSR", "PSND", "CRI1", "CRI2", "CRI3",
                    "CRI4", "MPRI", "PRI*CI2", "CI2", "PSRI", "ClAInt", 
                    "TGI", "PRI_norm","PARS","DPI","Datt7","Datt8",
                    "GDVI_2","GDVI_3","GDVI_4","LWVI1","LWVI2",
                    "DWSI1","DWSI2","DWSI3","DWSI4","DWSI5",
                    "SWIR FI", "SWIR LI", "SWIR SI", "SWIR VI"
                )
            )

            if(is.na(subset)) {
                # if no indeices are specified, return all of them.
                all_indices_from_lib <- hsdar::vegindex(spec_library, av, weighted = (! use_nearest))
                return(all_indices_from_lib)
            } else {
                # if a subset of indeices is specified, filter first
                all_indices_from_lib <- hsdar::vegindex(spec_library, av[subset], weighted = (! use_nearest))
                return(all_indices_from_lib)
            }
}


calc_headwall_veg_index <- function(spec_library) {
    headwall_bands <- -c(
                3, 26, 27, 31, 32, 33,
                35, 48, 49, 58, 60, 66, 67,
                71, 82, 99, 102, 103, 104, 105)

    indices <- calc_veg_index(
        spec_library,
        subset = headwall_bands,
        use_nearest = TRUE)

    return(indices)
}

calculate_aviris_veg_index <- function(spec_library) {
    indices <- calc_veg_index(
        spec_library, 
        subset = c(-58),
        use_nearest = TRUE)
    return(indices)
}

resample_df <- function(df) {
   df_no_metadata <- extract_bands(df)
   print("DF after removing metadata")
   print(summary(df_no_metadata))
    spec_library <- spectrolab::as_spectra(df)
    df_resampled <- spectrolab::resample(
        spec_library,
        seq(397.593,899.424,5)
    ) %>%
        as.data.frame() %>%
        dplyr::select(-sample_name)

    colnames(df_resampled) <- paste(
        colnames(df_resampled),
        "5nm", 
        sep="_"
    )
    combined_df <- cbind(remove_band_column(df), df_resampled)

    return(combined_df)
}




quick_veg_index <- function(df) {
    spec_lib <- df_to_speclib(df)

    veg_indices <- NA
    if(length(colnames(df))==272) {
        veg_indices <- calc_headwall_veg_index(spec_lib)
    } else {
        veg_indices <- calc_aviris_veg_index(spec_lib)
    }
    veg_index_df <- speclib_to_dataframe(veg_indices)
    
    combined_df <- cbind(df, veg_index_df)
    return(combined_df)
}

adjoin_veg_index <- function(df, index_names) {
    spec_lib <- dataframe_to_speclib(df)
    indices_lib <- hsdar::vegindex(
        spec_lib,
        index = index_names,
        weighted = FALSE
        )
    indices_df <- speclib_to_dataframe(indices_lib)
    output_df <- cbind(df, indices_df)
    return(output_df)
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
apply_pipeline <- function(data, pipeline_functions) {
    pipeline_length <- length(pipeline_functions)
    x <- pipeline_functions[[1]](data)

    for (f in pipeline_functions[2:pipeline_length]) {
        x <- f(x)
    }

    return(x)
}

get_var_names <- function(ml_model) {
    Vars_names <- c(ml_model$forest$independent.variable.names) 
    Vars_names2 <- gsub("^X", "", Vars_names[1:length(Vars_names)])
    return(Vars_names2)
}

get_required_veg_indices <- function(ml_model) {
    var_names <- get_var_names(ml_model)
     av <- sort(
                c(
                    "NDVI","OSAVI","SAVI","MTVI","NDWI","PWI",
                    "MSI", "SRWI","GMI1","GMI2","MCARI","TVI",
                    "Vogelmann4","Boochs","Boochs2",
                    "CARI","CI","Carter","Carter2","Carter3","Carter4",
                    "Carter5","Carter6","Datt","Datt2","Datt3","Datt4",
                    "Datt5","Datt6","DD","DDn","D1","D2","EVI","EGFR","EGFN",
                    "GI","Gitelson","Gitelson2","Green NDVI","MCARI/OSAVI",
                    "MCARI2","MCARI2/OSAVI2","mNDVI","mND705","Maccioni",
                    "mREIP","MSAVI","mSR","mSR705","mSR2","MTCI","NDVI2",
                    "NDVI3","NPCI","OSAVI2","RDVI","REP_LE","REP_Li",
                    "SIPI","SPVI","SR","SR1","SR2","SR3","SR4","SR5","SR6",
                    "SR7", "SR8","SRPI","Sum_Dr1","Sum_Dr2","TCARI","TCARI2",
                    "TCARI/OSAVI","TCARI2/OSAVI2","Vogelmann","NDLI",
                    "Vogelmann2","Vogelmann3","PRI","CAI","NDNI",
                    "PSSR", "PSND", "CRI1", "CRI2", "CRI3",
                    "CRI4", "MPRI", "PRI*CI2", "CI2", "PSRI", "ClAInt", 
                    "TGI", "PRI_norm","PARS","DPI","Datt7","Datt8",
                    "GDVI_2","GDVI_3","GDVI_4","LWVI1","LWVI2",
                    "DWSI1","DWSI2","DWSI3","DWSI4","DWSI5",
                    "SWIR FI", "SWIR LI", "SWIR SI", "SWIR VI"
                )
            )
        # get the items in both vectors 
        # (i.e. veg indeices used by the classifier)
    veg_indices <- intersect(av, var_names)
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
correct_variable_names <- function(df, ml_model, save_path = NULL) {


    Vars_names2 <- get_var_names(ml_model)
    # Creates a new model built on important variables
    new_df <- df %>%
        dplyr::select(x,y,all_of(Vars_names2))

    if (!is.null(save_path)) {
        write.table(new_df, file = save_path, sep = ",")
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
resample_spectra <- function(spec_lib) {
    print("Not yet implemented")
    return(spec_lib)
}



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
     spectral_lib <- read.csv(filepath, check.names = F)
    
    spectral_lib <- deriv_combine(spectral_lib)
    
    write.csv(
        spectral_lib,
        paste(out_file,"D_002_SpecLib_Derivs",".csv", sep=""),
        row.names = F
        )
    
    # Normalize Values here
    return(spectral_lib)
}

# function to fill in missing values using partial mean matching
#' Lone line explanation
#'
#' Long Description here
#'
#' @return a dataframe with missing values filled
#' @param x: a dataframe to be imputed
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
    tile_outputs <- foreach::foreach(index=seq_along(tiles)) %dopar% 
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
    spectral_lib <- read.csv(
        filename,
        check.names = F)

    spectral_lib <- Deriv_combine(spectral_lib)

    write.csv(
        spectral_lib,
        paste(
            out_file,
            "D_002_SpecLib_Derivs",
            ".csv",
            sep = ""),
        row.names = F)

    # Normalize Values here
    return(spectral_lib)
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

#' Function Reads in the configuration file
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
load_config <- function(filename) {
    
}

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
    output_filepath = "./",
    cache_filepath = "./",
    output_filename = "predictions"
) {

    num_cores <- parallel::detectCores()
    print(paste0(num_cores, " Cores Detected for processing..."))
    ls_cluster <- parallel::makeCluster(num_cores)
    # Read in the configuration file
    config <- rjson::fromJSON(file = config_path)

    # Load the model and extract the relevant information
    model <- load_model(config$model_path)
    model_indices <- get_required_veg_indices(model)

    input_raster <- raster::brick(input_filepath)
    input_df <- raster::rasterToPoints(input_raster) 

    parallel::parApply(
        cl = ls_cluster,
        X = input_df
        FUN = process_pixel,
        MARGIN = 1
    )
    # load the input datacube and split into tiles
    
    tiles <- make_tiles(input_raster, num_tiles = config$tiles)
    #edge artifacts?

    tile_results <- parallel::parLapply(tile=tiles) %dopar% { process_tile(tile)}

    results <- merge_results(tile_results)

    return(results)
}

process_tile <- function(tile) {
    
}

clean_tile_df <- function(tile) {

}