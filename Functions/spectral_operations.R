#### DEPRECATED
source(("Functions/utilities.R"))


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
resample_spectral_dataframe <- function(
    df, 
    wavelength=5,
    start = 397.593,
    end = 899.424,
    normalize = TRUE) {
    #Separate out data columns & convert to spectal object
    df_no_metadata <- remove_meta_column(df)
    speclib_df <- spectrolab::as_spectra(df_no_metadata)

    if(normalize){
        speclib_df <- spectrolab::normalize(speclib_df)
    }

    # resample to new data frame
    resampled_df_no_metadata <- spectrolab::resample(
        speclib_df, 
        seq(start, end, wavelength)) %>%
        as.data.frame() %>%
        dplyr::select(-sample_name)

    # rename columns and add metadata
    colnames(resampleddf_no_metadata) <- paste0(
        "X",
        colnames(resampled_df_no_metadata),
        "5nm",
        sep = "_")
    resampled_df <- cbind(remove_band_column(df), resampled_df_no_metadata)

    return(resampled_df)
}



#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
resample_df <- function(df, normalize = TRUE) {
    spec_library <- df_to_speclib(df, type="spectrolab")
    
    if(normalize){
        spec_library <- spectrolab::normalize(spec_library)
    }

    speclib_resampled <- spectrolab::resample(
        spec_library,
        seq(397.593,899.424,5),
        parallel = FALSE
    ) 
    df_resampled <- speclib_to_df(speclib_resampled) %>% dplyr::select(-sample_name)

    colnames(df_resampled) <- paste0(
        "X",
        colnames(df_resampled),
        "_5nm"
    )
    combined_df <- cbind(remove_band_column(df), df_resampled)
    return(combined_df)
}


#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
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


## THIS IS A SPECTRAL OPERATION THAT EXPECTS A DATA.FRAME AS INPUT

#' Calculates AVIRIS Vegetation Indices
#'
#' Long Description here
#'
#' @return vegetation indices for supplied 
#' @param cluster (optional): A parallel computing framework  
#' @param base_index (optional): vegetation indices for computations.  
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#' 
get_vegetation_indices <- function(
    df,
    ml_model,
    cluster = NULL) {

    target_indices <- get_required_veg_indices(ml_model)
    # Creates a new model built on important variables
    # Initialize variable
    veg_indices <- NULL

    spec_library <- df_to_speclib(df)

    if(!is.null(cluster)){
        # cluster supplied, so use parallel execution
        #doSNOW::registerDoSNOW(cluster)
        doParallel::registerDoParallel(cluster)
        veg_indices <- foreach(
            i = seq_along(target_indices),
            .combine = cbind,
            .packages = c("hsdar", "spectrolab")) %dopar% {
                a <- hsdar::vegindex(
                    spec_library,
                    index = target_indices[[i]],
                    weighted = FALSE)
            }
    } else {
        # sequential calculation
        veg_indices <- foreach(
            i = seq_along(target_indices),
            .combine = cbind,
            .packages = c("hsdar","spectrolab")) %do% {
                a <- hsdar::vegindex(
                    spec_library,
                    index = target_indices[[i]],
                    weighted = FALSE)
            }

    }
    #colnames(veg_indices) <- target_indices
    index_df <- veg_indices %>% as.data.frame()

    colnames(index_df) <- clean_colnames(target_indices)
    
    return(index_df)
}



#' a quick function based on the original code
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_wavelength_strings <- function(wavelengths) {
    ncharacters <- nchar(wavelengths)
    corrected_values <- numeric(length = length(wavelengths))
    for (w in seq_along(wavelengths)){
        decimal_digits <- ncharacters[w] - 3
        int_conversion <- NULL
        result <- tryCatch({
            int_conversion <- as.numeric(wavelengths[w])
        }, 
        warning = function(w){
            # should use external bands names instead
            stop("Band Names cannot be converted to numeric.  
            If using estimate_land_cover(), try again with use_external_bands = TRUE")
        })
        
        corrected_value <- int_conversion 
        corrected_values[[w]] <- corrected_value
    }
    return(corrected_values)
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

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
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




#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
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
#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
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