require(tidyverse)
require(compiler)
require(raster)
#require(parallel)
#require(doParallel)
require(hsdar)
require(spectrolab)
require(ranger)
require(stringr)
require(stringi)
require(rjson)
require(rgdal)
require(gdalUtils)
require(snow)
require(doSNOW)
require(stats)
require(rasterVis)


#' Functions returns columns that are bandpasses
#' 
#' @inheritParams None
#' @return a dataframe without non-bandpass columns
#' @param x: a dataframe
#' @seealso remove_band_column
#' @export 
#' @examples Not Yet Implmented
remove_meta_column <- function(x) {
    meta <- c(grep(
        "[0-9][0-9][0-9]",
        colnames(x),
        value = FALSE,
        invert = FALSE))
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
resample_spectral_dataframe <- function(
    df, 
    wavelength=5,
    start = 397.593,
    end = 899.424) {
    #Separate out data columns & convert to spectal object
    df_no_metadata <- remove_meta_column(df)
    speclib_df <- spectrolab::as_spectra(df_no_metadata)

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
    bands <- remove_meta_column(df) %>% colnames()
    bands <- gsub(".nm", "", bands)
    bands <- gsub("_5", "", bands)
    bands <- gsub("_", "", bands)
    bands <- gsub("X","", bands) %>% convert_wavelength_strings()
    bands <- bands[!is.na(bands)]#remove NAs
    return(bands)
}


#' Converts Data Frame to a spaectral library
#' 
#' @inheritDotParams 
#' @return A spectral library
#' @param df: A dataframe to convert
#' @export
#'
df_to_speclib <- function(df, type="hsdar") {
    # Convert to a spectral library
    #print(colnames(df))
    df_no_metadata <- remove_meta_column(df)

    
    bands <- extract_bands(df)
    spectral_lib <- NULL
    if (type=="hsdar"){
        spectral_matrix <- as.matrix(df_no_metadata)

        spectral_lib <- hsdar::speclib(spectral_matrix, wavelength = bands)
    } else if (type == "spectrolab") {
        colnames(df_no_metadata) <- bands
        spectral_lib <- spectrolab::as_spectra(df_no_metadata)
    }
    
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
    spec_library <- df_to_speclib(df, type="spectrolab")
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
    return( gsub("^X", "", Vars_names))
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
    var_names <- gsub("MCARI2OSAVI2", "MCARI2/OSAVI2", var_names)
    var_names <- gsub("MCARIOSAVI", "MCARI/OSAVI", var_names)
    var_names <- gsub("TCARIOSAVI", "TCARI/OSAVI", var_names)
    var_names <- gsub("TCARI2OSAVI2", "TCARI2/OSAVI2", var_names)
    var_names <- gsub("PRICI2", "PRI*CI2", var_names)
    var_names <- gsub("SWIRFI", "SWIR FI", var_names)
    var_names <- gsub("SWIRLI", "SWIR LI", var_names)
    var_names <- gsub("SWIRSI", "SWIR SI", var_names)
    var_names <- gsub("SWIRVI", "SWIR VI", var_names)
    var_names <- gsub("GDVI2", "GDVI_2", var_names)
    var_names <- gsub("GDVI3", "GDVI_3", var_names)
    var_names <- gsub("GDVI4", "GDVI_4", var_names)
    var_names <- gsub("GreenNDVI", "Green NDVI", var_names)
    var_names <- gsub("PRInorm", "PRI_norm", var_names)
    var_names <- gsub("REPLE", "REP_LE", var_names)
    var_names <- gsub("REPLi", "REP_Li", var_names)
    var_names <- gsub("SumDr1", "Sum_Dr1", var_names)
    var_names <- gsub("SumDr2", "Sum_Dr2", var_names)



    veg_indices <- intersect(av, var_names)

    return(veg_indices)
}


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

    #print(veg_indices)
    colnames(index_df) <- clean_colnames(target_indices)
    
    return(index_df)
}

attach_veg_indices <- function(df) {

    # saving this for later.
        new_df <- df %>%
        dplyr::select(x,y,all_of(target_model_vars))


    ndvi = hsdar::vegindex(., "ndvi", weighted = FALSE)
    OSAVI = hsdar::vegindex(., "OSAVI", weighted = FALSE)
    SAVI = hsdar::vegindex(., "SAVI", weighted = FALSE)
    MTVI = hsdar::vegindex(., "MTVI", weighted = FALSE)
    NDWI = hsdar::vegindex(., "NDWI", weighted = FALSE)
    PWI = hsdar::vegindex(., "PWI", weighted = FALSE)
    MSI = hsdar::vegindex(., "MSI", weighted = FALSE)
    SRWI = hsdar::vegindex(., "SRWI", weighted = FALSE)
    GMI1 = hsdar::vegindex(., "GMI1", weighted = FALSE)
    GMI2 = hsdar::vegindex(., "GMI2", weighted = FALSE)
    MCARI = hsdar::vegindex(., "MCARI", weighted = FALSE)
    TVI = hsdar::vegindex(., "TVI", weighted = FALSE)
    Vogelmann4 = hsdar::vegindex(., "Vogelmann4", weighted = FALSE)
    Boochs = hsdar::vegindex(., "Boochs", weighted = FALSE)
    Boochs2 = hsdar::vegindex(., "Boochs2", weighted = FALSE)
    CARI = hsdar::vegindex(., "CARI", weighted = FALSE)
    CI = hsdar::vegindex(., "CI", weighted = FALSE)
    Carter = hsdar::vegindex(., "Carter", weighted = FALSE)
    Carter2 = hsdar::vegindex(., "Carter2", weighted = FALSE)
    Carter3 = hsdar::vegindex(., "Carter3", weighted = FALSE)
    Carter4 = hsdar::vegindex(., "Carter4", weighted = FALSE)
    Carter5 = hsdar::vegindex(., "Carter5", weighted = FALSE)
    Carter6 = hsdar::vegindex(., "Carter6", weighted = FALSE)
    Datt = hsdar::vegindex(., "Datt", weighted = FALSE)
    Datt2 = hsdar::vegindex(., "Datt2", weighted = FALSE)
    Datt3 = hsdar::vegindex(., "Datt3", weighted = FALSE)
    Datt4 = hsdar::vegindex(., "Datt4", weighted = FALSE)
    Datt5 = hsdar::vegindex(., "Datt5", weighted = FALSE)
    Datt6 = hsdar::vegindex(., "Datt6", weighted = FALSE)
    Datt7 = hsdar::vegindex(., "Datt7", weighted = FALSE)
    Datt8 = hsdar::vegindex(., "Datt8", weighted = FALSE)
    DD = hsdar::vegindex(., "DD", weighted = FALSE)
    DDn = hsdar::vegindex(., "DDn", weighted = FALSE)
    D1 = hsdar::vegindex(., "D1", weighted = FALSE)
    D2 = hsdar::vegindex(., "D2", weighted = FALSE)
    EVI = hsdar::vegindex(., "EVI", weighted = FALSE)
    EGFR = hsdar::vegindex(., "EGFR", weighted = FALSE)
    EGFN = hsdar::vegindex(., "EGFN", weighted = FALSE)
    GI = hsdar::vegindex(., "GI", weighted = FALSE)
    Gitelson = hsdar::vegindex(., "Gitelson", weighted = FALSE)
    Gitelson2 = hsdar::vegindex(., "Gitelson2", weighted = FALSE)
    Green_NDVI = hsdar::vegindex(., "Green NDVI", weighted = FALSE)
    MCARI_OSAVI = hsdar::vegindex(., "MCARI/OSAVI", weighted = FALSE)
    MCARI2 = hsdar::vegindex(., "MCARI2", weighted = FALSE)
    MCARI2_OSAVI2 = hsdar::vegindex(., "MCARI2/OSAVI2", weighted = FALSE)
    mNDVI = hsdar::vegindex(., "mNDVI", weighted = FALSE)
    mND705 = hsdar::vegindex(., "mND705", weighted = FALSE)
    Maccioni = hsdar::vegindex(., "Maccioni", weighted = FALSE)
    mREIP = hsdar::vegindex(., "mREIP", weighted = FALSE)
    MSAVI = hsdar::vegindex(., "MSAVI", weighted = FALSE)
    mSR = hsdar::vegindex(., "mSR", weighted = FALSE)
    mSR705 = hsdar::vegindex(., "mSR705", weighted = FALSE)
    mSR2 = hsdar::vegindex(., "mSR2", weighted = FALSE)
    MCTI = hsdar::vegindex(., "MCTI", weighted = FALSE)
    NDVI2 = hsdar::vegindex(., "NDVI2", weighted = FALSE)
    NDVI3 = hsdar::vegindex(., "NDVI3", weighted = FALSE)
    NPCI = hsdar::vegindex(., "NPCI", weighted = FALSE)
    OSAVI2 = hsdar::vegindex(., "OSAVI2", weighted = FALSE)
    RDVI = hsdar::vegindex(., "RDVI", weighted = FALSE)
    REP_LE = hsdar::vegindex(., "REP_LE", weighted = FALSE)
    REP_Li = hsdar::vegindex(., "REP_Li", weighted = FALSE)
    SIPI = hsdar::vegindex(., "SIPI", weighted = FALSE)
    SPVI = hsdar::vegindex(., "SPVI", weighted = FALSE)
    SR = hsdar::vegindex(., "SR", weighted = FALSE)
    SR2 = hsdar::vegindex(., "SR2", weighted = FALSE)
    SR3 = hsdar::vegindex(., "SR3", weighted = FALSE)
    SR4 = hsdar::vegindex(., "SR4", weighted = FALSE)
    SR5 = hsdar::vegindex(., "SR5", weighted = FALSE)
    SR6 = hsdar::vegindex(., "SR6", weighted = FALSE)
    SR7 = hsdar::vegindex(., "SR7", weighted = FALSE)
    SR8 = hsdar::vegindex(., "SR8", weighted = FALSE)
    Vogelmann = hsdar::vegindex(., "Vogelmann", weighted = FALSE)
    Vogelmann = hsdar::vegindex(., "Vogelmann2", weighted = FALSE)
    Vogelmann = hsdar::vegindex(., "Vogelmann3", weighted = FALSE)
    NDLI = hsdar::vegindex(., "NDLI", weighted = FALSE)
    SRPI = hsdar::vegindex(., "SRPI", weighted = FALSE)
    Sum_Dr1 = hsdar::vegindex(., "Sum_Dr1", weighted = FALSE)
    Sum_Dr2 = hsdar::vegindex(., "Sum_Dr2", weighted = FALSE)
    TCARI = hsdar::vegindex(., "TCARI", weighted = FALSE)
    TCARI2 = hsdar::vegindex(., "TCARI2", weighted = FALSE)
    TCARI_OSAVI = hsdar::vegindex(., "TCARI/OSAVI", weighted = FALSE)
    TCARI2_OSAVI2 = hsdar::vegindex(., "TCARI2/OSAVI2", weighted = FALSE)
    PSSR = hsdar::vegindex(., "PSSR", weighted = FALSE)
    PSND = hsdar::vegindex(., "PSND", weighted = FALSE)
    CRI1 = hsdar::vegindex(., "CRI1", weighted = FALSE)
    CRI2 = hsdar::vegindex(., "CRI2", weighted = FALSE)
    CRI3 = hsdar::vegindex(., "CRI3", weighted = FALSE)
    CRI4 = hsdar::vegindex(., "CRI4", weighted = FALSE)
    MPRI = hsdar::vegindex(., "MPRI", weighted = FALSE)
    PRI_CI2 = hsdar::vegindex(., "PRI*CI2", weighted = FALSE)
    CI2 = hsdar::vegindex(., "CI2", weighted = FALSE)
    PSRI = hsdar::vegindex(., "PSRI", weighted = FALSE)
    ClAInt = hsdar::vegindex(., "ClAInt", weighted = FALSE)
    TGI = hsdar::vegindex(., "TGI", weighted = FALSE)
    PRI_norm = hsdar::vegindex(., "PRI_norm", weighted = FALSE)
    PARS = hsdar::vegindex(., "PARS", weighted = FALSE)
    DPI = hsdar::vegindex(., "DPI", weighted = FALSE)
    GDVI_2 = hsdar::vegindex(., "GDVI_2", weighted = FALSE)
    GDVI_3 = hsdar::vegindex(., "GDVI_3", weighted = FALSE)
    GDVI_4 = hsdar::vegindex(., "GDVI_4", weighted = FALSE)
    LWVI1 = hsdar::vegindex(., "LWVI1", weighted = FALSE)
    LWVI2 = hsdar::vegindex(., "LWVI2", weighted = FALSE)
    DWSI1 = hsdar::vegindex(., "DWSI1", weighted = FALSE)
    DWSI2 = hsdar::vegindex(., "DWSI2", weighted = FALSE)
    DWSI3 = hsdar::vegindex(., "DWSI3", weighted = FALSE)
    DWSI4 = hsdar::vegindex(., "DWSI4", weighted = FALSE)
    DWSI5 = hsdar::vegindex(., "DWSI5", weighted = FALSE)
    SWIR_FI = hsdar::vegindex(., "SWIR FI", weighted = FALSE)
    SWIR_LI = hsdar::vegindex(., "SWIR LI", weighted = FALSE)
    SWIR_SI = hsdar::vegindex(., "SWIR SI", weighted = FALSE)
    SWIR_VI = hsdar::vegindex(., "SWIR VI", weighted = FALSE)
}


#' Combines derivatives (veg indices) and prediction
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

clean_colnames <- function(x_strs){
    return(
        stringr::str_remove_all(
            x_strs, 
            "[[:punct:]]| "
        )
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
impute_spectra <- function(x, cluster = NULL, method = "missForest", transpose=FALSE) {
    df <- x 
    

    # convert to a data.frame if x is a raster
    if (!is.data.frame(x)) {
        df <- x %>%
        rasterToPoints() %>%
        as.data.frame() 
    }
    # save band names in case we need to transpose and restore column names later
    bands <- colnames(df)


    # transpose if needed
    if(transpose){
        df <- df %>%
        as.matrix() %>% 
        t() %>% 
        as.data.frame()
    }

    print("Imputing...")
    if (method == "missForest") {
        output_data <- missForest::missForest(df, maxiter = 1,)$ximp
    } else if(method == "median"){
        output_data <- useful::simple.impute(df)     
    } else if( method == "mean"){
        output_data <- useful::simple.impute(df, fun = mean)
    }
    
    if(transpose){
        output_data <- output_data %>% 
        as.matrix() %>% 
        t() %>% 
        as.data.frame()
        colnames(output_data) <- bands
    }

    
    if (!is.data.frame(x)) {
        spectral_matrix <- as.matrix(df)
        output_data <- raster::rasterFromXYZ(
            spectral_matrix,
            crs = raster::crs(x))
        raster::extent(output_data) <- raster::extent(x)
    }
    gc()

    return(output_data)
}#end impute_spectra

#' creates tiles from raster brick and returns the locations on disk
#'
#' Long Description here
#'
#' @return 
#' @param raster_obj: a rasterBrick object (RasterLayer and RasterStack should also work but are not tested) 
#' @param num_x: number of tiles in x direction
#' @param num_y: number of tiles in y direction
#' @param save_path: folder to save the tiles
#' @param cluster: parallel compute cluster (e.g. from parallel::beginCluster()), or NULL
#' @param verbose: (boolean, default: false) determines whether details are printed during evaluation 
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
make_tiles <- function(
    raster_obj,
    num_x = 10,
    num_y = 10,
    save_path = "./", 
    cluster = NULL,
    verbose = FALSE) {

    if(verbose){
        print("Creating tile templates")
    }
    tiles <- SpaDES.tools::splitRaster(
        raster_obj[[1]], 
        nx=num_x,
        ny = num_y, 
        path = save_path)

    gc()

    if(verbose){
        print("Templates created, cropping raster")
    }


    get_random_filename <- function() {
        random_tile_id <- stringi::stri_rand_strings(1,16)
        random_filename <- paste0("tile_", random_tile_id, ".envi")
        return(random_filename)
    }


    crop_and_save_raster <- function(tile) {
        tile_filename <- file.path(save_path, get_random_filename())
            raster::crop(
                raster_obj, 
                tile,
                filename=tile_filename
            )
            gc()
            return(tile_filename)
    }

    tile_filenames <- list()

    if(!is.null(cluster)){
        if(verbose){
            print("Processing using supplied cluster")
        }
        tile_filenames <- parallel::parLapply(cluster, tiles, crop_and_save_raster)
    } else {
        if(verbose){
            print("Processing sequentially")
        }
        tile_filenames <- lapply(tiles, crop_and_save_raster)
    }

    if(verbose){
        print("complete")
    }
    gc()

    return(tile_filenames)
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
aggregate_results_df <- function(prediction, x, y) {

    dfx <- data.frame(x)
    colnames(dfx) <- c("x")
    dfxy <- cbind(dfx, y)
    z <- c()

    for (my_prediction in prediction) {
        prediction_df <- as.data.frame(
        raster::levels(my_prediction)[[1]], xy = TRUE)
        num_pixels <- nrow(prediction_df)
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
    return(eval(parse(text = load(filepath))))
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
    cache_filepath = "./",
    output_filepath =  paste("output-",format(Sys.time(), "%a-%b-%d-%H-%M-%S-%Y"), ".envi", sep=""),
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
        num_tiles_x <- calc_num_tiles(input_filepath, max_size = config$max_size)
        num_tiles_y <- calc_num_tiles(input_filepath, max_size = config$max_size)
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

handle_empty_tile <- function(tile_raster, save_path = NULL, target_crs = NULL){
    # convert to a raster
    output_raster <- tile_raster[[1]]

    
    if(!is.null(target_crs)){
        raster::crs(output_raster) <- target_crs
    }

    names(output_raster) <- c("predictions")
    raster::dataType(output_raster) <- "INT2U"
    
    print(raster::NAvalue(output_raster))
    if(!is.null(save_path)){
        raster::writeRaster(output_raster, save_path, datatype = "INT2U")
    }

    
      
    print("Empty Input Raster")
    print(tile_raster)
    print("Output Raster")
    print(output_raster)
    return(output_raster)
}

has_empty_column <- function(df){
    # chack if there are not enough data in any column for missForest
    num_na_per_col <- colSums(is.na(df))
    all_na_column <- (num_na_per_col >= (nrow(df) - 2))

    if(any(all_na_column)){
        return( TRUE )
    }
    return( FALSE )
}


drop_empty_columns <- function(df){
    num_na_per_col <- colSums(is.na(df))
    all_na_column <- (num_na_per_col == (nrow(df)))
    df_no_cols <- df[, !all_na_column]
    return(df_no_cols)
}
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
    return_filename = FALSE,
    save_path = NULL,
    suppress_output = FALSE
    ) {
    raster_obj <- raster::brick(tile_filename)
    input_crs <- raster::crs(raster_obj)
    print(paste0("preprocessing raster at ", tile_filename))
    base_df <- preprocess_raster_to_df(raster_obj, ml_model, band_names=band_names)
    print(input_crs)
    
    if(nrow(base_df) < 2){
        print("The tile has no rows!")
        print(dim(base_df))
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
        print(save_path)
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
    
        try(
            rm(cleaned_df)
        )# sometimes garbage collection gets there first, which is fine
        gc()

        # drop rows that are uniformly zero
      
        resampled_df <- resample_df(imputed_df)
        gc()

        veg_indices <- get_vegetation_indices(resampled_df, ml_model, cluster = cluster)

        print("Resampled Dataframe Dimensions:")
        print(dim(resampled_df))
        print("Index Dataframe Dimensions:")
        print(dim(veg_indices))

        df <- cbind(resampled_df, veg_indices)
        #df <- df %>% dplyr::select(x, y, dplyr::all_of(target_model_cols)) 
        # above line should not be needed, testing then deleting
        rm(veg_indices)
        rm(resampled_df)
        gc()

        imputed_df_full <- impute_spectra(df, method="median")

        
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
            print(save_path)
            return(unlist(save_path))
        }
        return(prediction)
    }
}

#' removes columns outside the min and max indicies
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
remove_noisy_cols <- function(df, min_index = 1, max_index = 274) {
    return(df[min_index:max_index])
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
filter_bands <- function(df) {
    # from original code, but sets the values to NA instead of -999 
    df[-1:-2][df[-1:-2] > 1] <- 1.0#formerly NA
    df[-1:-2][df[-1:-2] < 0 ] <- 0.0#formerly NA
    df[-1:-2][sapply(df[-1:-2], is.nan)] <- NA
    df[-1:-2][sapply(df[-1:-2], is.infinite)] <- NA
    return(df)
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
preprocess_raster_to_df <- function(raster_obj, model, band_names=NULL) {

    #unique_levels <- unique(raster::values(raster_obj))
    #mean_of_levels <- mean(unique_levels, na.rm = TRUE)

    filter_value <- mean(raster::values(raster_obj), na.rm = TRUE)# NA or 0 is bad

    if( filter_value == 0 | is.na(filter_value)){
        return(data.frame())
    }

    saved_names <- names(raster_obj)

    imputed_raster <- raster::approxNA(
        raster_obj,
        rule = 1
    )


    if(!is.null(band_names)){
        #try assigning the names to the bands (ignoring extras)
        try({
            names(imputed_raster) <- band_names[1:length(names(imputed_raster))]
        })
    } else {
        names(imputed_raster) <- saved_names
    }

    #print(names(imputed_raster))

    rm(raster_obj)

    df <- raster::rasterToPoints(imputed_raster) %>% as.data.frame()
    if(nrow(df) < 1){
        #return df here as filtering fails later
        return (df)
    }
    print("Converted to Data frame?")
    print(is.data.frame(df))
    df <- remove_noisy_cols(df, max_index = 284) %>% as.data.frame()
    print("Noisy columns removed")
    print(is.data.frame(df))
    df <- filter_bands(df)
    #df <- filter_empty_points(df)
    print("Filtered")
    print(is.data.frame(df))
    gc()
    return(df)
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
datacube_to_csv <- function(raster_path, save_path) {
    datacube <- raster::brick(raster_path)
    df <- raster::rasterToPoints(datacube)
    write.csv(df, save_path)
}


# converts raster to CSV file on disk
preprocess_raster_to_csv <- function(
    raster_filepath,
    model,
    output_filepath = "./converted_data.csv") {

        raster_brick <- raster::brick(raster_filepath)
        df <- preprocess_raster_to_df(raster_brick)
        rm(raster_brick)
        gc()
    write.csv(
        df,
        output_filepath
    )
}

preprocess_raster_to_parquet <- function(
    raster_filepath,
    model_obj,
    output_filepath = "./converted_data.parquet") {
        raster_brick <- raster::brick(raster_filepath)
        df <- preprocess_raster_to_df(raster_brick)
        rm(raster_brick)
        gc()
        arrow::write_parquet(df, output_filepath)
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
    print(wavelengths)
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

#' a quick function based on the original code
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
clean_names <- function(variables){
    return(
        stringr::str_remove_all(
            variables,
            "[[:punct:]]| "
            )
        )
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
apply_model <- function(df, model, threads = 1, clean_names = TRUE){

    prediction <- tryCatch(
        {
            predict(
                model,
                data = df,
                type='response',
                num.threads = threads
            )$prediction %>% as.data.frame() #(Ranger model)

    }, 
    error = function(cond){
        message("Error applying model - likely an empty file:")
        message(cond)
        # return df of NAs
        y <- data.frame()
        y$prediction <- df[, 5]
        y

    })

    #print(prediction$prediction)
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
convert_fg2_string <- function(df) {
    df_convert_results <- df %>% dplyr::mutate(z = case_when(
        z == "Litter" ~ 0L,
        z == "Mineral" ~ 1L,
        z == "FernAlly" ~ 2L,
        z == "ForbFlower" ~ 3L,
        z == "GraminoidGrass" ~ 4L,
        z == "GraminoidSedge" ~ 5L,
        z == "DarkTerrestrialMacrolichen" ~ 6L,
        z == "LightTerrestrialCrustose" ~ 7L,
        z == "LightTerrestrialMacrolichen" ~ 8L,
        z == "YellowTerrestrialCrustose" ~ 9L,
        z == "YellowTerrestrialMacrolichen" ~ 10L,
        z == "MossAcrocarp" ~ 11L,
        z == "MossPleurocarp" ~ 12L,
        z == "MossSphagnum" ~ 13L,
        z == "ShrubAlder" ~ 14L,
        z == "ShrubBetula" ~ 15L,
        z == "ShrubDecidOther" ~ 16L,
        z == "ShrubSalix" ~ 17L,
        z == "ShrubEvergreenNeedle" ~ 18L,
        z == "ShrubEvergreenBroadleaf" ~ 19L,
        z == "TreeBetula" ~ 20L,
        z == "TreeBroadleafOther" ~ 21L,
        z == "TreePopulus" ~ 22L,
        z == "TreeConiferOther" ~ 23L,
        z == "TreeSpruce" ~ 24L,
        z == "Unknown" ~ 25L,
    ), .keep = "unused") %>%
    dplyr::select(x,y,z)
return(df_convert_results)
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
assemble_tiles_from_disk <- function(tiles, output_path){
    tiles = grep("Pred", tiles, value = TRUE)
    chunks<-lapply(
        tiles,
        function(x) {
            raster::raster(x)
        })
    pred_merged<-Reduce(raster::merge, chunks)
    #writeRaster(pred_merged, filename = "Output/prediction/")
    return(pred_merged)
}

merge_tiles <- function(input_files, output_path = NULL, target_layer = 1) {

    master_raster <- raster::raster(input_files[[1]])
    raster::dataType(master_raster) <- "INT2U"
    for (input_file in tail(input_files, -1)) {
        new_raster <- raster::raster(input_file)
        raster::dataType(new_raster) <- "INT2U"
        # above is robust against multi-layer images
        master_raster <- safe_merge(
            master_raster,
            new_raster
        )
    }
    if(!is.null(output_path)) {
        raster::writeRaster(master_raster, output_path, datatype='INT2U', overwrite = TRUE)
    }

    raster::dataType(master_raster) <- "INT2U"
    return(master_raster)
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
.convert_tile_filename <- function(tile_path) {
    return( 
        new_filename <- gsub(
            "tile_",
            "prediction_", 
            c(tile_path),
            fixed = TRUE
            )[[1]]
    )
}

#' aconverts functional group 2 codes from integers to Strings
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame.  Assumes that the target data is in column 'z'
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_fg2_int <- function(df) {
    converted_df <- df %>% dplyr::mutate(
        z = dplyr::case_when(
            z == 0L ~ "Litter",
            z == 1L ~ "Mineral",
            z == 2L ~ "FernAlly",
            z == 3L ~ "ForbFlower",
            z == 4L ~ "GraminoidGrass",
            z == 5L ~ "GraminoidSedge",
            z == 6L ~ "DarkTerrestrialMacrolichen",
            z == 7L ~ "LightTerrestrialCrustose",
            z == 8L ~ "LightTerrestrialMacrolichen",
            z == 9L ~ "YellowTerrestrialCrustose",
            z == 10L ~ "YellowTerrestrialMacrolichen",
            z == 11L ~ "MossAcrocarp",
            z == 12L ~ "MossPleurocarp",
            z == 13L ~ "MossSphagnum",
            z == 14L ~ "ShrubAlder",
            z == 15L ~ "ShrubBetula",
            z == 16L ~ "ShrubDecidOther",
            z == 17L ~ "ShrubSalix",
            z == 18L ~ "ShrubEvergreenNeedle",
            z == 19L ~ "ShrubEvergreenBroadleaf",
            z == 20L ~ "TreeBetula",
            z == 21L ~ "TreeBroadleafOther",
            z == 22L ~ "TreePopulus",
            z == 23L ~ "TreeConiferOther",
            z == 24L ~ "TreeSpruce",
            z == 25L ~ "Unknown",

        ), .keep = "unused"
    ) %>% 
    dplyr::select(x,y,z)

    return(converted_df)
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
convert_fg1_string <- function(df) {
    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == "Abiotic" ~ 0L,
            z == "Forb" ~ 1L,
            z == "Graminoid" ~ 2L,
            z == "Lichen" ~ 3L,
            z == "Moss" ~ 4L,
            z == "ShrubDecid" ~ 5L,
            z == "ShrubEvergreen" ~ 6L,
            z == "TreeBroadleaf" ~ 7L,
            z == "TreeConifer" ~ 8L,
            z == "Unknown" ~ 9L
    ), .keep = "unused") %>%
    dplyr::select(x,y,z)

    return(converted_df)
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
convert_fg1_int <- function(df) {
    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == 0 ~ "Abiotic",
            z == 1 ~ "Forb",
            z == 2 ~ "Graminoid",
            z == 3 ~ "Lichen",
            z == 4 ~ "Moss",
            z == 5 ~ "ShrubDecid",
            z == 6 ~ "ShrubEvergreen",
            z == 7 ~ "TreeBroadleaf",
            z == 8 ~ "TreeConifer",
            z == 9 ~ "Unknown",
    ), .keep = "unused") %>% 
    dplyr::select(x,y,z)
}

#' converts the species names on the 
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_species_string <- function(df){

    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == "Dead Salix" ~ 0L,
            z == "Pices (bark)" ~ 1L,
            z == "Bare Rock" ~ 2L,
            z == "Bare Soil" ~ 3L,
            z == "Quartz" ~ 4L,
            z == "Equisetum arvense" ~ 5L,
            z == "Equisetum sylvaticum" ~ 6L,
            z == "Arenaria pseudofrigida" ~ 7L,
            z == "Heracleum lanatum" ~ 8L,
            z == "Hieracium sp." ~ 9L,
            z == "Iris sp." ~ 10L,
            z == "Lupinus sp." ~ 11L,
            z == "Pedicularis racemosa" ~ 12L,
            z == "Pedicularis sudetica" ~ 13L,
            z == "Petasites frigida" ~ 14L,
            z == "Pestasites frigidus" ~ 15L,
            z == "Saxifraga punctata" ~ 16L,
            z == "Toefeldia sp." ~ 17L,
            z == "Arctagrostis latifolia" ~ 18L,
            z == "Arctophila fulva" ~ 19L,
            z == "Calamogrostis sp." ~ 20L,
            z == "Dupontia fisheri" ~ 21L,
            z == "Carex sp." ~ 22L,
            z == "Carex aquatilis" ~ 23L,
            z == "Eriophorum vaginatum" ~ 24L,
            z == "Eriophorum angustifolium" ~ 25L,
            z == "Bryoria sp." ~ 26L,
            z == "Cetraria islandica" ~ 27L,
            z == "Cetraria laevigata" ~ 28L,
            z == "Masonhalea richardsonii" ~ 29L,
            z == "Melanelia sp." ~ 30L,
            z == "Peltigera apthosa" ~ 31L,
            z == "Peltigers leucophlebia" ~ 32L,
            z == "Peltigera malacea" ~ 33L,
            z == "Peltigera scabrata" ~ 34L,
            z == "Porpidia sp." ~ 35L,
            z == "Rhizocarpon sp." ~ 36L,
            z == "Umbilicaria arctica" ~ 37L,
            z == "Umbilicaria hyperborea" ~ 38L,
            z == "Icmadophila ericetorum" ~ 39L,
            z == "Pilophorus acicularis" ~ 40L,
            z == "Stereocaulon sp." ~ 41L,
            z == "Trapelopsis granulosa" ~ 42L,
            z == "Alectoria ochroleuca" ~ 43L,
            z == "Arctocetraria centrifuga" ~ 44L,
            z == "Asahinea chrysantha" ~ 45L,
            z == "Cladonia cornuta" ~ 46L,
            z == "Cladonia gracilis" ~ 47L,
            z == "Cladonia rangiferina" ~ 48L,
            z == "Cladonia stygia" ~ 49L,
            z == "Hypogymnia austerodes" ~ 50L,
            z == "Parmelia omphalodes" ~ 51L,
            z == "Parmelis sulcata" ~ 52L,
            z == "Unknown" ~ 53L,
            z == "Rhizocarpon geographicum" ~ 54L,
            z == "Cladonia amaurocraea" ~ 55L,
            z == "Cladonia mitis" ~ 56L,
            z == "Cladonia steallaris" ~ 57L,
            z == "Cladonia sulphurina" ~ 58L,
            z == "Cladonia uncialis" ~ 59L,
            z == "Dactylina arctica" ~ 60L,
            z == "Evernia mesomorpha" ~ 61L,
            z == "Flavocetraria cucculata" ~ 62L,
            z == "Flavocetraria nivalis" ~ 63L,
            z == "Nephroma arcticum" ~ 64L,
            z == "Parmeliopsis ambigua" ~ 65L,
            z == "Usnea lapponica" ~ 66L,
            z == "Usnea scabrata" ~ 67L,
            z == "Vulpicida pinastri" ~ 68L,
            z == "Aulacomnium palustre" ~ 69L,
            z == "Aulacomnium turgidum" ~ 70L,
            z == "Ceratadon purpureus" ~ 71L,
            z == "Dicranum sp." ~ 72L,
            z == "Plagiomnium sp." ~ 73L,
            z == "Polytrichum juniperinum" ~ 74L,
            z == "Polytrichum strictum" ~ 75L,
            z == "Polytrichum sp." ~ 76L,
            z == "Racomitrium lanoiginosum" ~ 77L,
            z == "Hylocomium splendens" ~ 78L,
            z == "Pleurozium schreberi" ~ 79L,
            z == "Rhytidum rugosum" ~ 80L,
            z == "Tomenthypnum nitens" ~ 81L,
            z == "Sphagnum sp." ~ 82L,
            z == "Sphagnum fuscum" ~ 83L,
            z == "Alnus sp." ~ 84L,
            z == "Betula nana" ~ 85L,
            z == "Arctostaphyllos" ~ 86L,
            z == "Rhus typhina" ~ 87L,
            z == "Rosa acicularis" ~ 88L,
            z == "Rubus sp." ~ 89L,
            z == "Vaccinium uliginosum" ~ 90L,
            z == "Salix alaxensis" ~ 91L,
            z == "Salix arbusculoides" ~ 92L,
            z == "Salix glauca" ~ 93L,
            z == "Salix lanata" ~ 94L,
            z == "Salix ovalifolia" ~ 95L,
            z == "Salix pulchra" ~ 96L,
            z == "Salix richardsonii" ~ 97L,
            z == "Salix (wooly)" ~ 98L,
            z == "Salix phlebophylla" ~ 99L,
            z == "Cassiope tetragona" ~ 100L,
            z == "Dryas sp." ~ 101L,
            z == "Empetrum nigrum" ~ 102L,
            z == "Ledum decumbens" ~ 103L,
            z == "Loisleuria procumbens" ~ 104L,
            z == "Vaccinium vitis-idea" ~ 105L,
            z == "Betula alleghaniensis" ~ 106L,
            z == "Betula neoalaskana" ~ 107L,
            z == "Betula papyrifera" ~ 108L,
            z == "Betula populifolia" ~ 109L,
            z == "Acer rubrum" ~ 110L,
            z == "Acer pensylvanicum" ~ 111L,
            z == "Fagus grandifolia" ~ 112L,
            z == "Fraxinus americana" ~ 113L,
            z == "Prunus pensylvanica" ~ 114L,
            z == "Quercus Rubra" ~ 115L,
            z == "Populus balsamifera" ~ 116L,
            z == "Populus grandidentata" ~ 117L,
            z == "Abies balsamea" ~ 118L,
            z == "Larix larcina" ~ 119L,
            z == "Pinus strobus" ~ 120L,
            z == "Thuja occidentalis" ~ 121L,
            z == "Tsuga canadensis" ~ 122L,
            z == "Picea mariana" ~ 123L,
            z == "Picea rubens" ~ 124L,

        ), .keep = "unused"
    ) %>% dplyr::select(x,y,z)
    return(converted_df)
}

#' converts the species integer codes to the species name (strings)
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_species_int <- function(df){

    
    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == 0 ~ "Dead Salix",
            z == 1 ~ "Pices (bark)",
            z == 2 ~ "Bare Rock",
            z == 3 ~ "Bare Soil",
            z == 4 ~ "Quartz",
            z == 5 ~ "Equisetum arvense",
            z == 6 ~ "Equisetum sylvaticum",
            z == 7 ~ "Arenaria pseudofrigida",
            z == 8 ~ "Heracleum lanatum",
            z == 9 ~ "Hieracium sp.",
            z == 10 ~ "Iris sp.",
            z == 11 ~ "Lupinus sp.",
            z == 12 ~ "Pedicularis racemosa",
            z == 13 ~ "Pedicularis sudetica",
            z == 14 ~ "Petasites frigida",
            z == 15 ~ "Pestasites frigidus",
            z == 16 ~ "Saxifraga punctata",
            z == 17 ~ "Toefeldia sp.",
            z == 18 ~ "Arctagrostis latifolia",
            z == 19 ~ "Arctophila fulva",
            z == 20 ~ "Calamogrostis sp.",
            z == 21 ~ "Dupontia fisheri",
            z == 22 ~ "Carex sp.",
            z == 23 ~ "Carex aquatilis",
            z == 24 ~ "Eriophorum vaginatum",
            z == 25 ~ "Eriophorum angustifolium",
            z == 26 ~ "Bryoria sp.",
            z == 27 ~ "Cetraria islandica",
            z == 28 ~ "Cetraria laevigata",
            z == 29 ~ "Masonhalea richardsonii",
            z == 30 ~ "Melanelia sp.",
            z == 31 ~ "Peltigera apthosa",
            z == 32 ~ "Peltigers leucophlebia",
            z == 33 ~ "Peltigera malacea",
            z == 34 ~ "Peltigera scabrata",
            z == 35 ~ "Porpidia sp.",
            z == 36 ~ "Rhizocarpon sp.",
            z == 37 ~ "Umbilicaria arctica",
            z == 38 ~ "Umbilicaria hyperborea",
            z == 39 ~ "Icmadophila ericetorum",
            z == 40 ~ "Pilophorus acicularis",
            z == 41 ~ "Stereocaulon sp.",
            z == 42 ~ "Trapelopsis granulosa",
            z == 43 ~ "Alectoria ochroleuca",
            z == 44 ~ "Arctocetraria centrifuga",
            z == 45 ~ "Asahinea chrysantha",
            z == 46 ~ "Cladonia cornuta",
            z == 47 ~ "Cladonia gracilis",
            z == 48 ~ "Cladonia rangiferina",
            z == 49 ~ "Cladonia stygia",
            z == 50 ~ "Hypogymnia austerodes",
            z == 51 ~ "Parmelia omphalodes",
            z == 52 ~ "Parmelis sulcata",
            z == 53 ~ "Unknown",
            z == 54 ~ "Rhizocarpon geographicum",
            z == 55 ~ "Cladonia amaurocraea",
            z == 56 ~ "Cladonia mitis",
            z == 57 ~ "Cladonia steallaris",
            z == 58 ~ "Cladonia sulphurina",
            z == 59 ~ "Cladonia uncialis",
            z == 60 ~ "Dactylina arctica",
            z == 61 ~ "Evernia mesomorpha",
            z == 62 ~ "Flavocetraria cucculata",
            z == 63 ~ "Flavocetraria nivalis",
            z == 64 ~ "Nephroma arcticum",
            z == 65 ~ "Parmeliopsis ambigua",
            z == 66 ~ "Usnea lapponica",
            z == 67 ~ "Usnea scabrata",
            z == 68 ~ "Vulpicida pinastri",
            z == 69 ~ "Aulacomnium palustre",
            z == 70 ~ "Aulacomnium turgidum",
            z == 71 ~ "Ceratadon purpureus",
            z == 72 ~ "Dicranum sp.",
            z == 73 ~ "Plagiomnium sp.",
            z == 74 ~ "Polytrichum juniperinum",
            z == 75 ~ "Polytrichum strictum",
            z == 76 ~ "Polytrichum sp.",
            z == 77 ~ "Racomitrium lanoiginosum",
            z == 78 ~ "Hylocomium splendens",
            z == 79 ~ "Pleurozium schreberi",
            z == 80 ~ "Rhytidum rugosum",
            z == 81 ~ "Tomenthypnum nitens",
            z == 82 ~ "Sphagnum sp.",
            z == 83 ~ "Sphagnum fuscum",
            z == 84 ~ "Alnus sp.",
            z == 85 ~ "Betula nana",
            z == 86 ~ "Arctostaphyllos",
            z == 87 ~ "Rhus typhina",
            z == 88 ~ "Rosa acicularis",
            z == 89 ~ "Rubus sp.",
            z == 90 ~ "Vaccinium uliginosum",
            z == 91 ~ "Salix alaxensis",
            z == 92 ~ "Salix arbusculoides",
            z == 93 ~ "Salix glauca",
            z == 94 ~ "Salix lanata",
            z == 95 ~ "Salix ovalifolia",
            z == 96 ~ "Salix pulchra",
            z == 97 ~ "Salix richardsonii",
            z == 98 ~ "Salix (wooly)",
            z == 99 ~ "Salix phlebophylla",
            z == 100 ~ "Cassiope tetragona",
            z == 101 ~ "Dryas sp.",
            z == 102 ~ "Empetrum nigrum",
            z == 103 ~ "Ledum decumbens",
            z == 104 ~ "Loisleuria procumbens",
            z == 105 ~ "Vaccinium vitis-idea",
            z == 106 ~ "Betula alleghaniensis",
            z == 107 ~ "Betula neoalaskana",
            z == 108 ~ "Betula papyrifera",
            z == 109 ~ "Betula populifolia",
            z == 110 ~ "Acer rubrum",
            z == 111 ~ "Acer pensylvanicum",
            z == 112 ~ "Fagus grandifolia",
            z == 113 ~ "Fraxinus americana",
            z == 114 ~ "Prunus pensylvanica",
            z == 115 ~ "Quercus Rubra",
            z == 116 ~ "Populus balsamifera",
            z == 117 ~ "Populus grandidentata",
            z == 118 ~ "Abies balsamea",
            z == 119 ~ "Larix larcina",
            z == 120 ~ "Pinus strobus",
            z == 121 ~ "Thuja occidentalis",
            z == 122 ~ "Tsuga canadensis",
            z == 123 ~ "Picea mariana",
            z == 124 ~ "Picea rubens",
        ), .keep = "unused") %>%
        dplyr::select(x,y,z)
    return(converted_df)

}

convert_genus_int <- function(df) {
        converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == 0 ~ "LeafLitter",
            z == 1 ~ "Wood",
            z == 2 ~ "Rock",
            z == 3 ~ "Soil",
            z == 4 ~ "Equisetum",
            z == 5 ~ "Arenia",
            z == 6 ~ "Heracleum",
            z == 7 ~ "Hieracium",
            z == 8 ~ "Iris",
            z == 9 ~ "Lupinus",
            z == 10 ~ "Pedicularis",
            z == 11 ~ "Petasites",
            z == 12 ~ "Saxifraga",
            z == 13 ~ "Toefeldia",
            z == 14 ~ "Arctagrostis",
            z == 15 ~ "Arctophila",
            z == 16 ~ "Calamagrostis",
            z == 17 ~ "Dupontia",
            z == 18 ~ "Carex",
            z == 19 ~ "Eriophorum",
            z == 20 ~ "Bryoria",
            z == 21 ~ "Cetraria",
            z == 22 ~ "Masonhalea",
            z == 23 ~ "Melanelia",
            z == 24 ~ "Peltigera",
            z == 25 ~ "Porpidia",
            z == 26 ~ "Rhizocarpon",
            z == 27 ~ "Umbilicaria",
            z == 28 ~ "Icmadophila",
            z == 29 ~ "Pilophorus",
            z == 30 ~ "Stereocaulon",
            z == 31 ~ "Trapeliopsis",
            z == 32 ~ "Alectoria",
            z == 33 ~ "Arctocetraria",
            z == 34 ~ "Asahinea",
            z == 35 ~ "Cladonia",
            z == 36 ~ "Hypogymnia",
            z == 37 ~ "Parmelia",
            z == 38 ~ "Unknown",
            z == 39 ~ "Dactylina",
            z == 40 ~ "Evernia",
            z == 41 ~ "Flavocetraria",
            z == 42 ~ "Nephroma",
            z == 43 ~ "Parmeliopsis",
            z == 44 ~ "Usnea",
            z == 45 ~ "Vulpicida",
            z == 46 ~ "Aulacomnium",
            z == 47 ~ "Ceratadon",
            z == 48 ~ "Dicranum",
            z == 49 ~ "Plagiomnium",
            z == 50 ~ "Polytrichum",
            z == 51 ~ "Racomitrium",
            z == 52 ~ "Hylocomnium",
            z == 53 ~ "Pleurozium",
            z == 54 ~ "Rhytidium",
            z == 55 ~ "Tomenthypnum",
            z == 56 ~ "Sphagnum",
            z == 57 ~ "Alnus",
            z == 58 ~ "Betula",
            z == 59 ~ "Arctostaphyllos",
            z == 60 ~ "Rhus",
            z == 61 ~ "Rosa",
            z == 62 ~ "Rubus",
            z == 63 ~ "Vaccinium",
            z == 64 ~ "Salix",
            z == 65 ~ "Cassiope",
            z == 66 ~ "Dryas",
            z == 67 ~ "Empetrum",
            z == 68 ~ "Ledum",
            z == 69 ~ "Loisleuria",
            z == 70 ~ "Acer",
            z == 71 ~ "Fagus",
            z == 72 ~ "Prunus",
            z == 73 ~ "Quercus",
            z == 74 ~ "Populus",
            z == 75 ~ "Abies",
            z == 76 ~ "Larix",
            z == 77 ~ "Pinus",
            z == 78 ~ "Thuja",
            z == 79 ~ "Tsuga",
            z == 80 ~ "Picea",
            z == 81 ~ "nan"
        ), .keep = "unused"
    ) %>% dplyr::select(x,y,z)
    return(converted_df)
}

convert_genus_string <- function(df) {
        converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == "LeafLitter" ~ 0L,
            z == "Wood" ~ 1L,
            z == "Rock" ~ 2L,
            z == "Soil" ~ 3L,
            z == "Equisetum" ~ 4L,
            z == "Arenia" ~ 5L,
            z == "Heracleum" ~ 6L,
            z == "Hieracium" ~ 7L,
            z == "Iris" ~ 8L,
            z == "Lupinus" ~ 9L,
            z == "Pedicularis" ~ 10L,
            z == "Petasites" ~ 11L,
            z == "Saxifraga" ~ 12L,
            z == "Toefeldia" ~ 13L,
            z == "Arctagrostis" ~ 14L,
            z == "Arctophila" ~ 15L,
            z == "Calamagrostis" ~ 16L,
            z == "Dupontia" ~ 17L,
            z == "Carex" ~ 18L,
            z == "Eriophorum" ~ 19L,
            z == "Bryoria" ~ 20L,
            z == "Cetraria" ~ 21L,
            z == "Masonhalea" ~ 22L,
            z == "Melanelia" ~ 23L,
            z == "Peltigera" ~ 24L,
            z == "Porpidia" ~ 25L,
            z == "Rhizocarpon" ~ 26L,
            z == "Umbilicaria" ~ 27L,
            z == "Icmadophila" ~ 28L,
            z == "Pilophorus" ~ 29L,
            z == "Stereocaulon" ~ 30L,
            z == "Trapeliopsis" ~ 31L,
            z == "Alectoria" ~ 32L,
            z == "Arctocetraria" ~ 33L,
            z == "Asahinea" ~ 34L,
            z == "Cladonia" ~ 35L,
            z == "Hypogymnia" ~ 36L,
            z == "Parmelia" ~ 37L,
            z == "Unknown" ~ 38L,
            z == "Dactylina" ~ 39L,
            z == "Evernia" ~ 40L,
            z == "Flavocetraria" ~ 41L,
            z == "Nephroma" ~ 42L,
            z == "Parmeliopsis" ~ 43L,
            z == "Usnea" ~ 44L,
            z == "Vulpicida" ~ 45L,
            z == "Aulacomnium" ~ 46L,
            z == "Ceratadon" ~ 47L,
            z == "Dicranum" ~ 48L,
            z == "Plagiomnium" ~ 49L,
            z == "Polytrichum" ~ 50L,
            z == "Racomitrium" ~ 51L,
            z == "Hylocomnium" ~ 52L,
            z == "Pleurozium" ~ 53L,
            z == "Rhytidium" ~ 54L,
            z == "Tomenthypnum" ~ 55L,
            z == "Sphagnum" ~ 56L,
            z == "Alnus" ~ 57L,
            z == "Betula" ~ 58L,
            z == "Arctostaphyllos" ~ 59L,
            z == "Rhus" ~ 60L,
            z == "Rosa" ~ 61L,
            z == "Rubus" ~ 62L,
            z == "Vaccinium" ~ 63L,
            z == "Salix" ~ 64L,
            z == "Cassiope" ~ 65L,
            z == "Dryas" ~ 66L,
            z == "Empetrum" ~ 67L,
            z == "Ledum" ~ 68L,
            z == "Loisleuria" ~ 69L,
            z == "Acer" ~ 70L,
            z == "Fagus" ~ 71L,
            z == "Prunus" ~ 72L,
            z == "Quercus" ~ 73L,
            z == "Populus" ~ 74L,
            z == "Abies" ~ 75L,
            z == "Larix" ~ 76L,
            z == "Pinus" ~ 77L,
            z == "Thuja" ~ 78L,
            z == "Tsuga" ~ 79L,
            z == "Picea" ~ 80L,
            z == "nan" ~ 81L,

                        ), .keep = "unused"
    ) %>% dplyr::select(x,y,z)
    return(converted_df)
}


convert_pft_codes <- function(df, aggregation_level, to="int"){

    # list of functions for converting the data from strings to integers
    to_int = list(
        Functional_group1 = convert_fg1_string,
        Functional_group2 = convert_fg2_string,
        Genus = convert_genus_string,
        Species = convert_species_string
        )

    # list of functions for converting from integers to strings
    to_string = list(
        Functional_group1 = convert_fg1_int,
        Functional_group2 = convert_fg2_int,
        Genus = convert_genus_int,
        Species = convert_species_int
    )

    # split by conversion type, then use the appropriate function based on the aggregation level
    if( to == "int" | to =="Int" | to == "integer"){
        result <- to_int[[aggregation_level]](df)
        #result$z <- as.integer(result$z)
        return( result )
    } else if( to == "string" | to == "String") {
        return( to_string[[aggregation_level]](df) )
    } else {
        # catch invalid 'to' parameter
        stop(paste0("Cannot convert to type ", to))
    }
}

#'
#' @export


update_filename <- function(prefix){
    if(!file.exists(prefix)){return(prefix)}
    i=1
    repeat {
       f = paste(prefix,i,sep=".")
       if(!file.exists(f)){
           return(f)
           }
       i=i+1
     }
}


#' aconverts the output to the correct data type (base::data.frame or raster::rasterLayer) and save it to disk
#'
#' In addition to converting the file, it saves the file to disk.  
#' In order to avoid issues with overwriting files, the function will 
#' adjoin 16 random characters to the filename to avoid collision.
#'
#' @return 
#' @param df: A data.frame
#' @param save_path: the file path for saving the converted file.  
#' If NULL (default), no file is saved.  
#' @param return_raster: If TRUE, the function converts 'df' to a
#'  raster before saving and returning the converted data. 
#' If FALSE, the data.frame is saved to disk, but no coversion is made.
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_and_save_output <- function(df, aggregation_level, save_path = NULL, return_raster = TRUE, target_crs = NULL){
    prediction <- convert_pft_codes(df, aggregation_level = aggregation_level, to = "int")

    print(head(prediction))

    # convert the precition values to integers
    prediction$z <- prediction$z %>% round() %>% as.integer()

    print(head(prediction))

    print(paste0("Attempting to save to ", save_path))
    if(return_raster){
        prediction <- raster::rasterFromXYZ(prediction, digits=4)
        print("Converted to Raster")

        raster::dataType(prediction) <- "INT2U" # set to int datatype (unsigned int // 2 bytes)
        levels(prediction) <- get_attribute_table(aggregation_level)
        if(!is.null(target_crs)){
            raster::crs(prediction) <- target_crs
        }
        if(!is.null(save_path)){
                raster::writeRaster(prediction, filename = save_path, datatype='INT2U', overwrite = TRUE)
            } 
        return(prediction)
    } else {
        if(!is.null(save_path)){
            if(file.exists(save_path)){
                write.csv(prediction, save_path, overwrite=TRUE)
            }
        }
            return(df)
    }
}

#' Performs post-processing for the process_tile function
#'
#' Renames columns and adds the spatial information that is lost at model inference time.
#'
#' @return 
#' @param prediction_df: A data.frame of prediction from the model.  It should have a single column of data.
#' @param base_df: the base data.frame for creating the prediction; 
#' it should have columns x and y specifying the spatial coordinates for that row.
#' @return a data.frame with three columns: 'x', 'y', and 'z'.  'x' and 'y' are the spatial location of the 
#' prediction in the original CRS, and the 'z' column is prediction. 
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
postprocess_prediction <- function(prediction_df, base_df){
    colnames(prediction_df) <- c("z")
    prediction_df$x <- base_df$x
    prediction_df$y <- base_df$y
    gc()

    prediction_df <- prediction_df %>% dplyr::select(x, y, z)

    return(prediction_df)
}


#' loads the attribute table for the output (categorical) raster.  
#'
#' Gets the raster attribute table from the 
#'
#' @return 
#' @param aggregation: an integer that specifies the aggregation level for the output raster.  Should be 1 (functional group 1), 2 (functional group 2), 3 (genus), or 4 (species).
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
get_attribute_table <- function(aggregation) {
    attribute_table <- data.frame()
    if(aggregation == 1){
        attribute_table <- read.csv("fg1RAT.csv")
    }
    if(aggregation == 2){
        attribute_table <- read.csv("fg2RAT.csv")
        
    }
    if(aggregation == 3) {
        attribute_table <- read.csv("genusRAT.csv")

    }
    if(aggregation == 4){
        attribute_table <- read.csv("speciesRAT.csv")
    }
}

#' visualizes the output of the pipeline based on a specified colormap.
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
visualize_prediction <- function(filepath, colormap){

}

#' removes empty rows from the data.frame
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
filter_empty_points <- function(df){
    print("Data summary before filtering")
    print(dim(df))
    drop_cols <- c("x","y")
    columns_to_check <- setdiff(colnames(df), drop_cols)

    df_no_empty_rows <- df %>% 
        naniar::replace_with_na_all(condition = ~.x == 0) %>%
        dplyr::filter_at(
            .vars = vars(one_of(columns_to_check)),
            ~!is.na(.)
        ) 
    print("data summary after filering")
    print(dim(df_no_empty_rows))
    return(df_no_empty_rows)
}


drop_zero_rows <- function(df) {
    # sort the columsn to make sure x and y are first before calculating rowsums
    df_rowsums <- df %>% dplyr::relocate(y) %>% dplyr::relocate(x)
    num_cols = ncol(df)
    # return only the rows with rowsums over zero
    return( df_rowsums[rowSums(df_rowsums[,3:num_cols], na.rm = TRUE) > 0.0001, ])
}

#' removes empty rows from the data.frame
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
merge_tiles_gdal <- function(
    target_files,
    output_filename,
    cache_vrt = "./temp_cache.vrt",
    return_raster
    ){
        vrt <- gdalUtils::gdalbuildvrt(target_files, cache_vrt, overwrite=TRUE)
        output <- gdalUtils::gdal_translate(
            cache_vrt,
            output_filename,
            ot = "UInt16",
            output_raster = return_raster)
    return(output)
}


#' removes empty rows from the data.frame
#'
#' Long Description here
#'
#' @return 
#' @param file_path: a string
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
calc_num_tiles <- function(file_path, max_size = 1024){
    file_size <- file.info(file_path)$size / (1024 * 1024)
    tile_size <- file_size / max_size
    num_xy <- ceiling(sqrt(tile_size))
    return(num_xy)
}

# talk to NASA spectral imaging working group r/e gaps

visualize_prediction <- function(filepath, key_file, column){
    require(leaflet)
    color_map <- create_color_map(key_file, column)
    labels <- create_labels(key_file, column)
    layer <- raster::raster(filepath)
    epsg_code <- 3857
    layer_projected <- project_to_epsg(layer, epsg_code, categorical_raster = TRUE)
    map <- leaflet::leaflet() %>%
        leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
             options = providerTileOptions(minZoom = 8, maxZoom = 100)) %>%
        leaflet::addRasterImage(layer, layerId = "layer", colors = color_map) %>%
        leaflet::addLegend("bottomleft", colors = color_map(labels), labels = labels, opacity = 1) %>%
        leaflet.opacity::addOpacitySlider(layerId = "layer")
    return(map)
}

create_color_map <- function(filepath, column){
    levels <- unlist(read.csv(filepath, header = TRUE)[column])
    num_levels <- length(unique(levels))
    palette <- leaflet::colorFactor(grDevices::topo.colors(num_levels), sort(levels))
    return(palette)
}

create_labels <- function(filepath, column){
    return( sort(unique(unlist(read.csv(filepath, header = TRUE)[column]))))
}

crs_from_epsg <- function(epsg_code) {
    target_wkt <- sf::st_crs(epsg_code)[[2]]
    target_crs <- sp::CRS(target_wkt)
}


project_to_epsg <- function(raster_obj, epsg_code, categorical_raster = FALSE){
    target_wkt <- sf::st_crs(epsg_code)[[2]]
    target_crs <- sp::CRS(target_wkt)
    if(categorical_raster){
        return(raster::projectRaster(
            raster_obj,
            crs=target_crs,
            method="ngb"
        ))
    }
    return(raster::projectRaster(raster_obj, target_crs))
}

build_adjacency_list <- function(filepath) {
    df <- read.csv(filepath, header = TRUE)
    num_rows <- nrow(df)
    num_levels <- ncol(df)
    adjacency_list <- list()

    for(i in seq.int(num_rows)){
        for( j in seq.int(num_levels)){
            counter <- 1
            key <- df[[i,j]]
            values <- vector(mode = "character", length = num_levels)
            while(counter <= num_levels){
                if(counter < j){
                    values[[counter]] <- NA
                } else {
                    values[[counter]] <- df[[i, counter]]
                }
                counter <- counter + 1
            }
            adjacency_list[[key]] <- values[2:(num_levels)]
        }
    }

    return(adjacency_list)
}

enum_pfts <- function(pft){
    if(pft == 1){
        return("Functional_Group1")
    }
    if(pft == 2){
        return("Functional_Group2")
    }
    if(pft == 3){
        return("Genus")
    } 
    if(pft == 4){
        return("Species")
    } 
    if(pft == "Functional_Group1"){
        return(1)
    } 
    if(pft == "Functional_Group2"){
        return(2)
    }
    if( pft == "Genus"){
        return(3)
    } 
    if( pft == "Species"){
        return(4)
    }
}

# changes aggregation level
change_aggregation <- function(prediction_vec, aggregation_level, aggregation_key){
    updated_predictions <- vector("character", length=length(prediction_vec))
    for(i in seq_along(prediction_vec)){
        aggregation_idx <- 5-aggregation_level
        prediction <- prediction_vec[[i]]
        print(prediction)
        updated_predictions[[i]] <- aggregation_key[[prediction]][[aggregation_idx]]
    }

    return(updated_predictions)
}

get_prediction_distribution <- function(prediction_df){
    num_observations <- nrow(prediction_df)
    df <- prediction_df %>% count(z) %>% as.data.frame()
    #df$key <- unique(prediction_vec) %>% as.vector()
    df$distribution <- df$n / num_observations
    return(df)
}


separate_quadrats <- function(prediction_ras){

}


# Note to self: use R JSON to save and load adjacency list
# write a function that uses the list and the value to get new value
# then an lapply to the df column to create new level column
# then assemble into a convenient wrapper

# should take predictions file and map it to target type as a data frame

# then aggregate the df
# score against hand-labeled data.
get_log_filename <- function(tile_path) {
    return( 
        new_filename <- gsub(
            ".envi",
            ".log", 
            c(tile_path),
            fixed = TRUE
            )[[1]]
    )
}




safe_merge <- function(raster_one, raster_two, target_crs = NULL){
    template <- raster::projectRaster(from = raster_two, to= raster_one, alignOnly=TRUE)
    #template is an empty raster that has the projected extent of r2 but is aligned with r1 (i.e. same resolution, origin, and crs of r1)
    r2_aligned <- raster::projectRaster(from = raster_two, to= template)
    return( 
        raster::merge(
            raster_one, 
            r2_aligned,
            datatype='INT2U',
            tolerance = 1.0        ) 
    )
}

validate_results <- function(
    prediction_ras, 
    quadrat_shapefile, 
    validation_table,
    pft_key,
    template_path,
    aggregation_level = 1
 ){
    # Need to make keys shared and unique.  Really need the R equivalent of spark's RDD.reduceByKey()
    # store the results
    results <- list()
       
    for(i in 1:nrow(quadrat_shapefile)){
        # load the template 
        template <- read.csv(file = template_path, row.names = 1)

        # crop the raster to the quadrat
        quadrat_shape <- quadrat_shapefile[i,]
        quadrat_ras <- raster::crop(prediction_ras, quadrat_shape)

        plot_options <- define_plot_options(
            title = paste0("Quadrat ", i, " Predictions"),
            xLabel = "Longitude",
            yLabel = "Latitude"
        )

    

        png(paste0("./test_hist_", i, ".png"))
        hist(quadrat_ras)
        dev.off()
        plot_categorical_raster(quadrat_ras, plot_options = plot_options)

        #windows();
        
        ggsave(
            paste0("quadrat_", i, "_plot.png"),
            plot = plot_categorical_raster(
                quadrat_ras,
                colors = fg1_palette,
                plot_options = plot_options
            )
        )
        
        quadrat_df <- raster::rasterToPoints(quadrat_ras) %>% as.data.frame()

        print(quadrat_df %>% group_by(z) %>% tally())


        # prediction 
        predictions <- convert_pft_codes(quadrat_df, aggregation_level = aggregation_level, to="string")
        print(predictions %>% group_by(z) %>% tally())


        # extract the validation data for this quadrat
        quadrat_validation_df <- get_prediction_distribution(predictions) %>% as.data.frame()

      
        filtered_validation_df <- validation_table[validation_table$UID == quadrat_shape$CLASS_NAME, ]



        #print(head(filtered_validation_df))
        filtered_validation_df$Plant <- change_aggregation(
            filtered_validation_df$Plant,
            aggregation_level = aggregation_level,
            pft_key
        ) %>% as.vector()


    


        #print(head(filtered_validation_df))
        # aggregate the predictions and validation using the template
        aggregated_results <- aggregate_result_template(
            quadrat_validation_df,
            filtered_validation_df,
            template)

        print(aggregated_results)
        
        results[[i]] <- aggregated_results
    }

    return(results)
 }

 apply_chi_squared_test <- function(validation_aggregates){
     results <- lapply(
         validation_aggregates, 
         function(aggregated_results){
            return( chisq.test(
                aggregated_results$validation_counts,
                aggregated_results$predicted_counts)
            )
         }
     )
 }

apply_KS_test <- function(validation_aggregates, type="two.sided", use_monte_carlo = FALSE, exact_p = NULL){
    
   return(
        lapply(
            validation_aggregates,
            function(aggregated_results){
                prediction_cdf <- cumsum(aggregated_results$prediction_prop)
                validation_cdf <- cumsum(aggregated_results$validation_prop)
                return( ks.test(
                    prediction_cdf,
                    validation_cdf,
                    alternative=type,
                    exact = exact_p,
                    simulate.p.value = use_monte_carlo
                    )
                )
            }
        )
    )
}


 aggregate_result_template <- function(df, validation_df, input_template ){
    num_rows_df <- nrow(df)
    num_rows_template <- nrow(input_template)
    num_rows_validation <- nrow(validation_df)
    print("Number of Validation Rows:")
    print(num_rows_validation)
    num_observations <- sum(df$n)
    
    # copy the template
    template <- data.frame(input_template)

    # iterate over the template to match the data from the two other inputs
    for(template_row_idx in 1:num_rows_template){
        # iterate over the prediction data.frame
        for(df_row_idx in 1:num_rows_df){
            if(template[[template_row_idx, "key"]] == df$z[[df_row_idx]]){
                template[[template_row_idx, "prediction_prop"]] <- df$distribution[[df_row_idx]]
                template[[template_row_idx, "predicted_counts"]] <- df$n[[df_row_idx]]
            }
        }
        # iterate over validation
        for(val_row_idx in 1:num_rows_validation){
            if(template[[template_row_idx, "key"]] == validation_df[[val_row_idx, "Plant"]]){
                # store values
                current_count <- template[[template_row_idx, "validation_counts"]]
                current_prop <- template[[template_row_idx, "validation_prop"]]
                validation_raw <- validation_df[[val_row_idx, "cover_prn"]]
                # convert NAs to 0's
                print(validation_raw)
                if(is.null(validation_raw)){
                    validation_raw <- NA
                }
                additional_prop <- if(!is.na(validation_raw)) (validation_raw * 0.01) else 0.0 # ternary
                # aggregate
                template[[template_row_idx, "validation_counts"]] <- current_count + (additional_prop * num_observations)
                template[[template_row_idx, "validation_prop"]] <- current_prop + additional_prop
            }
        }
    }

    return(template)
 }


build_validation_template <- function(df, col = 5){
    pft_template <- df[, col] %>% unique() %>% as.data.frame()
    colnames(pft_template) <- c("key")
    pft_template$predicted_counts <- 0
    pft_template$prediction_prop <- 0
    pft_template$validation_counts <- 0
    pft_template$validation_prop <- 0

    return(pft_template) 
}


filter_aggregate <- function(quadrat_aggregate){
    data <- data.frame(quadrat_aggregate)
    pft_to_exclude <- (data$predicted_counts == 0) & (data$validation_counts == 0)
    data <- data[!pft_to_exclude,]

    return(data)
}

plot_quadrat_counts <- function(quadrat_aggregate, filter_missing = TRUE){
    data <- data.frame(quadrat_aggregate)
    if(filter_missing){
        data <- filter_aggregate(quadrat_aggregate)
    }

    plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(
        x=data$key,
    ))

    return ( plot )
}

plot_quadrat_proportions <- function(quadrat_aggregate, filter_missing = TRUE, plot_options = list(
    title = "Prediction and Validation",
    xLabel = "Plant Functional Type",
    yLabel = "Proportion",
    legend = c("Prediction", "Validation"),
    legendTitle = ""
)){
    data <- data.frame(quadrat_aggregate)
    if(filter_missing){
        data <- filter_aggregate(data)
    }

    print("Checking Predicted Proportions (should sum to 1)")
    print(sum(data$prediction_prop))
    print("Checking Validation Proportions (should sum to 1)")
    print(sum(data$validation_prop))

    data_tall <- tidyr::pivot_longer(
        data = data,
        cols = ends_with("_prop"),
        names_to = "Legend",
        values_to = "Proportion"
    )

    plot <- ggplot2::ggplot(data = data_tall, mapping = ggplot2::aes(
        key,
        Proportion,
        fill = Legend
    )) + 
    theme_minimal() + 
    #scale_fill_manual(values=c("#8aedff", "#a3000b")) + 
    labs(title = plot_options$title, x = plot_options$xLabel, y = plot_options$yLabel) + 
    scale_fill_discrete(name = plot_options$legendTitle, labels = plot_options$legend) +

    geom_bar(stat="identity", position = position_dodge()) + 
    theme(axis.text.x = element_text(angle=90,hjust=1,vjust=1))

    return( plot )
}


define_plot_options <- function(
    title = "",
    xLabel = "Plant Functional Type",
    yLabel = "Proportion",
    legend = c("Prediction", "Validation"),
    legendTitle = "Legend",
    save_location = NULL
){
    return( list(
        title = title,
        xLabel = yLabel,
        yLabel = xLabel,
        legend = legend,
        legendTitle = legendTitle,
        saveLocation = save_location
    ))
}

# plots the raster object wioth ggplot using theme_void


#Aboitic: #ffffff
#Forb: #db2a53
#Graminoid: #c9ae69
#Lichen: #faf87d
#Moss: #7dfaf8
#ShrubDecid: #69876b
#EvergreenShrub: #db2ad2
#TreeConifer: #2ac4db
#TreeBroadleaf: #03fc2c
#Unknown: #000000


fg1_palette <- c(
        "#ffffff",
        "#db2a53",
        "#c9ae69",
        "#faf87d",
        "#7dfaf8",
        "#69876b",
        "#db2ad2",
        "#2ac4db",
        "#03fc2c",
        "#000000"
)

fg1_names <- c(
    "Abiotic",
    "Forb",
    "Graminoid",
    "Lichen",
    "Moss",
    "ShrubDecid",
    "ShrubEvergreen",
    "TreeConifer",
    "TreeBroadleaf",
    "Unknown"
)

fg1_breaks <- c(
    '0','1','2','3','4','5','6','7','8','9'
)

fg1_palette_map <- function(value) {
    # note: value is 0-indexed and R is 1-indexed
    return (fg1_palette[[value]])
}


plot_categorical_raster <- function(ras, colors = fg1_palette, plot_options) {
    ras_plot <- rasterVis::gplot(ras, 100000000) + 
        theme_classic() +
        labs(
            title = plot_options$title,
            x = plot_options$xLabel,
            y = plot_options$yLabel,
            color = "Plant Type"
            ) + 
        scale_fill_manual(
            breaks = fg1_breaks,
            labels = fg1_names,
            values = fg1_palette, 
            name = "Functional Type"
            ) + 
        geom_tile(aes(
            fill = factor(
                value,
                ordered = FALSE))) + 
        coord_quickmap()


    return(ras_plot)
}
