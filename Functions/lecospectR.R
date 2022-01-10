require(tidyverse)
require(compiler)
require(raster)
require(parallel)
require(doParallel)
require(hsdar)
require(spectrolab)
require(ranger)
require(stringr)
require(stringi)
require(rjson)
require(rgdal)
require(gdalUtils)
require(magrittr)
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
        seq(397.593,899.424,5)
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
impute_spectra <- function(x, cluster = NULL, method = "missForest") {

    df <- x

    if (!is.data.frame(x)) {
        df <- x %>%
        rasterToPoints() %>%
        as.data.frame()
    }

    print("Imputing...")
    if (method == "missForest") {
        missForest::missForest(df, maxiter = 3)
        output_data <- df
    } else {
        output_data <- useful::simple.impute(df)
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
        random_filename <- paste0("tile_", random_tile_id, ".grd")
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
aggregate_results_df <- function(predictions, x, y) {

    dfx <- data.frame(x)
    colnames(dfx) <- c("x")
    dfxy <- cbind(dfx, y)
    z <- c()

    for (my_prediction in predictions) {
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
    output_filepath = "./predictions.grd",
    use_external_bands = FALSE
) {


    # Read in the configuration file
    config <- rjson::fromJSON(file = config_path)

    # determine the number of cores to use
    num_cores <- parallel::detectCores() #detect cores on system
    # see if the number of cores to use is specified in the config
    if(is.integer(config$clusterCores)){
        num_cores <- config$clusterCores
    }
    # set up the parallel cluster
    raster::beginCluster(num_cores)
    cl <- raster::getCluster()


    print(paste0(num_cores, " Cores Detected for processing..."))


    # Load the model
    model <- load_model(config$model_path)

    # load the input datacube and split into tiles
    input_raster <- raster::brick(input_filepath)
    crs_cache <- raster::crs(input_raster)

    if(use_external_bands){
        band_count <- raster::nlayers(input_raster)
        bandnames <- read.csv(config$external_bands)$x[1:band_count] %>% as.vector()
        names(input_raster) <- bandnames
    }

    num_tiles_x <- config$x_tiles
    num_tiles_y <- config$y_tiles

    if(config$automatic_tiling){
        num_tiles_x <- calc_num_tiles(input_filepath)
        num_tiles_y <- calc_num_tiles(input_filepath)
    }

    tile_filenames <- make_tiles(
        input_raster,
        num_x = config$x_tiles,
        num_y = config$y_tiles,
        save_path = config$tile_path,
        cluster = cl,
        verbose = FALSE
    )

    rm(input_raster)
    gc()

    prediction_filenames <- lapply(
        tile_filenames,
        function(tile_filename){
            return(.convert_tile_filename(tile_filename, config = config))
    }) %>% as.vector()

    # initialize the variable for the tilewise results
    tile_results <- NULL
    #edge artifacts?
    if(config$parallelize_by_tiles){
        doParallel::registerDoParallel(cl)
        tile_results <- foreach::foreach(
            i = seq_along(tile_filenames),
            .export = as.vector(ls(.GlobalEnv))
        ) %dopar% {
            tile_results = unlist(process_tile(
                tile_filename = tile_filenames[[i]],
                ml_model = model, 
                cluster = NULL,
                return_raster = TRUE,
                return_filename = TRUE,
                save_path = prediction_filenames[[i]],
                suppress_output = TRUE,
                aggregation = config$aggregation))
            gc()#garbage collect between iterations
        }
    } else {
        tile_results <- foreach::foreach(
            i=seq_along(tile_filenames), 
            .export = c("model", "cl")
        ) %do% {
           tile_results = unlist(process_tile(
                tile_filename = tile_filenames[[i]],
                ml_model = model, 
                cluster = cl,
                return_raster = TRUE,
                return_filename = TRUE,
                save_path = prediction_filenames[[i]],
                suppress_output = TRUE,
                aggregation = config$aggregation))
            gc()
        }
    }
    gc() #clean up

    

    print("Tile based processing complete")
    print(tile_results)

    results <- merge_tiles(
        prediction_filenames, 
        output_path = output_filepath,
        set_crs = crs_cache)

    raster::endCluster()

    return(results)
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
    cluster = NULL,
    return_raster = TRUE,
    return_filename = FALSE,
    save_path = NULL,
    suppress_output = FALSE,
    aggregation = 2
    ) {
    raster_obj <- raster::brick(tile_filename)
    input_crs <- raster::crs(raster_obj)
    input_extent <- raster::extent(raster_obj)
    input_res <- raster::res(raster_obj)
    print(paste0("preprocessing raster at ", tile_filename))
    base_df <- preprocess_raster_to_df(raster_obj, ml_model)
    
    if(nrow(base_df) == 0){
        if(!suppress_output){
            if(return_raster){
                return(raster_obj)
            } else {
                return(base_df)
            } 
        } else {
            return(save_path)
        }
    }

        #if there is no data, return the empty tile in the specified format

    rm(raster_obj)
    gc()

    target_indices <- get_required_veg_indices(ml_model)

    imputed_df <- impute_spectra(base_df)
    rm(base_df)
    gc()

    resampled_df <- resample_df(imputed_df)
    gc()

    veg_indices <- get_vegetation_indices(resampled_df, ml_model, cluster = cluster)

    df <- cbind(resampled_df, veg_indices)
    #df <- df %>% dplyr::select(x, y, dplyr::all_of(target_model_cols)) 
    # above line should not be needed, testing then deleting
    rm(veg_indices)
    rm(resampled_df)
    gc()

    
    predictions <- apply_model(df, ml_model) %>% as.data.frame()
    rm(df)
    gc()

    predictions <- postprocess_predictions(predictions, imputed_df)
    
    predictions <- convert_and_save_output(
        predictions,
        save_path = save_path,
        return_raster = return_raster,
        aggregation = aggregation,
        target_extent = input_extent,
        target_crs = input_crs,
        target_res = input_res)

    if(suppress_output){
        return(save_path)
    } else {
        return(predictions)
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
    df[-1:-2][df[-1:-2] > 1.5] <- NA
    df[-1:-2][df[-1:-2] < 0  ] <- NA
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
preprocess_raster_to_df <- function(raster_obj, model) {
    df <- raster::rasterToPoints(raster_obj) %>% as.data.frame()
    df <- remove_noisy_cols(df)
    df <- filter_bands(df)
    df <- filter_empty_points(df) 
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

    prediction <-predict(
        model,
        data = df,
        type='response',
        num.threads = threads
    ) #(Ranger model)
    #print(predictions$prediction)
    return(prediction$predictions %>% as.data.frame())
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
convert_fg2_strings <- function(df) {
    df_convert_results <- df %>% dplyr::mutate(z = case_when(
        z == "Abiotic" ~ 1,
        z == "Forb" ~ 2,
        z == "Graminoid" ~ 3,
        z == "Lichen_Dark" ~ 4,
        z == "Lichen_Light" ~ 5,
        z == "Lichen_Yellow" ~ 6,
        z == "Moss" ~ 7, 
        z == "Shrub_Decid" ~ 8,
        z == "Shrub_Evergreen" ~ 9,
        z == "Tree_Decid" ~ 10,
        z == "Tree_Evergreen" ~ 11,
        z == "Unknown" ~ 12
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
    #writeRaster(pred_merged, filename = "Output/Predictions/")
    return(pred_merged)
}

merge_tiles <- function(input_files, output_path = NULL, target_layer = 1, set_crs = NULL) {

    master_raster <- as(raster::brick(input_files[[1]])[[target_layer]], "RasterLayer")

    for (input_file in tail(input_files, -1)) {
        new_raster <- as(raster::brick(input_file)[[target_layer]], "RasterLayer")
        # above is robust against multi-layer images
        master_raster <- raster::merge(
            master_raster,
            new_raster,
            tolerance = 0.5)
        
    }

    if(!is.null(set_crs)){
        raster::crs(master_raster) <- set_crs
    }

    if(!is.null(output_path)) {
        raster::writeRaster(master_raster, output_path, overwrite = TRUE)
    }

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
.convert_tile_filename <- function(tile_path, config) {
    path <- gsub(config$tile_path, config$output_path, c(tile_path))
    return(gsub("tile", "prediction", path)[[1]])
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
            z == 1 ~ "Abiotic",
            z == 2 ~ "Forb",
            z == 3 ~ "Graminoid",
            z == 4 ~ "Lichen_Dark",
            z == 5 ~ "Lichen_Light",
            z == 6 ~ "Lichen_Yellow",
            z == 7 ~ "Moss",
            z == 8 ~ "Shrub_Decid",
            z == 9 ~ "Shrub_Evergreen",
            z == 10 ~ "Tree_Decid",
            z == 11 ~ "Tree_Evergreen",
            z == 12 ~ "Unknown"
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
            z == "Dwarf_Shrub_Decid" ~ 1,
			z == "Forb" ~ 2,
			z == "Graminoid_Grass" ~ 3,
			z == "Graminoid_Sedge" ~ 4,
			z == "Lichen_Crustose_Dark" ~ 5,
			z == "Lichen_Crustose_Light" ~ 6,
			z == "Lichen_Epiphyte_Dark" ~ 7,
			z == "Lichen_Epiphyte_Yellow" ~ 8,
			z == "Lichen_Foliose_Dark" ~ 9,
			z == "Lichen_Foliose_Dark_Peltigera" ~ 10,
			z == "Lichen_Foliose_Light" ~ 11,
			z == "Lichen_Foliose_Yellow" ~ 12,
			z == "Lichen_Fruticose_Dark" ~ 13,
			z == "Lichen_Fruticose_Light" ~ 14,
			z == "Lichen_Fruticose_Yellow" ~ 15,
			z == "Litter" ~ 16,
			z == "Moss_Aulacomnium" ~ 17,
			z == "Moss_Ceratadon" ~ 18,
			z == "Moss_Dicranum" ~ 19,
			z == "Moss_Hylocomnium" ~ 20,
			z == "Moss_Plagiomnium" ~ 21,
			z == "Moss_Pleurozium" ~ 22,
			z == "Moss_Polytrichum" ~ 23,
			z == "Moss_Racomitrium" ~ 24,
			z == "Moss_Rhytidium" ~ 25,
			z == "Moss_Sphagnum_fuscum" ~ 26,
			z == "Moss_Sphagnum_other" ~ 27,
			z == "Moss_Tomenthypnum" ~ 28,
			z == "Rock" ~ 29,
			z == "Shrub_Alder" ~ 30,
			z == "Shrub_Betula" ~ 31,
			z == "Shrub_Evergreen" ~ 32,
			z == "Shrub_Rosa" ~ 33,
			z == "Shrub_Salix" ~ 34,
			z == "Soil" ~ 35,
			z == "Tree_Decid" ~ 36,
			z == "Tree_Evergreen" ~ 37,
			z == "Unknown" ~ 38,
			z == "Wood_Coarse" ~ 39

    )) %>%
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
            z == 1 ~ "Dwarf_Shrub_Decid",
			z == 2 ~ "Forb",
			z == 3 ~ "Graminoid_Grass",
			z == 4 ~ "Graminoid_Sedge",
			z == 5 ~ "Lichen_Crustose_Dark",
			z == 6 ~ "Lichen_Crustose_Light",
			z == 7 ~ "Lichen_Epiphyte_Dark",
			z == 8 ~ "Lichen_Epiphyte_Yellow",
			z == 9 ~ "Lichen_Foliose_Dark",
			z == 10 ~ "Lichen_Foliose_Dark_Peltigera",
			z == 11 ~ "Lichen_Foliose_Light",
			z == 12 ~ "Lichen_Foliose_Yellow",
			z == 13 ~ "Lichen_Fruticose_Dark",
			z == 14 ~ "Lichen_Fruticose_Light",
			z == 15 ~ "Lichen_Fruticose_Yellow",
			z == 16 ~ "Litter",
			z == 17 ~ "Moss_Aulacomnium",
			z == 18 ~ "Moss_Ceratadon",
			z == 19 ~ "Moss_Dicranum",
			z == 20 ~ "Moss_Hylocomnium",
			z == 21 ~ "Moss_Plagiomnium",
			z == 22 ~ "Moss_Pleurozium",
			z == 23 ~ "Moss_Polytrichum",
            z == 24 ~ "Moss_Racomitrium",
			z == 25 ~ "Moss_Rhytidium",
			z == 26 ~ "Moss_Sphagnum_fuscum",
			z == 27 ~ "Moss_Sphagnum_other",
			z == 28 ~ "Moss_Tomenthypnum",
			z == 29 ~ "Rock",
			z == 30 ~ "Shrub_Alder",
			z == 31 ~ "Shrub_Betula",
			z == 32 ~ "Shrub_Evergreen",
			z == 33 ~ "Shrub_Rosa",
			z == 34 ~ "Shrub_Salix",
			z == 35 ~ "Soil",
			z == 36 ~ "Tree_Decid",
			z == 37 ~ "Tree_Evergreen",
			z == 38 ~ "Unknown",
			z == 39 ~ "Wood_Coarse"

    ))
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
            z == "abibal"	 ~ 1,
			z == "acepen" ~ 2,
			z == "acerub" ~ 3,
			z == "aleoch" ~ 4,
			z == "alnfru" ~ 5,
			z == "alninc" ~ 6,
			z == "ALVI5" ~ 7,
			z == "ARALA7" ~ 8,
			z == "arccen" ~ 9,
			z == "arcnig" ~ 10,
			z == "arcrub" ~ 11,
			z == "arcsta" ~ 12,
			z == "arctop" ~ 13,
			z == "ARFU2" ~ 14,
			z == "ARLA2" ~ 15,
			z == "asachr" ~ 16,
			z == "aulpal" ~ 17,
			z == "aultur" ~ 18,
			z == "bare rock" ~ 19,
			z == "bare_soil" ~ 20,
			z == "BEGL" ~ 21,
			z == "BENA" ~ 22,
			z == "BEPU4" ~ 23,
			z == "betall" ~ 24,
			z == "betnan" ~ 25,
			z == "betneo" ~ 26,
			z == "betpap" ~ 27,
			z == "betpop" ~ 28,
			z == "bryoria" ~ 29,
			z == "CAAQ" ~ 30,
			z == "calcan" ~ 31,
			z == "carlin" ~ 32,
			z == "carlyn" ~ 33,
			z == "carram" ~ 34,
			z == "castet" ~ 35,
			z == "cerpur" ~ 36,
			z == "cetisl" ~ 37,
			z == "cetlae" ~ 38,
			z == "claama" ~ 39,
			z == "clacor" ~ 40,
			z == "clacuc" ~ 41,
			z == "clagra" ~ 42,
			z == "clamit" ~ 43,
			z == "claran" ~ 44,
			z == "claste" ~ 45,
			z == "clasty" ~ 46,
			z == "clasul" ~ 47,
			z == "claunc" ~ 48,
			z == "dacarc" ~ 49,
			z == "DAFRF" ~ 50,
			z == "dead salix" ~ 51,
			z == "dicranum" ~ 52,
			z == "DOFR" ~ 53,
			z == "DROCA2" ~ 54,
			z == "dryala" ~ 55,
			z == "dryhyb" ~ 56,
			z == "dryoct" ~ 57,
			z == "DUFI" ~ 58,
			z == "EMNI" ~ 59,
			z == "empnig" ~ 60,
			z == "EQAR" ~ 61,
			z == "equarv" ~ 62,
			z == "equsyl" ~ 63,
			z == "ERAN6" ~ 64,
			z == "erivag" ~ 65,
			z == "ERVA4" ~ 66,
			z == "evemes" ~ 67,
			z == "faggra" ~ 68,
			z == "flacuc" ~ 69,
			z == "flaniv" ~ 70,
			z == "fraame" ~ 71,
			z == "gravel" ~ 72,
			z == "grey_rhizocarpon" ~ 73,
			z == "herlan" ~ 74,
			z == "HIERA" ~ 75,
			z == "hylspl" ~ 76,
			z == "hypaus" ~ 77,
			z == "hypspl" ~ 78,
			z == "icmeri" ~ 79,
			z == "irisit" ~ 80,
			z == "KAPR" ~ 81,
			z == "larlar" ~ 82,
			z == "leddec" ~ 83,
			z == "LEDUM" ~ 84,
			z == "loipro" ~ 85,
			z == "luparc" ~ 86,
			z == "masric" ~ 87,
			z == "melanelia" ~ 88,
			z == "melhep" ~ 89,
			z == "naparc" ~ 90,
			z == "neparc" ~ 91,
			z == "orange_Porpidia" ~ 92,
			z == "paramb" ~ 93,
			z == "paromp" ~ 94,
			z == "parsul" ~ 95,
			z == "pedrac" ~ 96,
			z == "pedsud" ~ 97,
			z == "PEFR5" ~ 98,
			z == "pelapt" ~ 99,
			z == "pelleu" ~ 100,
			z == "pelmal" ~ 101,
			z == "pelsca" ~ 102,
			z == "petfri" ~ 103,
			z == "picmar" ~ 104,
			z == "picrub" ~ 105,
			z == "pilaci" ~ 106,
			z == "pinstr" ~ 107,
			z == "plagiomnium" ~ 108,
			z == "plesch" ~ 109,
			z == "poljen" ~ 110,
			z == "poljun" ~ 111,
			z == "polstr" ~ 112,
			z == "polytrichum" ~ 113,
			z == "popbal" ~ 114,
			z == "popbal" ~ 115,
			z == "popgra" ~ 116,
			z == "prupen" ~ 117,
			z == "quartz" ~ 118,
			z == "querub" ~ 119,
			z == "raclan" ~ 120,
			z == "rhigeo" ~ 121,
			z == "rhutyp" ~ 122,
			z == "rhyrug" ~ 123,
			z == "rosaci" ~ 124,
			z == "rosasc" ~ 125,
			z == "rubcam" ~ 126,
			z == "rubcha" ~ 127,
			z == "RUCH" ~ 128,
			z == "SAG" ~ 129,
			z == "salala" ~ 130,
			z == "salarb" ~ 131,
			z == "salgla" ~ 132,
			z == "sallan" ~ 133,
			z == "salova" ~ 134,
			z == "Salova" ~ 135,
			z == "salpul" ~ 136,
			z == "salric" ~ 137,
			z == "SAPH" ~ 138,
			z == "SAPU15" ~ 139,
			z == "SAPU6" ~ 140,
			z == "SARI4" ~ 141,
			z == "sphagn" ~ 142,
			z == "sphfus" ~ 143,
			z == "spruce bark" ~ 144,
			z == "stepas" ~ 145,
			z == "stetas" ~ 146,
			z == "thuocc" ~ 147,
			z == "toefeldia" ~ 148,
			z == "tomnit" ~ 149,
			z == "tragra" ~ 150,
			z == "tsucan" ~ 151,
			z == "umbarc" ~ 152,
			z == "umbhyp" ~ 153,
			z == "usnlap" ~ 154,
			z == "usnsca" ~ 155,
			z == "vaculi" ~ 156,
			z == "vacvit" ~ 157,
			z == "VAUL" ~ 158,
			z == "VAVI" ~ 159,
			z == "vulpin" ~ 160,
			z == "wooly_salix" ~ 161
        )
    )
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
    converted_df <- df %>% mutate(
        case_when(
            z == 1 ~ "abibal",
			z == 2 ~ "acepen",
			z == 3 ~ "acerub",
			z == 4 ~ "aleoch",
			z == 5 ~ "alnfru",
			z == 6 ~ "alninc",
			z == 7 ~ "ALVI5",
			z == 8 ~ "ARALA7",
			z == 9 ~ "arccen",
			z == 10 ~ "arcnig",
			z == 11 ~ "arcrub",
			z == 12 ~ "arcsta",
			z == 13 ~ "arctop",
			z == 14 ~ "ARFU2",
			z == 15 ~ "ARLA2",
			z == 16 ~ "asachr",
			z == 17 ~ "aulpal",
			z == 18 ~ "aultur",
			z == 19 ~ "bare rock",
			z == 20 ~ "bare_soil",
			z == 21 ~ "BEGL",
			z == 22 ~ "BENA",
			z == 23 ~ "BEPU4",
			z == 24 ~ "betall",
			z == 25 ~ "betnan",
			z == 26 ~ "betneo",
			z == 27 ~ "betpap",
			z == 28 ~ "betpop",
			z == 29 ~ "bryoria",
			z == 30 ~ "CAAQ",
			z == 31 ~ "calcan",
			z == 32 ~ "carlin",
			z == 33 ~ "carlyn",
			z == 34 ~ "carram",
			z == 35 ~ "castet",
			z == 36 ~ "cerpur",
			z == 37 ~ "cetisl",
			z == 38 ~ "cetlae",
			z == 39 ~ "claama",
			z == 40 ~ "clacor",
			z == 41 ~ "clacuc",
			z == 42 ~ "clagra",
			z == 43 ~ "clamit",
			z == 44 ~ "claran",
			z == 45 ~ "claste",
			z == 46 ~ "clasty",
			z == 47 ~ "clasul",
			z == 48 ~ "claunc",
			z == 49 ~ "dacarc",
			z == 50 ~ "DAFRF",
			z == 51 ~ "dead salix",
			z == 52 ~ "dicranum",
			z == 53 ~ "DOFR",
			z == 54 ~ "DROCA2",
			z == 55 ~ "dryala",
			z == 56 ~ "dryhyb",
			z == 57 ~ "dryoct",
			z == 58 ~ "DUFI",
			z == 59 ~ "EMNI",
			z == 60 ~ "empnig",
			z == 61 ~ "EQAR",
			z == 62 ~ "equarv",
			z == 63 ~ "equsyl",
			z == 64 ~ "ERAN6",
			z == 65 ~ "erivag",
			z == 66 ~ "ERVA4",
			z == 67 ~ "evemes",
			z == 68 ~ "faggra",
			z == 69 ~ "flacuc",
			z == 70 ~ "flaniv",
			z == 71 ~ "fraame",
			z == 72 ~ "gravel",
			z == 73 ~ "grey_rhizocarpon",
			z == 74 ~ "herlan",
			z == 75 ~ "HIERA",
			z == 76 ~ "hylspl",
			z == 77 ~ "hypaus",
			z == 78 ~ "hypspl",
			z == 79 ~ "icmeri",
			z == 80 ~ "irisit",
			z == 81 ~ "KAPR",
			z == 82 ~ "larlar",
			z == 83 ~ "leddec",
			z == 84 ~ "LEDUM",
			z == 85 ~ "loipro",
			z == 86 ~ "luparc",
			z == 87 ~ "masric",
			z == 88 ~ "melanelia",
			z == 89 ~ "melhep",
			z == 90 ~ "naparc",
			z == 91 ~ "neparc",
			z == 92 ~ "orange_Porpidia",
			z == 93 ~ "paramb",
			z == 94 ~ "paromp",
			z == 95 ~ "parsul",
			z == 96 ~ "pedrac",
			z == 97 ~ "pedsud",
			z == 98 ~ "PEFR5",
			z == 99 ~ "pelapt",
			z == 100 ~ "pelleu",
			z == 101 ~ "pelmal",
			z == 102 ~ "pelsca",
			z == 103 ~ "petfri",
			z == 104 ~ "picmar",
			z == 105 ~ "picrub",
			z == 106 ~ "pilaci",
			z == 107 ~ "pinstr",
			z == 108 ~ "plagiomnium",
			z == 109 ~ "plesch",
			z == 110 ~ "poljen",
			z == 111 ~ "poljun",
			z == 112 ~ "polstr",
			z == 113 ~ "polytrichum",
			z == 114 ~ "popbal",
			z == 115 ~ "popbal",
			z == 116 ~ "popgra",
			z == 117 ~ "prupen",
			z == 118 ~ "quartz",
			z == 119 ~ "querub",
			z == 120 ~ "raclan",
			z == 121 ~ "rhigeo",
			z == 122 ~ "rhutyp",
			z == 123 ~ "rhyrug",
			z == 124 ~ "rosaci",
			z == 125 ~ "rosasc",
			z == 126 ~ "rubcam",
			z == 127 ~ "rubcha",
			z == 128 ~ "RUCH",
			z == 129 ~ "SAG",
			z == 130 ~ "salala",
			z == 131 ~ "salarb",
			z == 132 ~ "salgla",
			z == 133 ~ "sallan",
			z == 134 ~ "salova",
			z == 135 ~ "Salova",
			z == 136 ~ "salpul",
			z == 137 ~ "salric",
			z == 138 ~ "SAPH",
			z == 139 ~ "SAPU15",
			z == 140 ~ "SAPU6",
			z == 141 ~ "SARI4",
			z == 142 ~ "sphagn",
			z == 143 ~ "sphfus",
			z == 144 ~ "spruce bark",
			z == 145 ~ "stepas",
			z == 146 ~ "stetas",
			z == 147 ~ "thuocc",
			z == 148 ~ "toefeldia",
			z == 149 ~ "tomnit",
			z == 150 ~ "tragra",
			z == 151 ~ "tsucan",
			z == 152 ~ "umbarc",
			z == 153 ~ "umbhyp",
			z == 154 ~ "usnlap",
			z == 155 ~ "usnsca",
			z == 156 ~ "vaculi",
			z == 157 ~ "vacvit",
			z == 158 ~ "VAUL",
			z == 159 ~ "VAVI",
			z == 160 ~ "vulpin",
			z == 161 ~ "wooly_salix"
        )
    )
}


convert_pft_string <- function(df, key_path){
    key_df <- read.csv(key_path)
    saved_names <- colnames(df)
    mergded_df <- merge(df, key_df, by.x=z, by.y=names) %>% 
    select(x,y,id)
    colnames(merged_df) <- saved_names

    return(merged_df)
}

convert_pft <- function(df_xyz, aggregation = 2, to_string = FALSE){
    if(aggregation == 0){
        if(to_string){
            return(convert_species_string(df_xyz))
        } else {
            return(convert_species_int(df_xyz))
        }
    } else if (aggregation == 1) {
        if(to_string){
            return(convert_fg1_string(df_xyz))
        } else {
            return(convert_fg1_int(df_xyz))
        }
    } else if (aggregation == 2) {
        if(to_string){
            return(convert_fg2_string(df_xyz))
        } else {
            return(convert_fg2_int(df_xyz))
        } 
    } else {
           if(file.exists(aggregation)){
               return(convert_pft_string(df_xyz, aggregation))
           } else {
               # invalid key file and not a prespecified aggregation
              stop("Invalid aggregation specified: parameter must be 0,1,2 or a valid filepath.")
           }
    }
}


#' Converts the output to the correct data type (base::data.frame or raster::rasterLayer) and save it to disk
#'
#' In addition to converting the file, it saves the file to disk.  
#' In order to avoid issues with overwriting files, the function will 
#' adjoin 16 random characters to the filename to avoid collision.
#'
#' @return 
#' @param df: A data.frame
#' @param save_path: the file path for saving the converted file.  If NULL (default), no file is saved.  
#' @param return_raster: If TRUE, the function converts 'df' to a raster before saving and returning the converted data. 
#' If FALSE, the data.frame is saved to disk, but no coversion is made.
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_and_save_output <- function(df, save_path = NULL, return_raster = TRUE, aggregation = 2, target_extent = NULL, target_crs = NULL, target_res = NULL){
        if(return_raster){
        predictions <- convert_pft(df, aggregation = aggregation, to_string = FALSE)
        print("Dimensions of Output dataframe:")
        print(dim(df))
        print("Dimensions of converted dataframe (string -> int):")
        print(dim(predictions))
        predictions <- raster::rasterFromXYZ(predictions)

        # set the exent and CRS if they are supplied by the user
        if(!is.null(target_extent)){
            raster::extent(predictions) <- target_extent
        }
        if(!is.null(target_crs)){
            raster::crs(predictions) <- target_crs
        }
        if(!is.null(target_res)){
            raster::res(predictions) <- target_res
        }
        if(!is.null(save_path)){
            if(file.exists(save_path)){
                random_string <- paste0("_", stringi::stri_rand_strings(1,4), ".")[[1]]
                new_save_path <- gsub(".", random_string, c(save_path))[[1]]
                raster::writeRaster(predictions, filename = new_save_path)
            } else {
                raster::writeRaster(predictions, filename = save_path)
            }
        }
        return(predictions)
    } else {
        if(!is.null(save_path)){
            if(file.exists(save_path)){
                random_string <- paste0("_", stringi::stri_rand_strings(1,4), ".")[[1]]
                new_save_path <- gsub(".", random_string, c(save_path))[[1]]
                write.csv(predictions, new_save_path)
            } else {
                write.csv(df, save_path)
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
#' @param predictions_df: A data.frame of predictions from the model.  It should have a single column of data.
#' @param base_df: the base data.frame for creating the predictions; 
#' it should have columns x and y specifying the spatial coordinates for that row.
#' @return a data.frame with three columns: 'x', 'y', and 'z'.  'x' and 'y' are the spatial location of the 
#' prediction in the original CRS, and the 'z' column is predictions. 
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
postprocess_predictions <- function(predictions_df, base_df){
    colnames(predictions_df) <- c("z")
    predictions_df$x <- base_df$x
    predictions_df$y <- base_df$y
    gc()

    predictions_df <- predictions_df %>% dplyr::select(x, y, z)

    return(predictions_df)
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
visualize_predictions <- function(filepath, colormap){

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
    drop_cols <- c("x","y")
    columns_to_check <- setdiff(colnames(df), drop_cols) 
    df_no_empty_rows <- df %>% 
        dplyr::filter_at(
            .vars = vars(one_of(columns_to_check)),
            ~ !is.na(.)
        )
    return(df_no_empty_rows)
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
calc_num_tiles <- function(file_path, max_size = 128){
    file_size <- file.info(file_path)$size
    tile_size <- file_size / max_size
    num_xy <- ceiling(sqrt(tile_size))
    return(num_xy)
}

# talk to NASA spectral imaging working group r/e gaps