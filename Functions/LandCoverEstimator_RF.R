require(tidyverse)
require(compiler)
require(raster)
require(parallel)
require(doParallel)
require(hsdar)
require(spectrolab)
require(ranger)
require(stringr)

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

    if (! is.data.frame(x)) {
        df <- x %>%
        rasterToPoints() %>%
        as.data.frame()
    }

    print("Imputing...")
    if (method == "missForest") {
        missForest::missForest(df, maxiter = 3)
        output_data <- df
    } else {
        output_data <- useful::simple.impute(df )
    }
        
    if (! is.data.frame(x)) {
        spectral_matrix <- as.matrix(df)
        output_data <- raster::rasterFromXYZ(
            spectral_matrix,
            crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
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
    output_filepath = "./",
    cache_filepath = "./",
    output_filename = "predictions",
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
    if(use_external_bands){
        band_count <- raster::nlayers(input_raster)
        bandnames <- read.csv(config$external_bands)$x[1:band_count]
        names(input_raster) <- bandnames
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

    # initialize the variable for the tilewise results
    tile_results <- NULL
    #edge artifacts?
    if(config$parallelize_by_tiles){
        doParallel::registerDoParallel(cl)
        tile_results <- foreach(
            tile_filename=tile_filenames,
            .export = c("model")
        ) %dopar% {
            process_tile(
                tile_filename = tile_filename,
                ml_model = model, 
                cluster = NULL)
            gc()#garbage collect between iterations
        }
    } else {
        tile_results <- foreach(
            tile_filename=tile_filenames, 
            .export = c("model", "cl")
        ) %do% {
            process_tile(
                tile_filename = tile_filename,
                ml_model = model, 
                cluster = cl)
            gc()
        }

    }
    gc() #clean up

    raster::endCluster()

    print("Tile based processing complete")

    results <- aggregate_results_df(tile_results)

    return(results)
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
process_tile <- function(tile_filename, ml_model, cluster = NULL, return_raster = TRUE) {
    raster_obj <- raster::brick(tile_filename)
    print(paste0("preprocessing raster at ", tile_filename))
    base_df <- preprocess_raster_to_df(raster_obj, ml_model)
    if(nrow(base_df) == 0){
        #if there is no data, return the empty tile in the specified format
        if(return_raster){
            return(raster_obj)
        } else {
            return(base_df)
        }
        
    }

    rm(raster_obj)
    gc()

    target_indices <- get_required_veg_indices(ml_model)

    imputed_df <- impute_spectra(base_df)
    rm(base_df)
    gc()

    resampled_df <- resample_df(imputed_df)
    gc()
    print(summary(imputed_df))

    veg_indices <- get_vegetation_indices(resampled_df, ml_model, cluster = cluster)

    df <- cbind(resampled_df, veg_indices)
    #df <- df %>% dplyr::select(x, y, dplyr::all_of(target_model_cols)) 
    # above line should not be needed, testing then deleting
    rm(veg_indices)
    gc()

    
    predictions <- apply_model(df, ml_model) %>% as.data.frame()
    rm(df)
    gc()

    print(paste0("Completed Processing tile at ", tile_filename))

    predictions$x <- imputed_df$x
    predictions$y <- imputed_df$y
    gc()

    if(return_raster){
        print(colnames(predictions))
        return(raster::rasterFromXYZ(predictions))

        
    }
    return(predictions)
}

# a quick function based on the original code
remove_noisy_cols <- function(df) {
    return(df[1:274])
}

filter_bands <- function(df) {
    # from original code, but sets the values to NA instead of -999 
    df[-1:-2][df[-1:-2] > 1.5] <- NA
    df[-1:-2][df[-1:-2] < 0  ] <- NA
    df[-1:-2][sapply(df[-1:-2], is.nan)] <- NA
    df[-1:-2][sapply(df[-1:-2], is.infinite)] <- NA
    return(df)
}

preprocess_raster_to_df <- function(raster_obj, model) {
    df <- raster::rasterToPoints(raster_obj) %>% as.data.frame()
    df <- remove_noisy_cols(df)
    df <- filter_bands(df)
    gc()
    target_model_cols <- get_var_names(model)
    #new_names <- clean_df_colnames(df)
    #colnames(df) <- new_names
    
    gc()
    #df <- impute_spectra(df)
    return(df)
}



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
            stop()
        })
        
        corrected_value <- int_conversion 
        corrected_values[[w]] <- corrected_value
    }
    return(corrected_values)
}

clean_names <- function(variables){
    return(
        stringr::str_remove_all(
            variables,
            "[[:punct:]]| "
            )
        )
}

apply_model <- function(df, model, threads = 1, clean_names = TRUE){

    prediction <-predict(
        model,
        data = df,
        type='response',
        num.threads = threads
    ) #(Ranger model)
    return(prediction$predictions)
}




