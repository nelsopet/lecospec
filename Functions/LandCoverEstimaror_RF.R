#' Functions returns columns that are bandpasses
#' 
#' @inheritParams None
#' @return a dataframe without non-bandpass columns
#' @param x: a dataframe
#' @seealso .RemoveBandColumn
#' @export 
#' @examples Not Yet Implmented
.RemoveMetaColumn<-function(x){
    meta<-c(grep("^[0-9][0-9][0-9]",colnames(x)))
    colremove<-x[,meta]
    return(colremove)
}
  
#' Functions returns columns that are NOT bandpasses
#' 
#' Long Description here
#' 
#' @inheritParams None
#' @return explanation
#' @param x: a dataframe
#' @seealso .RemoveMetaColumn
#' @export 
#' @examples Not Yet Implmented
#' 
.RemoveBandColumn<-function(x){   
    meta<-c(grep("[a-z A-Z]",colnames(x)))
    colremove <- x[,meta]
    return(colremove)
}

#' Resamples the given dataframe to every 'wavelength' nanometers 
#' 
#' @inheritParams None
#' @return A dataframe with the spectral compaonents with the specified wavelength separation 
#'  between bands
#' @param df: a dataframe
#' @param wavelength: The wavelength separation for columns, in nanometers.
#' @seealso None
#' @export 
#' @examples Not Yet Implmented 
ResampleSpectralDataFrame <- function(df, wavelength=5){
    #Separate out data columns & convert to spectal object
    dfNoMetadata <- .RemoveMetaColumn(df)
    specLibDf <- spectrolab::as_spectra(dfNoMetadata)
    
    # resample to new data frame
    resampledDfNoMetadata <- spectrolab::resample(specLibDf, seq(397.593, 899.424, wavelength)) %>%
        as.data.frame() %>%
        dplyr::select(-sample_name)
    
    # rename columns and add metadata
    colnames(resampledDfNoMetadata) <- paste(colnames(resampledDfNoMetadata), "5nm", sep="_")
    resampledDf <- cbind(.RemoveBandColumn(df), resampledDfNoMetadata)
   
    return(resampledDf)
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
ExtractBands <- function(df){
    bands <- .RemoveBandColumn(df) %>%
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
DataframeToSpecLib <- function(df) {
    # Convert to a spectral library
    df_no_metadata <- .RemoveMetaColumn(df)
    spectralMatrix <- as.matrix(df_no_metadata)
    bands <- ExtractBands(df)
    spectralLib <- hsdar::speclib(spectralMatrix, bands)
    return(spectralLib)
}

#' Converts a spaectral library to a dataframe
#' 
#' @inheritDotParams 
#' @return A dataframe
#' @param speclib: A spectral library to convert
#' @export
#' 
SpecLibToDataframe <- function(speclib){
    print("Not Implemented")
}


#' Calculates the Vegetation Index (via hsdar package)
#' 
#' Long Description here
#' 
#' @inheritParams None
#' @return 
#' @param df: A dataframe of spectral data 
#' @param indices: base vegetation index for the calculations.
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
    ){
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

    for (f in functions[2:pipeline_length]){
        x <- f(x)
    }

    return(x)
}