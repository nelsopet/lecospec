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
    dfNoMetadata <- .RemoveMetaColumn(df)
    spectralMatrix <- as.matrix(dfNoMetadata)
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
#' @seealso hsdar::vegindex()
#' @export 
#' @examples Not Yet Implmented
CalculateVegIndexFromDf <- function(df, 
    indices = hsdar::vegindex(), 
    aviris = c(-58), 
    headwall = -c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)
    ){
        
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
CalculateVegIndexFromSpecLib <- function(speclib){

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
CalculateVegIndex <- function(x){

}