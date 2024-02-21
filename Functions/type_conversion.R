#' Converts Data Frame to a spaectral library
#'
#' @return A spectral library
#' @param df: A dataframe to convert
#' @export
#'
df_to_speclib <- function(df, type = "hsdar", use_external_bands = FALSE) {
    # Convert to a spectral library
    # print(colnames(df))
    df_no_metadata <- remove_meta_column(df)


    bands <- extract_bands(df)
    spectral_lib <- NULL
    if (type == "hsdar") {
        spectral_matrix <- as.matrix(df_no_metadata)

        spectral_lib <- hsdar::speclib(spectral_matrix, wavelength = bands)
    } else if (type == "spectrolab") {
        colnames(df_no_metadata) <- bands
        spectral_lib <- spectrolab::as_spectra(df_no_metadata)
    }

    return(spectral_lib)
}

#' Converts a spaectral library to a dataframe.  
#'
#' @return A dataframe
#' @param speclib: A spectral library to convert
#' @export
#'
speclib_to_df <- function(speclib) {
    df <- speclib %>%
        as.data.frame()

    return(df)
}
