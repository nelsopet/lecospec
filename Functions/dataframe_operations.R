
#' Functions returns columns that are bandpasses
#' 
#' @inheritParams None
#' @return a dataframe without non-bandpass columns
#' @param x: a dataframe
#' @seealso remove_band_column
#' @export 
#' @examples Not Yet Implmented
remove_meta_column <- function(x) {
    meta <- c(
        grep(
            "^[0-9][0-9][0-9]",
            colnames(x),
            value = FALSE,
            invert = FALSE  
        ),
        grep(
            "^[X x][0-9][0-9][0-9]",
            colnames(x),
            value = FALSE,
            invert = FALSE
        )

        ) %>% 
        unique() %>%
        as.vector()

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
output_df <- cbind(veg_index, remove_meta_column(resampled_data))

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
impute_spectra <- function(
    x, 
    cluster = NULL, 
    method = "missForest", 
    ignore_cols = NULL,
    transpose=FALSE) {
    


    zero_variance_cols <- c()

    for(col in colnames(x)) {
        x_var <- var(x[, col])
        if((x_var == 0) || is.na(x_var) || is.nan(x_var) || is.null(x_var)) {
            append(zero_variance_cols, col)
        }
    }

    if(!is.null(ignore_cols)) {
        
        ignored_cols <- unique(c(ignore_cols, zero_variance_cols))
    } else {
        ignored_cols <- zero_variance_cols
    }

    used_cols <- setdiff(colnames(x), ignored_cols)

    df <- x[, used_cols] %>% as.data.frame()

    # convert to a data.frame if x is a raster
    if (!is.data.frame(x)) {
        df <- x %>%
        raster::rasterToPoints() %>%
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

    #print("Imputing...")
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

    return(
        cbind(
            output_data,
            x[, ignored_cols]
        )
    )
}#end impute_spectra


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


##### THIS IS A SPECTRAL OPERATION TAKING A DATA.FRAME AS AN INPUT
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
convert_and_save_output <- function(
    df, 
    aggregation_level,
    save_path = NULL,
    return_raster = TRUE,
    target_crs = NULL
){
    prediction <- convert_pft_codes(
        df,
        aggregation_level = aggregation_level,
        to = "int")

    #print(head(prediction))

    # convert the precition values to integers
    prediction$z <- prediction$z %>% round() %>% as.integer()

    #print(head(prediction))

    #print(paste0("Attempting to save to ", save_path))
    if(return_raster){
        prediction <- raster::rasterFromXYZ(prediction, digits=4)
        #print("Converted to Raster")

        # set to int datatype (unsigned int // 2 bytes)
        raster::dataType(prediction) <- "INT2U" 
        levels(prediction) <- get_attribute_table(aggregation_level)
        if(!is.null(target_crs)){
            raster::crs(prediction) <- target_crs
        }
        if(!is.null(save_path)){
                raster::writeRaster(
                    prediction,
                    filename = save_path,
                    datatype='INT2U',
                    overwrite = TRUE)
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
    #print("Data summary before filtering")
    #print(dim(df))
    drop_cols <- c("x","y")
    columns_to_check <- setdiff(colnames(df), drop_cols)

    df_no_empty_rows <- df %>% 
        naniar::replace_with_na_all(condition = ~.x == 0) %>%
        dplyr::filter_at(
            .vars = vars(one_of(columns_to_check)),
            ~!is.na(.)
        ) 
    #print("data summary after filtering")
    #print(dim(df_no_empty_rows))
    return(df_no_empty_rows)
}


drop_zero_rows <- function(df) {
    # sort the columsn to make sure x and y are first before calculating rowsums
    df_rowsums <- df %>% dplyr::relocate(y) %>% dplyr::relocate(x)
    num_cols = ncol(df)
    # return only the rows with rowsums over zero
    return( df_rowsums[rowSums(df_rowsums[,3:num_cols], na.rm = TRUE) > 0.0001, ])
}



#' Performs post-processing for the process_tile function
#'
#' Renames columns and adds the spatial information that is lost at model inference time.
#'
#' @return 
#' @param prediction_df: A data.frame of prediction from the model.  
#' It should have a single column of data.
#' @param base_df: the base data.frame for creating the prediction; 
#' it should have columns x and y specifying the spatial coordinates for that row.
#' @return a data.frame with three columns: 'x', 'y', and 'z'.  'x' and 'y' 
#' are the spatial location of the prediction in the original CRS, 
#' and the 'z' column is prediction. 
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


#' Extracts indices from data frame column names
#' 
#' This utility function parses column names to extract only the numberic wavelength
#' 
#' @inheritParams None
#' @return A character vector of wavelengths
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

#' resamples a data.frame of spectra
#'
#' Resamples a data.frame of spectral information by converting it to a spectral library and back.
#'
#' @param df: a data.frame of spectra with column names as bands (possibly pre-pended with 'X')
#' @param normalize: (defaul FALSE) whether or not the spectra should be normalized before resampling  
#' @param min_wavelength: 
#' @param max_wavelength: 
#' @param delta: the distance (in the same ) 
#' @param drop_existing (boolean, default FALSE).  If FALSE, the new columns will be added to the
#' data frame.  If TRUE, only the new data will be returned.
#' 
#' @return a data.frame of spectra
#'  
#'  
#' @seealso None
#' @export 
resample_df <- function(
    df,
    normalize = FALSE,
    min_wavelength = 402.593,
    max_wavelength = 995.716,
    delta = 5,
    drop_existing = FALSE
    ) {
    spec_library <- df_to_speclib(df, type="spectrolab")
    
    if(normalize){
        spec_library <- spectrolab::normalize(spec_library)
    }

    speclib_resampled <- spectrolab::resample(
        spec_library,
        seq(min_wavelength, max_wavelength, delta),
        parallel = FALSE
    )
    
    df_resampled <- speclib_to_df(speclib_resampled) %>%
        dplyr::select(-sample_name)

    colnames(df_resampled) <- paste0(
        "X",
        colnames(df_resampled),
        "_5nm"
    )

    if(drop_existing){
        return(df_resampled)
    }
    combined_df <- cbind(remove_band_column(df), df_resampled)
    return(combined_df)
}

#' scales a data.frame based on the largest value across all rows and columns
#'
#' Scaled all the the data to the interval [0,1] using the global (as opposed to columnwise)
#' minimum and maximum in the data.frame.  This transformation is not applied to columns 
#' specified in ignore_cols
#'
#' @param df: the data.frame
#' @param ignore_cols: NULL or a character vector specifying columns
#' which should not be considered ro transformed
#' @return a data.frame
#'  
#'  
#' @seealso None
#' @export 
global_min_max_scale <- function(df, ignore_cols = NULL){
    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(colnames(df), ignore_cols)
    }

    target_df <- df[,used_cols]

    global_min <- min(target_df)
    global_max <- max(target_df)

    scaled_df <- base::scale(
        target_df, 
        center = rep(global_min, ncol(target_df)),
        scale = rep((global_max - global_min), ncol(target_df))
    )

    rm(target_df)
    gc()

    return( cbind(df[, ignore_cols], scaled_df))
}





#' Filters a data frame (using the columns specified) to be between the values provided

#'
#' Clips a data.frame to be between the min_value and max_value by dropping rows
#'
#' @param df: a data.frame of input data
#' @param min_value: a number: the lowerest value that should remain in the data frame 
#' (can be -Inf)
#' @param max_vallue: the greatest number that should be allowed in the data.frame 
#' (can be +Inf)
#' @param ignore_cols: NULL or a character vector specifing columns that should not be 
#' used for filtering
#' @return a data.frame 
#'  
#'  
#' @seealso None
#' @export 
filter_all_between <- function(
    df, 
    min_value, 
    max_value,
    ignore_cols = NULL){

     used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(colnames(df), ignore_cols)
    }


        max_by_row <- apply(df[, used_cols], 1, base::max) %>% 
            as.data.frame()
        df_filtered <- df[max_by_row <= max_value, ]

        min_by_row <- apply(df_filtered[, used_cols], 1, base::min) %>% 
            as.data.frame()
        df_filtered <- df_filtered[min_by_row >= min_value, ]

        return(df_filtered)
}

#' performs rudamentory vector normalization
#'
#' long
#'
#' @param vec: a numeric vector
#' @return a numeric vector
#'  
#'  
#' @seealso None
#' @export 
normalize_vector <- function(vec){
    x <- vec[!is.na(vec)]

    min_x <- min(x)
    max_x <- max(x)

    if(min_x == max_x){
        stop("There is only one unique value in the supplied vector")
    }

    y <- vec
    y[!is.na(vec)] <- (x - min_x) / (max_x - min_x)

    return(y)
}

#' rescales a data.frame to have all columns to the interval [0,1]
#'
#' long
#'
#' @param df: a data.frame
#' @param ignore_cols: NULL or a character vector of column names to ignore in the tranformation.
#' @return a dataframe with rescaled columns
#'  
#'  
#' @seealso None
#' @export 
columnwise_min_max_scale <- function(df, ignore_cols = NULL){
    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(colnames(df), ignore_cols)
    }

    new_df <- apply(df[,used_cols], 2, normalize_vector) %>% as.data.frame()

    if(!is.null(ignore_cols)){
        new_df <- cbind(df[, as.character(ignore_cols)], new_df)
    }
    return(new_df)
}

#' short
#'
#' long
#'
#' @param df:.  
#' @param normalize:  
#' @return 
#'  
#'  
#' @seealso None
#' @export 
vector_normalize_df <- function(df, maintain_names = FALSE){
    band_cols <- c(
        grep(
            "^[0-9][0-9][0-9]",
            colnames(df),
            value = FALSE,
            invert = FALSE  
        ),
        grep(
            "^[X x][0-9][0-9][0-9]",
            colnames(df),
            value = FALSE,
            invert = FALSE
        )

        ) %>% 
        unique() %>%
        as.vector()

    band_df <- df[,band_cols]
    non_band_df <- df[, -band_cols]

    normalized_bands <- band_df %>% 
        df_to_speclib(., type="spectrolab") %>% 
        spectrolab::normalize() %>% 
        as.data.frame()

    if(maintain_names){
        colnames(normalized_bands) <- colnames(band_df)
    }
    
    #colnames(normalized_bands) <- colnames(band_df)

    return(cbind(
        non_band_df,
        normalized_bands
    ))
}

#' Converts +/- infinity to NA in a data.frame
#'
#' 
#'
#' @param df: A data.frame
#' @param ignore_cols: NULL or a character vector of column names specifying which columns should
#' be ignored in the analysis 
#' @return a data.frame
#'  
#'  
#' @seealso None
#' @export 
inf_to_na <- function(df){
    return(
        do.call(
            data.frame,                      # Replace Inf in data by NA
            lapply(
                df,
                function(x) {return(replace(x, is.infinite(x), NA))}))
    )
}

#' Locates the outliers in a data.frame
#'
#' Checks all columns of a data.frame and returns a boolean vector which is TRUE  at position i
#' if there is an outlier in the i-th row of the data frame (in one or more columns) and FALSE 
#' otherwise.  Outliers are detected using the IQR method.  
#'
#' @param df: a data.frame  
#' @param ignore_cols: NULL or a character vector of column names that should be ignored
#' in the analysis (defaults to NULL) 
#' @return a boolean vector specifying whether an outlier is present in teh corresponding 
#' row of the input data.frame df
#'  
#'  
#' @seealso None
#' @export 
detect_outliers_columnwise <- function(df, ignore_cols = NULL){
    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(used_cols, ignore_cols)
    }

    outliers <- rep(c(FALSE), times = nrow(df))
    
    for(column_name in used_cols){
        q1 <- stats::quantile(df[,column_name], 1/4, type = 4, na.rm=TRUE)
        q3 <- stats::quantile(df[,column_name], 3/4, type = 4, na.rm=TRUE)
        col_iqr <- (q3 - q1)

        upper_fence <- q3 + (1.5 * col_iqr)
        lower_fence <- q1 - (1.5 * col_iqr)

        new_outliers <- (df[,column_name] > upper_fence) | 
            (df[,column_name] < lower_fence)

        for(index in seq_along(outliers)){
            if(new_outliers[[index]]){
                outliers[[index]] <- TRUE
            }
        }
    }

    return(outliers)
}

#' Not Yet Implemented
#'
#' long
#'
#' @param df:.  
#' @param normalize:  
#' @return 
#'  
#'  
#' @seealso None
#' @export 
detect_outliers_by_distance <- function(df, ignore_cols = NULL){

}

#' replaces outliers in a data.frame with NA
#'
#' Outliers are detected using inter-quartile range method.
#'
#' @param df: the data frame
#' @param ignore_cols: columns that should be ignored (left unchanged) 
#' @return a data.frame with NAs where there were previously outliers
#'  
#'  
#' @seealso None
#' @export 
outliers_to_na <- function(df, ignore_cols = NULL){
    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(used_cols, ignore_cols)
    }

    new_df <- df
    
    for(column_name in used_cols){
        q1 <- stats::quantile(df[,column_name], 1/4, type = 4, na.rm=TRUE)
        q3 <- stats::quantile(df[,column_name], 3/4, type = 4, na.rm=TRUE)
        col_iqr <- q3 - q1

        upper_fence <- q3 + (1.5 * col_iqr)
        lower_fence <- q1 - (1.5 * col_iqr)

        new_outliers <- (df[,column_name] > upper_fence) | 
            (df[,column_name] < lower_fence)


        new_df[new_outliers, column_name] = NA

    }
    
    return(new_df)
}

#' scales each column to center using the column mean and the scales using the standard deviation.
#'
#' 
#'
#' @param df: a data.frame of the data that should be centered and scaled
#' @param ignore_cols: data columns that should be ignored in the calculation (defaults to NULL) 
#' @return a data.frame with the same column names, but the data (not in ignore_cols) centered and scaled
#'  
#'  
#' @seealso None
#' @export 
standardize_df <- function(df, ignore_cols = NULL){
    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(used_cols, ignore_cols)
    }

    scaled_df <- df[,used_cols]

    
    for(col in used_cols){
        col_centered <- scaled_df[,col] - mean(scaled_df[,col], na.rm = TRUE)
        col_c_sd <- sd(col_centered, na.rm = TRUE)
        if(!is.null(col_c_sd) & !is.na(col_c_sd)){
            scaled_df[,col] <- (col_centered / col_c_sd)
        } else {
            scaled_df[,col] <- col_centered
        }
    }


    if(!is.null(ignore_cols)){
        return(cbind(scaled_df, df[,intersect(colnames(df),ignore_cols)]))
    } else {
        return(scaled_df)
    }
}

#' scales each column to center using the column median and the scales using the inter-quartile range.
#'
#' 
#'
#' @param df: a data.frame of the data that should be centered and scaled
#' @param ignore_cols: data columns that should be ignored in the calculation (defaults to NULL) 
#' @return a data.frame with the same column names, but the data (not in ignore_cols) centered and scaled
#'  
#'  
#' @seealso None
#' @export 
columnwise_robust_scale <- function(df, ignore_cols = NULL){
    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(used_cols, ignore_cols)
    }

    scaled_df <- df %>% as.data.frame()

    for(col in used_cols){
        col_centered <- scaled_df[,col] - median(scaled_df[,col], na.rm = TRUE)
        col_c_iqr <- stats::IQR(col_centered, na.rm = TRUE)
        if(!is.null(col_c_iqr) & !is.na(col_c_iqr)){
            scaled_df[,col] <- (col_centered / col_c_iqr)
        } else {
            scaled_df[,col] <- col_centered
        }
    }

    return(scaled_df)
}

#' matches two dataframes based on the levels of a categorical variable
#'
#' 
#'
#' @param left_df: a data.frame
#' @param right_df: another data.frame
#' @param cols: a character vector of length two specifying the column in each 
#' dataframe that should be matched  
#' @return a list with two fields (left and right) that are the left and right
#'  dataframes (respectively) matched such that the ith row in each has the same 
#' value of the categorical variable in cols
#'  
#'  
#' @seealso None
#' @export 
create_matched_data <- function(left_df, right_df, cols = c("targets", "targets")){
    
    shared_levels <- intersect(
        levels(as.factor(left_df[, cols[1]])),
        levels(as.factor(right_df[, cols[[2]]]))
        )

    # store inermediate results in a list
    level_dfs_l <- NULL
    level_dfs_r <- NULL

    for(level in shared_levels){
        # filter only to the selected PFT
        filtered_left <- left_df[(left_df[,cols[[1]]] == level),]
        filtered_right <- right_df[right_df[,cols[[2]]] == level,]

        # get the number of records to include in the output
        num_records <- min(nrow(filtered_left), nrow(filtered_right))

        if(num_records > 0){
            # shuffle the data before selecting rows
            shuffle_left <- permute::shuffle(nrow(filtered_left))
            shuffle_right <- permute::shuffle(nrow(filtered_right))
            if(is.null(level_dfs_l)){
                level_dfs_l <- filtered_left[shuffle_left,][1:num_records,]
            } else {
                level_dfs_l <- rbind(
                    level_dfs_l, 
                    filtered_left[shuffle_left,][1:num_records,])
            }

            if(is.null(level_dfs_r)){
                level_dfs_r <- filtered_right[shuffle_right,][1:num_records,]
            } else {
                level_dfs_r <- rbind(
                    level_dfs_r, 
                    filtered_right[shuffle_right,][1:num_records,])
            }
        }
    }

    return(list(
        left = level_dfs_l,
        right = level_dfs_r
    ))


}


#' Imputes the NAs and outliers in the provided data.frame
#' 
#' This function imputes the NAs and the outliers in the dataset in two passes,
#' one for the NAs, then replaces the outliers with NA, then imputes again.  
#' Missforest is used for each imputation
#' 
#' @param df: the data.frame to transform
#' @param ignore_cols: data columns that should be ignored in the calculation (defaults to NULL) 
#' @return 
#' @export
#' 
#'
impute_outliers_and_na <- function(df, ignore_cols=NULL){

    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){

        used_cols <- setdiff(colnames(df), ignore_cols)
    }

    transformed_df <- df[, used_cols]
    transformed_df <- inf_to_na(transformed_df)
    transformed_df <- impute_spectra(transformed_df)
    transformed_df <- outliers_to_na(transformed_df)
    transformed_df <- impute_spectra(transformed_df)

    if(!is.null(ignore_cols)){
        cols_to_reattach <- setdiff(ignore_cols, colnames(df))
        transformed_df <- cbind(transformed_df, df[,cols_to_reattach])
    }

    return(transformed_df)
}

#' Clips outliers on the high and low ends
#' 
#' This function alters the data.frame to have have all outliers replaced 
#' with the upper or lower fence, meaning low outliers are replaces with 
#' $Q_1 - 1.5IQR $ and the high outliers are replaced with $Q_3 + 1.5IQR $
#' 
#' @param df, the data frame to clip
#' @param ignore_cols: data columns that should be ignored in the calculation (defaults to NULL) 
#' @return a data.frame with outliers replaced
#' @export
#' 
#'
clip_outliers <- function(df, ignore_cols=NULL){
    new_df <- df %>% as.data.frame()
    
    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(colnames(df), ignore_cols)
    }

    for(column_name in used_cols){
        if(is.numeric(df[,column_name])){

            q1 <- stats::quantile(df[,column_name], 1/4, type = 4, na.rm=TRUE)
            q3 <- stats::quantile(df[,column_name], 3/4, type = 4, na.rm=TRUE)
            col_iqr <- q3 - q1

            upper_fence <- q3 + (1.5 * col_iqr)
            lower_fence <- q1 - (1.5 * col_iqr)

            high_outliers <- df[,column_name] > upper_fence
            low_outliers <- df[,column_name] < lower_fence

            
            new_df[high_outliers, column_name] = upper_fence
            new_df[low_outliers, column_name] = lower_fence
        } else {
            warning(
                paste0(
                    "Skipping column ", 
                    column_name,
                    " as it is not numeric")
                    )
        }
    }

    return(new_df)
}

apply_transform <- function(df, transform_type, ignore_cols = NULL){
    if(is.function(transform_type)){
        return(transform_type(df, ignore_cols = ignore_cols))
    }

    if(is.null(transform_type)){
        return(df)
    }

    if(transform_type == "none"){
        return(df)
    } else if (transform_type == "minmax") {
        #print("Min-Max scaling data")
       return(columnwise_min_max_scale(df, ignore_cols = ignore_cols))
    } else if(transform_type == "standard"){
        #print("Standard scaling data")
        return(standardize_df(df, ignore_cols = ignore_cols))
    } else if(transform_type == "robust"){
        #print("Robust scaling data")
        return(columnwise_robust_scale(df, ignore_cols = ignore_cols))
    } else {
        warning("Invalid transform specified, skipping...")
        return(df)
    }
}

handle_outliers <- function(df, transform_type, ignore_cols = NULL){

    if(is.function(transform_type)){
        return(transform_type(df, ignore_cols = ignore_cols))
    }

    if(is.null(transform_type)){
        return(df)
    }

    if(transform_type == "clip"){
        return(
            clip_outliers(df, ignore_cols = ignore_cols)
            )
    } else if(transform_type == "none"){
        return(df)
    } else if(transform_type == "drop"){
        
        outlier_rows <- detect_outliers_columnwise(df, ignore_cols = ignore_cols)
        return(
            df[!outlier_rows,]
            )
    } else if(transform_type == "impute"){
        return(
            impute_outliers_and_na(df, ignore_cols = ignore_cols)
        )
    }
}


create_clip_transform <- function(df, ignore_cols = NULL ){
    new_df <- df %>% as.data.frame()
    cache_path <- "./assets/clip_data.json"
    
    used_cols <- colnames(df)
    if(!is.null(ignore_cols)){
        used_cols <- setdiff(colnames(df), ignore_cols)
    }

    fences <- list()

    for(column_name in used_cols){
        if(is.numeric(df[,column_name])){

            q1 <- stats::quantile(df[,column_name], 1/4, type = 4, na.rm=TRUE)
            q3 <- stats::quantile(df[,column_name], 3/4, type = 4, na.rm=TRUE)
            col_iqr <- q3 - q1

            upper_fence <- q3 + (1.5 * col_iqr)
            lower_fence <- q1 - (1.5 * col_iqr)
        }

        fences[column_name] <- list(
            upper = upper_fence,
            lower = lower_fence
        )
    }

    fence_json_str <- rjson::toJSON(fences)
    write(fence_json_str, file = cache_path)


    clip_transform <- function(
            df, 
            ignore_cols = NULL
        ){
            new_df <- df %>% as.data.frame()
            cache_path <- "./assets/clip_data.json"

            fences <- rjson::fromJSON(file=cache_path)

            used_cols <- colnames(df)
            if(!is.null(ignore_cols)){
                used_cols <- setdiff(
                    intersect(
                        names(fences),
                        colnames(df)
                    ), 
                    ignore_cols
                )
            }

            for(column_name in used_cols){
                upper_fence <- fences[column_name]$upper
                lower_fence <- fences[column_name]$lower

                high_outliers <- df[,column_name] > upper_fence
                low_outliers <- df[,column_name] < lower_fence

                new_df[high_outliers, column_name] = upper_fence
                new_df[low_outliers, column_name] = lower_fence
            }

            return(new_df)
        }

    return(
        clip_transform
    )
}

# extend the is.nan prototype function for data frames
is.nan.data.frame <- function(x){

    do.call(cbind, lapply(x, is.nan))
}


filter_df_bands <- function(df){
    bands_col_names <- read.csv("assets/band_cols.csv")$names %>% as.character()
    df_cols <- colnames(df) %>% as.character()
    target_cols <- intersect(df_cols, bands_col_names)
    return(df[,target_cols])
}

filter_df_indices <- function(df){
    #index_col_names <- read.csv("assets/vegIndicesUsed.csv")$x %>% as.character()
    df_cols <- colnames(df) %>% as.character()
    target_cols<-df_cols[grepl("_5nm",df_cols)]
    #target_cols <- setdiff(df_cols, index_col_names)
    return(df[,target_cols])
}


bin_df <- function(df, num_bins = 10){
    binned_df <- impute_spectra(as.data.frame(df))

    for(col in colnames(df)){
        binned_df[,col] <- as.numeric(ntile(df[,col], n = num_bins))
    }

    print(summary(binned_df))

    return(binned_df)

}
