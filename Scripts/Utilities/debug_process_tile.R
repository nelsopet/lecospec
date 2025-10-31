



    #tile_filename 
    #ml_model  
    #aggregation 
    #cluster = NULL
    #return_raster = TRUE
    #band_names=NULL
    #bandwidth = 5
    #outlier_processing = "none"
    #transform_type = "none"
    #return_filename = FALSE
    #save_path = NULL
    #suppress_output = FALSE
    #raster_datatype = "INT2U"
    

    i=1
    tile_filename = tile_filenames[[1]]
    ml_model = model
    aggregation = config$aggregation
    cluster = cl
    return_raster = TRUE
    band_names = bandnaimz
    bandwidth = config$bandwidth
    outlier_processing = outlier_processing_cfg
    transform_type = transform_type_cfg
    return_filename = TRUE
    save_path = prediction_filenames[[i]]
    suppress_output = TRUE
    raster_datatype = raster_datatype

    
    set.seed(61718)
    raster_obj <- raster::brick(tile_filename)
    input_crs <- raster::crs(raster_obj)
    print(paste0("preprocessing raster at ", tile_filename))
    base_df <- preprocess_raster_to_df(
        raster_obj,
        ml_model,
        band_names = bandnaimz)
    #print("preprocessing raster")

    #if(nrow(base_df) < 2) {
    #    handle_empty_tile(
    #        raster_obj,
    #        save_path = save_path,
    #        target_crs = input_crs,
    #        raster_datatype = raster_datatype)
    #
    #   if(!suppress_output){
    #       if(return_raster){
    #           return(raster_obj)
    #      } else {
    #           return(base_df)
    #        } 

    #    } 
    #    return(unlist(save_path))
        # add return value if output is suppressed
    #} else {
        # this runs if and only if there is sufficient data
    

            #if there is no data, return the empty tile in the specified format

        rm(raster_obj)
        gc()
        #print(colnames(base_df))
        cleaned_df <- drop_zero_rows(base_df)
        rm(base_df)
        gc()

        cleaned_df_no_empty_cols <- drop_empty_columns(cleaned_df) 
        #print(summary(cleaned_df_no_empty_cols))
        veg_indices <- get_vegetation_indices(
            cleaned_df_no_empty_cols,
            NULL,
            cluster = cluster)

        #try(
            rm(cleaned_df)
        #)# sometimes garbage collection gets there first, which is fine
        gc()

        # drop rows that are uniformly zero
      
        resampled_df <- resample_df(
            cleaned_df_no_empty_cols,
            normalize = FALSE,
            delta = bandwidth,
            #max_wavelength = 995.716,
            drop_existing=TRUE)
        gc()

        


        df_full <- as.data.frame(
            cbind(
                subset(cleaned_df_no_empty_cols, select = c("x", "y")),
                resampled_df,
                veg_indices
            )
        )
        
        print(class(df_full))

        imputed_df <- impute_spectra(
            df_full,
            method = "median",
            cluster = cluster)
 
        # above line should not be needed, testing then deleting
        rm(veg_indices)
        rm(resampled_df)
        rm(cleaned_df_no_empty_cols)
        gc()

        if(!is.function(outlier_processing)){
            print(paste0("Handling Outliers with method: ", outlier_processing))
        } else {
            print("Handling Outliers with User supplied function")
        }
        df_no_outliers <- handle_outliers(
            imputed_df,
            outlier_processing,
            ignore_cols = c("x", "y")
        )
        rm(df_full)

        # replace Inf and NaN values with NA (to be imputed later)
        df_no_outliers <- inf_to_na(df_no_outliers)
        df_no_outliers[is.nan(df_no_outliers)] <- NA

        if(!is.function(outlier_processing)) {
            print(
                paste0(
                    "Transforming the data with transform: ",
                    transform_type
                )
            )
        } else {
            print("Transforming Data with user supplied functions")
        }
        df_preprocessed <- apply_transform(
            df_no_outliers,
            transform_type,
            ignore_cols = c("x", "y")
        )
        rm(df_no_outliers)
        gc()

        
        #print(summary(df_preprocessed))
        imputed_df_2 <- impute_spectra(
                inf_to_na(df_preprocessed),
                method="median")
        #print(summary(imputed_df_2))

        prediction <- apply_model(
            imputed_df_2,
            ml_model)
        

        prediction <- postprocess_prediction(prediction, df_preprocessed)
        rm(df_preprocessed)
        gc()



        prediction <- convert_and_save_output(
            prediction,
            aggregation,
            save_path = save_path,
            return_raster = return_raster,
            target_crs = input_crs,
            raster_datatype = raster_datatype)

        
        raster::crs(prediction) <- input_crs

        if(suppress_output){
            #print(save_path)
            return(unlist(save_path))
        }
        return(prediction)
    }
}