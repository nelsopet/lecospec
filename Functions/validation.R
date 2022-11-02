
get_prediction_distribution <- function(prediction_df){
    num_observations <- nrow(prediction_df)
    #print(colnames(prediction_df))
    df <- prediction_df %>% dplyr::group_by(z) %>% tally() %>% as.data.frame()
    #df$key <- unique(prediction_vec) %>% as.vector()
    df$distribution <- df$n / num_observations
    return(df)
}

get_prediction_distribution <- function(prediction_df){
    num_observations <- nrow(prediction_df)
    #print(colnames(prediction_df))
    df <- prediction_df %>% dplyr::group_by(z) %>% tally() %>% as.data.frame()
    #df$key <- unique(prediction_vec) %>% as.vector()
    df$distribution <- df$n / num_observations
    return(df)
}


validate_results <- function(
    prediction_ras, 
    quadrat_shapefile, 
    validation_table,
    pft_key,
    template_path,
    aggregation_level = 1,
    save_path = "./quadrat_"
 ){
    # Need to make keys shared and unique.  Really need the R equivalent of spark's RDD.reduceByKey()
    # store the results
    results <- list()
       
    for(i in seq(nrow(quadrat_shapefile))){
        # load the template 
        template <- read.csv(file = template_path, row.names = 1)

        # crop the raster to the quadrat
        quadrat_shape <- quadrat_shapefile[i,]
        print(quadrat_shape)
        quadrat_ras <- raster::crop(prediction_ras, quadrat_shape)

        plot_options <- define_plot_options(
            title = paste0("Quadrat ", i, " Predictions"),
            xLabel = "Longitude",
            yLabel = "Latitude"
        )

    

        #png(paste0("./test_hist_", i, ".png"))
        #hist(quadrat_ras)
        #dev.off()
        plot_categorical_raster(quadrat_ras, plot_options = plot_options)

        #windows();
        
        ggsave(
            paste0(save_path, i, "_plot.png"),
            plot = plot_categorical_raster(
                quadrat_ras,
                colors = fg1_palette,
                plot_options = plot_options
            )
        )
        
        quadrat_df <- raster::rasterToPoints(quadrat_ras) %>% as.data.frame()

        #print(quadrat_df %>% group_by(z) %>% tally())


        # prediction 
        predictions <- convert_pft_codes(quadrat_df, aggregation_level = aggregation_level, to="string")
        #print(predictions %>% group_by(z) %>% tally())


        # extract the validation data for this quadrat
        quadrat_validation_df <- get_prediction_distribution(predictions)

        #print("Quadrat Names")
        #print(quadrat_shape$CLASS_NAME)
        #print("Keys in validation")
        #print(validation_table$UID %>% unique())
        filtered_validation_df <- validation_table[validation_table$UID == quadrat_shape$CLASS_NAME, ]
        #print(head(filtered_validation_df))


        #print(head(filtered_validation_df))
        filtered_validation_df$Plant <- change_aggregation(
            filtered_validation_df$Plant,
            aggregation_level = aggregation_level,
            pft_key
        ) %>% as.vector()



    


        #print(head(filtered_validation_df, 20L))
        # aggregate the predictions and validation using the template
        aggregated_results <- aggregate_result_template(
            quadrat_validation_df,
            filtered_validation_df,
            template)

        #print(aggregated_results)
        
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
    #print("Number of Validation Rows:")
    #print(num_rows_validation)
    num_observations <- sum(df$n)
    if(num_rows_df == 0){
        warning(
            "Input (Prediction) Data is empty"
        )
    }

    
    # copy the template
    template <- data.frame(input_template)
    if(num_rows_validation == 0){
        warning(
            "Input (Validation) Data is empty"
        )
    }
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
            #print(validation_df[[val_row_idx, "Plant"]])
            if(template[[template_row_idx, "key"]] == validation_df[[val_row_idx, "Plant"]]){
                # store values
                current_count <- template[[template_row_idx, "validation_counts"]]
                current_prop <- template[[template_row_idx, "validation_prop"]]
                validation_raw <- validation_df[[val_row_idx, "cover_prn"]]
                # convert NAs to 0's
                #print(validation_raw)
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




# merge the validation templates 
merge_validation_dfs <- function(df1, df2){
    output_df <- data.frame(df1)

    output_df$validation_counts <- df1$validation_counts + df2$validation_counts
    output_df$predicted_counts <- df1$predicted_counts + df2$predicted_counts

    num_observations_val <- sum(output_df$validation_counts)
    num_observations_pred <- sum(output_df$predicted_counts)

    output_df$validation_prop <- output_df$validation_counts / num_observations_val
    output_df$prediction_prop <- output_df$predicted_counts / num_observations_pred

    
    return(output_df)
}

coalesce_results <- function(df_list, aggregator_df){
    output_df <- aggregator_df
    
    # merge all the counts
    for(i in seq_along(df_list)){
        output_df <- merge_validation_dfs(output_df, df_list[[i]])
    }

    for(j in seq_len(nrow(output_df))){

    }

    return(output_df)
}

# save the validation df to disk
save_validation <- function(template_dfs, base_filename = "validation"){
    for(i in seq_along(template_dfs)){
        write.csv(
            template_dfs[[i]],
            paste0(base_filename, "_", i, ".csv"))
    }
}


validate_model <- function(ml_model, save_directory, normalize_input = TRUE, scale_input = FALSE, cluster = NULL){
    source("Scripts/validation_defs.R")


        for( i in seq_along(quadrats)){
        # process the tile
        tile_results <- process_tile(
            quadrats[[i]],
            ml_model,
            1,
            cluster = cluster,
            return_raster = TRUE,
            band_names = band_names,
            normalize_input = normalize_input,
            scale_input = scale_input,
            save_path = "./validation_saved_output.grd",
            suppress_output = FALSE)

        # load shapefile and project to match
        shape <- sf::st_read(shapes[[i]])
        print(shape_names[[i]])
        shape$CLASS_NAME <- shape_names[[i]]
        projected_shapes <- sf::st_transform(shape, raster::crs(tile_results))

        # Validation data
        #validation_df <- read.csv(validation_paths[[i]], na.strings=c("NA", "n/a"))

        # run the validation
        validation_aggregates <- validate_results(
            tile_results,
            projected_shapes,
            validation_df,
            rjson::fromJSON(file = "./assets/pft_adj_list.json"),
            "./assets/pft1_template.csv",
            aggregation = 1, 
            save_path = paste0(save_directory, "site_", i, "_quadrat_")
        )

        print(names(tile_results))

        # bar plots
        for(j in seq_along(validation_aggregates)){
            plot_prop_test <- plot_quadrat_proportions(
                validation_aggregates[[j]],
                filter_missing = TRUE)

                #windows();plot_prop_test

            ggsave(
                paste0(
                    save_directory,
                    "site_",
                    i,
                    "quadrat_",
                    j,
                    "_bar.png"),
                device = png)

            write.csv(
                validation_aggregates[[j]], 
                paste0(
                    save_directory,
                    "validation_site_",
                    i, 
                    "quadrat_",
                    j,
                    ".csv"
            ))
        }
    } 
}


aggregate_results <- function(
    directory,
    template = "assets/pft1_template.json", 
    aggregation_level = 1){

    files <- list.files(
        directory,
        pattern = "*.csv",
        full.names = TRUE,
    )

    
    # load & merge data
    loaded_validation <- purrr::map(files, load_and_label_data)
    
    return(Reduce(rbind, loaded_validation))
}

site_indices <- c(
    "BisonGulch",
    "Chatanika",
    "TwelveMile",
    "TwelveMile",
    "EightMile",
    "MurphyDome",
    "MurphyDome",
    "MurphyDome"
)

parse_path <- function(path) {
    split_path <- strsplit(path, split = "/", fixed = TRUE)
    if(stringr::str_detect(path, "experiments")[1]){
        if(stringr::str_detect(path, "site_1")[1]){
            return("BisonGulch")
        } else if(stringr::str_detect(path, "site_2")[1]){
            return("Chatanika")
        } else if(stringr::str_detect(path, "site_3")[1]){
            return("TwelveMile")
        } else if(stringr::str_detect(path, "site_4")[1]){
            return("TwelveMile")
        } else if(stringr::str_detect(path, "site_5")[1]){
            return("EightMile")
        } else if(stringr::str_detect(path, "site_6")[1]){
            return("MurphyDome")
        } else if(stringr::str_detect(path, "site_7")[1]){
            return("MurphyDome")
        } else if(stringr::str_detect(path, "site_8")[1]){
            return("MurphyDome")
    } else {
        return(split_path[[1]][[2]])
    }
    }
}

# print(parse_path("figures/twelveMile2/tm2_validation_4.csv"))

load_and_label_data <- function(path) {
    label <- parse_path(path)
    df <- read.csv(path, header = TRUE) %>% as.data.frame()
    df$site <- label

    return(df %>% as.data.frame())
}

