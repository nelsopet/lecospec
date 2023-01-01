
#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
add_noise <- function(df, noise_power, used_cols = NULL){
    output_df <- df
    total_observations <- nrow(df)
    for (col_index in seq_along(used_cols)) {
        active_column <- used_cols[col_index]
        output_df[, active_column] <- output_df[, active_column] +
            rnorm(total_observations, 0, noise_power)
    }
    return(output_df)
}


#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
check_output_distribution <- function(df, model, targets){
    predictions <- apply_model(df, model)$. %>% as.factor()
    correct_observations <- 0
    print(head(predictions))
    print(head(targets))

    for(i in seq_along(predictions)){
        if( predictions[[i]] == targets[[i]]){
            correct_observations <- correct_observations + 1
        }
    }

    class_counts <- predictions  %>% table()

    return( 
        list(
            accuracy = correct_observations / nrow(df),
            counts = class_counts %>% as.list()
        ))
}

automated_check <- function(input_df, ignore_cols = NULL){
    used_cols <- colnames(input_df)
    if(!is.null(ignore_cols)){

        used_cols <- setdiff(colnames(input_df), ignore_cols)
    }

    df <- input_df[, used_cols]

    indx <- apply(df, 2, function(x) any(is.na(x) | is.infinite(x)))
    if(length(indx > 0)){
        print("NAs and/or infinite values detected in data")
    }

    column_max_values <- apply(df, 2, function(x) base::max(x, na.rm = TRUE))
    column_min_values <- apply(df, 2, function(x) base::min(x, na.rm = TRUE))
    column_median_values <- apply(df, 2, function(x) stats::median(x, na.rm = TRUE))
    column_mean_values <- apply(df, 2, function(x) base::mean(x, na.rm = TRUE))
    column_variances <- apply(df, 2, function(x) var(x, na.rm = TRUE))
    
    print(paste0("Global minimum: ", min(column_min_values)))
    print(paste0("Global maximum: ", max(column_max_values)))
    print(paste0("Global Mean: ", mean(column_mean_values)))
    print(paste0("Global Estimated Variance: ", mean(column_variances)))
    print(paste0("Global Minimum Variance: ", min(column_variances)))
    print(paste0("Global Maximum Variance: ", max(column_variances)))

    return( list(
        maxs = column_max_values,
        mins = column_min_values,
        means = column_mean_values,
        medians = column_median_values,
        vars = column_variances
    ))
}

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
targets_to_weights <- function(target_factors){
    weights_by_pft <- target_factors %>% 
        table() %>% 
        purrr::map(., function(x) return(abs(1/x))) %>% 
        as.list() 
    weights_by_pft$Unknown <- 0# should not include data with no label

    return(purrr::map(
        target_factors %>% as.character(),
        function(x){
            return(weights_by_pft[[x]])
        }   
    ) %>% 
    as.numeric()
    )



}

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
test_transferrability <- function(df1, df2, ignore_cols = NULL){
    used_cols <- intersect(colnames(df1), colnames(df2))
        if(!is.null(ignore_cols)){
        used_cols <- setdiff(used_cols, ignore_cols)
    }

    results <- list()

    for(col in used_cols){
        if(is.numeric(df1[,col]) & is.numeric(df2[,col])){
            test_result <- stats::ks.test(
                df1[,col],
                df2[,col]
            )
        }

        results[[col]] <- test_result
    }
    return(results)
}


create_grid_search_registrator <- function(
    base_path
) {
    
}


calculate_posterior_weights <- function(validation_path ="figures/merged_validation_s.csv" ){

    validation_df <- read.csv(validation_path, header = TRUE)
    print(head(validation_df))

    total_observations <- sum(validation_df$validation_counts)
    print(total_observations)
    weights <- (1/ validation_df$validation_prop)
    print(validation_df$validation_prop)

    total_by_fg1 <- aggregate(
        x = validation_df$validation_counts,
        by = list(validation_df$key),
        FUN = sum
    )
    print(total_by_fg1)
}

get_posterior_weights_from_targets <- function(target_factor, posterior_weight = calculate_posterior_weights()){
    unbiased_weights <- targets_to_weights(target_vec)

    target_name_char <- target_factor %>% as.character()

    output_weights <- seq_along(target_factor)

    for(i in seq_along(target_vec)){
        if(posterior_weight[target_name_char[[i]]] > 0){
            fg1_weight <- 1 / posterior_weight[target_name_char[[i]]]
        } else {
            fg1_weight <- 0
        }
        output_weights[[i]] <- unbiased_weights[[i]] * fg1_weight
    }

    return(output_weights)
}