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


test_transferrability <- function(df1, df2, ignore_cols = NULL){
    used_cols <- intersect(colnames(df1), colnames(df2))
        if(!is.null(ignore_cols)){

        used_cols <- setdiff(used_cols, ignore_cols)
    }

    results <- list()

    for(col in used_cols){
        test_result <- stats::ks.test(
            df1[,col],
            df2[,col]
        )

        results[[col]] <- test_result
    }
    return(results)
}
