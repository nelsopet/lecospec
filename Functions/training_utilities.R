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


add_noise_to_df <- function(df, noise_power = 0.01){}


