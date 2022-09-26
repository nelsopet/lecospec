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

