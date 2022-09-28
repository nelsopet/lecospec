source("Functions/lecospectR.R")

experiment_save_paths <- c(
    "mle/experiments/uav_trained/normed_unnoised_unweighted/",
    "mle/experiments/uav_trained/normed_unnoised_val_weighted/",
    "mle/experiments/uav_trained/normed_unnoised_weighted/",
    "mle/experiments/uav_trained/unnormed_unnoised_unweighted/",
    "mle/experiments/uav_trained/unnormed_unnoised_val_weighted/",
    "mle/experiments/uav_trained/unnormed_unnoised_weighted/",
    "mle/experiments/uav_trained/scaled_no_noise_unweighted/",
    "mle/experiments/uav_trained/scaled_no_noise_val_weighted/",
    "mle/experiments/uav_trained/scaled_no_noise_weighted/",
    "mle/experiments/uav_trained/normed_noised_unweighted/",
    "mle/experiments/uav_trained/normed_noised_val_weighted/",
    "mle/experiments/uav_trained/normed_noised_weighted/",
    "mle/experiments/uav_trained/unnormed_noised_unweighted/",
    "mle/experiments/uav_trained/unnormed_noised_val_weighted/",
    "mle/experiments/uav_trained/unnormed_noised_weighted/",
    "mle/experiments/uav_trained/scaled_noise_unweighted/",
    "mle/experiments/uav_trained/scaled_noise_weighted/",
    "mle/experiments/uav_trained/scaled_noised_val_weighted/"
)

results <- c(
    dir("mle/experiments/uav_trained/", full.names = TRUE),
    dir("mle/experiments/ground_trained/", full.names = TRUE)
)


for(folder in results){
    aggregated_results <- aggregate_results(folder)
    save_path <- paste0(folder, "/aggregates.html" )
    plot_by_pft(
        aggregated_results,
        save_path = save_path,
        open = FALSE
    )
}