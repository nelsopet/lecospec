source("Functions/lecospectR.R")

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

print(aggregated_results)
print(results)
