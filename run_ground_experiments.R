source("Functions/lecospectR.R")

model_paths <- c(
    "mle/models/norm_noise_post.rda",
    "mle/models/norm_noise_no_weight.rda",
    "mle/models/norm_noise_prior.rda",
    "mle/models/no_norm_noise_post.rda",
    "mle/models/no_norm_noise_no_weight.rda",
    "mle/models/no_norm_noise_prior.rda",
    "mle/models/norm_no_noise_post.rda",
    "mle/models/norm_no_noise_no_weight.rda",
    "mle/models/norm_no_noise_prior.rda",
    "mle/models/no_norm_no_noise_post.rda",
    "mle/models/no_norm_no_noise_no_weight.rda",
    "mle/models/no_norm_no_noise_prior.rda"
)

save_paths <- c(
    "mle/experiments/ground_trained/normed_noised_val_weighted/",
    "mle/experiments/ground_trained/normed_noised_unweighted/",
    "mle/experiments/ground_trained/normed_noised_weighted/",
    "mle/experiments/ground_trained/unnormed_noised_val_weighted/",
    "mle/experiments/ground_trained/unnormed_noised_unweighted/",
    "mle/experiments/ground_trained/unnormed_noised_weighted/",
    "mle/experiments/ground_trained/unnormed_noised_val_weighted/",
    "mle/experiments/ground_trained/normed_unnoised_val_weighted/",
    "mle/experiments/ground_trained/normed_unnoised_unweighted/",
    "mle/experiments/ground_trained/normed_unnoised_weighted/",
    "mle/experiments/ground_trained/unnormed_unnoised_val_weighted/",
    "mle/experiments/ground_trained/unnormed_unnoised_unweighted/",
    "mle/experiments/ground_trained/unnormed_unnoised_weighted/"
)

for(i in seq_along(model_paths)){
    model <- load_model(model_paths[[i]])
    save_path <- save_paths[[i]]
    validate_model(model, save_path)
}
