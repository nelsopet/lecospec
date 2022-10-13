source("Functions/lecospectR.R")


# set all the paths for saving the data, etc.
model_paths <- c(
    "mle/models/norm_no_noise_no_weight_exp.rda",
    "mle/models/norm_no_noise_post_exp.rda",
    "mle/models/norm_no_noise_prior_exp.rda",
    "mle/models/no_norm_no_noise_no_weight_exp.rda",
    "mle/models/no_norm_no_noise_post_exp.rda",
    "mle/models/no_norm_no_noise_prior_exp.rda",
    "mle/models/scaled_clean_no_weight_exp.rda",
    "mle/models/scaled_clean_post_exp.rda",
    "mle/models/scaled_clean_prior_exp.rda",
    "mle/models/norm_noise_no_weight_exp.rda",
    "mle/models/norm_noise_post_exp.rda",
    "mle/models/norm_noise_prior_exp.rda",
    "mle/models/no_norm_noise_no_weight_exp.rda",
    "mle/models/no_norm_noise_post_exp.rda",
    "mle/models/no_norm_noise_prior_exp.rda",
    "mle/models/scaled_noise_no_weight_exp.rda",
    "mle/models/scaled_noise_post_exp.rda",
    "mle/models/scaled_noise_prior_exp.rda"
)

scaled_ground_paths <- c(
    "mle/models/scaled_clean_post_exp.rda",
    "mle/models/scaled_clean_prior_exp.rda",
    "mle/models/scaled_clean_no_weight_exp.rda",
    "mle/models/scaled_noise_post_exp.rda",
    "mle/models/scaled_noise_prior_exp.rda",
    "mle/models/scaled_noise_no_weight_exp.rda"
)

scaled_save_paths <- c(
    "mle/experiments/ground_trained/scaled_noised_val_weighted/",
    "mle/experiments/ground_trained/scaled_noised_weighted/",
    "mle/experiments/ground_trained/scaled_noised_unweighted/",
    "mle/experiments/ground_trained/scaled_unnoised_val_weighted/",
    "mle/experiments/ground_trained/scaled_unnoised_weighted/",
    "mle/experiments/ground_trained/scaled_unnoised_unweighted/"
)

experiment_save_paths <- c(
    "mle/experiments/ground_expanded/normed_unnoised_unweighted/",
    "mle/experiments/ground_expanded/normed_unnoised_val_weighted/",
    "mle/experiments/ground_expanded/normed_unnoised_weighted/",
    "mle/experiments/ground_expanded/unnormed_unnoised_unweighted/",
    "mle/experiments/ground_expanded/unnormed_unnoised_val_weighted/",
    "mle/experiments/ground_expanded/unnormed_unnoised_weighted/",
    "mle/experiments/ground_expanded/scaled_no_noise_unweighted/",
    "mle/experiments/ground_expanded/scaled_no_noise_val_weighted/",
    "mle/experiments/ground_expanded/scaled_no_noise_weighted/",
    "mle/experiments/ground_expanded/normed_noised_unweighted/",
    "mle/experiments/ground_expanded/normed_noised_val_weighted/",
    "mle/experiments/ground_expanded/normed_noised_weighted/",
    "mle/experiments/ground_expanded/unnormed_noised_unweighted/",
    "mle/experiments/ground_expanded/unnormed_noised_val_weighted/",
    "mle/experiments/ground_expanded/unnormed_noised_weighted/",
    "mle/experiments/ground_expanded/scaled_noise_unweighted/",
    "mle/experiments/ground_expanded/scaled_noise_weighted/",
    "mle/experiments/ground_expanded/scaled_noised_val_weighted/"
)

normalize_flags <- c(
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
)

scale_flags <- c(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,    
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,    
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE
)


# run the experiments

for(model_idx in seq_along(model_paths)){
    model <- load_model(model_paths[[model_idx]])
    results <- validate_model(
        model, 
        experiment_save_paths[[model_idx]],
        normalize_input = normalize_flags[[model_idx]],
        scale_input = scale_flags[[model_idx]]
    )
}

print(model$forest$independent.variable.names)
print(model_idx)
