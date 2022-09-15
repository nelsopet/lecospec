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
    "mle/models/no_norm_no_noise_prior.rda",
    "mle/models/no_norm_noise_post.rda",
    "mle/models/no_norm_noise_no_weight.rda",
    "mle/models/no_norm_noise_prior.rda",
    "mle/models/no_norm_no_noise_post.rda",
    "mle/models/no_norm_no_noise_no_weight.rda",
    "mle/models/no_norm_no_noise_prior.rda"

)

exp_save_paths <- c(
    "mle/experiments/ground_trained/normed_noised_val_weighted/",
    "mle/experiments/ground_trained/normed_noised_unweighted/",
    "mle/experiments/ground_trained/normed_noised_weighted/",
    "mle/experiments/ground_trained/unnormed_noised_val_weighted/",
    "mle/experiments/ground_trained/unnormed_noised_unweighted/",
    "mle/experiments/ground_trained/unnormed_noised_weighted/",
    "mle/experiments/ground_trained/normed_unnoised_val_weighted/",
    "mle/experiments/ground_trained/normed_unnoised_unweighted/",
    "mle/experiments/ground_trained/normed_unnoised_weighted/",
    "mle/experiments/ground_trained/unnormed_unnoised_val_weighted/",
    "mle/experiments/ground_trained/unnormed_unnoised_unweighted/",
    "mle/experiments/ground_trained/unnormed_unnoised_weighted/",
    "mle/experiments/ground_trained/totally_unnormed_noised_unweighted",
    "mle/experiments/ground_trained/totally_unnormed_noised_val_weighted",
    "mle/experiments/ground_trained/totally_unnormed_noised_weighted",
    "mle/experiments/ground_trained/totally_unnormed_unnoised_unweighted",
    "mle/experiments/ground_trained/totally_unnormed_unnoised_val_weighted",
    "mle/experiments/ground_trained/totally_unnormed_unnoised_weighted"
)

normalization_scheme <- c(
    TRUE,
    TRUE, 
    TRUE,
    TRUE, 
    TRUE, 
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE, 
    FALSE,
    FALSE,
    FALSE, 
    FALSE,
    FALSE,
    FALSE, 
    FALSE
)

print(length(model_paths))
print(length(exp_save_paths))
print(length(normalization_scheme))

for(index in 13:length(exp_save_paths)){
    print(paste0("Processing Model ", index))
    model <- load_model(model_paths[[index]])
    save_path <- exp_save_paths[[index]]
    validate_model(
        model, 
        save_path, 
        normalize_input = normalization_scheme[[index]])
}
