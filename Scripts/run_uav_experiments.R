source("Functions/lecospectR.R")


# set all the paths for saving the data, etc.
model_paths <- c(
    "mle/models/norm_img.rda",
    "mle/models/norm_img_pw.rda",
    "mle/models/norm_img_w.rda",
    "mle/models/base_img.rda",
    "mle/models/base_img_pw.rda",
    "mle/models/base_img_w.rda",
    "mle/models/scale_img.rda",
    "mle/models/scale_img_pw.rda",
    "mle/models/scale_img_w.rda",
    "mle/models/norm_noise_img.rda",
    "mle/models/norm_noise_img_pw.rda",
    "mle/models/norm_noise_img_w.rda",
    "mle/models/base_noise_img.rda",
    "mle/models/base_noise_img_pw.rda",
    "mle/models/base_noise_img_w.rda",
    "mle/models/scale_noise_img.rda",
    "mle/models/scale_noise_img_pw.rda",
    "mle/models/scale_noise_img_w.rda"
)

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
        normalize_flags[[model_idx]]
    )
}