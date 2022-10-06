source("Functions/lecospectR.R")


# set all the paths for saving the data, etc.
model_paths <- c(
    "mle/models/norm_img_exp.rda",
    "mle/models/norm_img_pw_exp.rda",
    "mle/models/norm_img_w_exp.rda",
    "mle/models/base_img_exp.rda",
    "mle/models/base_img_pw_exp.rda",
    "mle/models/base_img_w_exp.rda",
    "mle/models/scale_img_exp.rda",
    "mle/models/scale_img_pw_exp.rda",
    "mle/models/scale_img_w_exp.rda",
    "mle/models/norm_noise_img_exp.rda",
    "mle/models/norm_noise_img_pw_exp.rda",
    "mle/models/norm_noise_img_w_exp.rda",
    "mle/models/base_noise_img_exp.rda",
    "mle/models/base_noise_img_pw_exp.rda",
    "mle/models/base_noise_img_w_exp.rda",
    "mle/models/scale_noise_img_exp.rda",
    "mle/models/scale_noise_img_pw_exp.rda",
    "mle/models/scale_noise_img_w_exp.rda"
)

experiment_save_paths <- c(
    "mle/experiments/uav_expanded/normed_unnoised_unweighted/",
    "mle/experiments/uav_expanded/normed_unnoised_val_weighted/",
    "mle/experiments/uav_expanded/normed_unnoised_weighted/",
    "mle/experiments/uav_expanded/unnormed_unnoised_unweighted/",
    "mle/experiments/uav_expanded/unnormed_unnoised_val_weighted/",
    "mle/experiments/uav_expanded/unnormed_unnoised_weighted/",
    "mle/experiments/uav_expanded/scaled_no_noise_unweighted/",
    "mle/experiments/uav_expanded/scaled_no_noise_val_weighted/",
    "mle/experiments/uav_expanded/scaled_no_noise_weighted/",
    "mle/experiments/uav_expanded/normed_noised_unweighted/",
    "mle/experiments/uav_expanded/normed_noised_val_weighted/",
    "mle/experiments/uav_expanded/normed_noised_weighted/",
    "mle/experiments/uav_expanded/unnormed_noised_unweighted/",
    "mle/experiments/uav_expanded/unnormed_noised_val_weighted/",
    "mle/experiments/uav_expanded/unnormed_noised_weighted/",
    "mle/experiments/uav_expanded/scaled_noise_unweighted/",
    "mle/experiments/uav_expanded/scaled_noise_weighted/",
    "mle/experiments/uav_expanded/scaled_noised_val_weighted/"
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
for(model_idx in 12:length(model_paths)){
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
