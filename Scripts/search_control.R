
base_paths <- c(
    "corrected_and_img.csv",
    "img_raw_raw.csv",
    "grd_raw_raw.csv"
    #"bison_gulch_stratified.csv",
    #"grd_raw_corrected.csv",
    #"img_indices_only.csv",# include veg indices
    #"grd_indices_only.csv"
)


outlier_functions <- list(
    #clip = load_model("./mle/clip_transform.rda"),
    no_treatment = function(x, ignore_cols = NULL){return(x)}# no transform
)

outlier_treatments <- c(
    "no_treatment"#,
    #"clip"
)

preprocess_functions <- list(
    no_treatment = function(x, ignore_cols = NULL){return(x)},# no transform
    min_max = columnwise_min_max_scale,
    robust = columnwise_robust_scale,
    standard = standardize_df
)

weight_functions <- list(
    posterior = get_posterior_weights_from_targets,
    balanced = targets_to_weights,
    no_treatment = function(x){return(NULL)}# No weights
)
weight_treatments <- c(
    #"no_treatment"#,
    "balanced"#,
    #"posterior"
)

preprocessing_treatments <- c(
    "no_treatment"#,
    #"min_max",
    #"standard",
    #"robust"
)

num_components <- c(
    1,
    2,
    #3,
    4,
    #5,
    #6,
    #7#
    8,
    #10,
    12,#14,
    16,#18,
    20,
    24,
    32,
    50,
    64#100,
    #128,
    #256,
    #512#,1024
)

depth_control <- c(
    #10,20, 
    
    40
)


seeds <- list(
    61718,
    11819,
    39790,
    736,
    37603
)