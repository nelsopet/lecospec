if(!dir.exists("Functions/")){
    setwd("../")
    if(!dir.exists("Functions")){
        setwd("M:/lecospec/lecospec/")
    }
}
source("Functions/lecospectR.R", echo = FALSE)

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

test_data <- subset(read.csv("Data/gs/x_test/img_raw_raw.csv"), select = -c(X))
test_labels <- read.csv("Data/gs/y_test/img_raw_raw.csv")$x %>% as.factor()
#train_labels <- read.csv("Data/gs/y_train/img_raw_raw.csv")$x %>% as.factor()
manifest_path <- "./gs_manifest_reduced_val_4sites.csv"



for(filepath in base_paths){
    train_data <- subset(read.csv(paste0("Data/gs/x_train/", filepath)), select = -c(X))
    labels <- read.csv(paste0("Data/gs/y_train/", filepath))$x %>% as.factor()

    for(o_treatment in outlier_treatments){
        for(p_treatment in preprocessing_treatments){
            for(w_treatment in weight_treatments){
                for(n in num_components){
                    for(seed in seeds){

                model_id <- uuid::UUIDgenerate()
                save_path <- paste0("mle/experiments/gs/", model_id, "/")
                if(!dir.exists(save_path)){
                    dir.create(save_path)
                }

                if("Forb" %in% levels(labels)){
                    levels(test_labels) <- c(levels(test_labels), c("Forb"))
                }

                rf_model_results <- train_model(
                    #filter_df_bands(train_data), 
                    train_data,
                    labels, 
                    test_data,
                    test_labels,
                    ntree = n,
                    max_depth = NULL,#d,
                    outlier_fn = outlier_functions[[o_treatment]],
                    preprocess_fn = preprocess_functions[[p_treatment]],
                    weight_fn = weight_functions[[w_treatment]],
                    model_id = model_id,
                    seed=seed,
                    log_string = paste(n, filepath, o_treatment, p_treatment, w_treatment)
                )
                #print(rf_model_results)

                rf_model <- rf_model_results$model
                acc <- as.list(rf_model_results$confusion$overall)$Accuracy
                #print(acc)

                if(acc > 0.6){


                results <- validate_model(
                    rf_model, 
                    save_path, 
                    outlier_processing = outlier_functions[[o_treatment]],
                    transform_type = preprocess_functions[[p_treatment]],
                    pft_aggregation = 0
                )

                aggregated_results <- aggregate_results(save_path)

                # calculate validation statistics
                #chi2 <- calculate_chi_squared_probability(aggregated_results)
                r2 <- calculate_validation_r2(aggregated_results)
                rpd <- calculate_rpd(aggregated_results)

                add_model_to_manifest(
                    model_id = model_id,
                    outlier = o_treatment,
                    preprocessing = p_treatment,
                    source = filepath,
                    weight = w_treatment,
                    n = n,
                    oob_error = rf_model$prediction.error,
                    accuracy = acc,
                    r2 = r2,
                    chi2prob = rpd,
                    seed = seed,
                    logpath=manifest_path
                )

                plot_by_pft(
                    aggregated_results,
                    save_path = paste0(save_path, "aggregate.html"),
                    open = FALSE,
                    image_path = NULL,
                    aggregation=0
                )
            #
                write_validation_table(
                    aggregated_results,
                    save_path = paste0(save_path, "table.html"),
                    open = FALSE
                )
                } else {
                    add_model_to_manifest(
                        model_id = model_id,
                        outlier = o_treatment,
                        preprocessing = p_treatment,
                        source = filepath,
                        weight = w_treatment,
                        n = n,
                        oob_error = rf_model$prediction.error,
                        accuracy = acc,
                        r2 = "Skipped",
                        chi2prob = "Skipped",
                        seed = seed,
                        logpath=manifest_path
                    )

                }
                    }
                }
            }
        }
    }
}