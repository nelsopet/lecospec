source("Functions/lecospectR.R")

TEST_NOISE <- TRUE
NOISE_POWER
TEST_MIN_MAX <- TRUE
USE_CAL_VAL_SPLIT <- TRUE
TEST_NORMALIZATION <- TRUE
base_path <- "mle/experiments/"

data_paths <- c(
    "mle/training.csv",
    # should have img, ground, and noisy version of each
)
data_names <- c("ground","image", "noisyImage", "noisyGround")
weights <- c(
    # read in the prior, posterior and vector of all ones
)
weight_names <- c(
    "validation",
    "training",
    "none"
)

transforms <- c(add_noise, global_min_max_scale, function(df){ return(df) }, )
transform_names <- c("MinMax Scale", "None")

num_trees <- c(256, 512, 786, 1024, 1280, 1536, 1792, 2048)


set.seed(617)
data <- read.csv(data_paths[[data_index]])
data_name <- data_names[[data_index]]

for(transform_index in seq_along(transforms)){
    training_data <- transforms[[transform_index]](data)
    training_data_name <- transform_names[[transform_index]]

    for(weight_index in weights){
        weight_vec <- # read the JSON file here
        weights_name <- weight_names[[weight_index]]

        for(trees in num_trees){
            # set up file paths and create directories
            save_dir <- paste(
                data_name, 
                transform_name, 
                weights_name, 
                trees, 
                sep = "_")
            if(!dir.exists(save_dir)){
                dir.create(save_dir)
            }
            model_save_path <- paste0(save_dir, "/model.rda")
            results_path <- paste0(save_dir, "/results")
            if(!dir.exists(results_path)){
                dir.create(results_path)
            }



            # train the model
            model <- ranger::ranger()

            # run the model validation only if the OOB prediction is high enough
            if(model){
                results <- validate_model(model, results_path)
                # should create the validation stuff as well

            } 
            
        }
    }
}



