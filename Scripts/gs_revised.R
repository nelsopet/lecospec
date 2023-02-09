# %%
if(!dir.exists("Functions/")){
    setwd("../")
    if(!dir.exists("Functions")){
        setwd("M:/lecospec/lecospec/")
    }
}
source("Functions/lecospectR.R", echo = FALSE)

# %%
log_model_results <- function(model_id, confusion_matrix, distribition, custom = NULL, logpath = "./gs.log"){
    # append performance data to the logs for later comparison
    sink(file = logpath, append = TRUE)
    print("-------------------------------------------------------")
    print("---------------------- Model Data ---------------------")
    
    print(paste0("Model Type: Ranger (Random Forest)"))
    print(paste0("Data Index: ",custom))
    print(paste0("Model UUID: ", model_id))
    print("---------------------- Confusion Matrix ---------------------")
    print(confusion_matrix)
    print("---------------------- Class Distribution ---------------------")
    print(distribition)
    print("-------------------------------------------------------")
    sink(NULL)
}

# %%
add_model_to_manifest <- function(
    model_id, 
    outlier = "", 
    preprocessing="",
    source="", 
    weight = "",
    oob_error = "",
    accuracy = "",
    r2 = "",
    chi2prob = "",
    logpath="./gs_manifest.csv"){
    if(!file.exists(logpath)){
        header <- "source,outliers,preprocessing,weight,oob,accuracy,r2,chi2prob,model_id"
        write(header, file = logpath)
    }

    line <- paste(
        source,
        outlier,
        preprocessing,
        weight,
        oob_error,
        accuracy,
        r2,
        chi2prob,
        sep=","
    )
    line <- paste0(line, ",", model_id)

    write(line, file=logpath, append = TRUE)
}

# %%
train_model <- function(
    train_df, 
    train_labels,
    test_df, 
    test_labels,
    outlier_fn = NULL,
    preprocess_fn = NULL,
    weight_fn = targets_to_weights,
    model_id = uuid::UUIDgenerate(),
    ignore_cols = NULL,
    seed = NULL,
    log_string = ""
){
    if(!is.null(seed)){
        set.seed(seed)
    }

    x_train <- train_df %>% as.data.frame()
    x_test <- test_df %>% as.data.frame()
    if(is.function(outlier_fn)){
        x_train <- outlier_fn(x_train)
    }
    if(is.function(preprocess_fn)){
        x_train <- preprocess_fn(x_train)
        x_test <- preprocess_fn(x_test)
    }

    model <- ranger::ranger(
            num.trees = 16,
            case.weights = weight_fn(train_labels),
            classification = TRUE,
            x=x_train,
            y=train_labels
        )

    if(("Forb" %in% levels(train_labels)) && !("Forb"  %in% levels(test_labels))){
            levels(test_labels) <- c(levels(test_labels), "Forb")
            }

    # create predictions (ranger)
        model_predictions <- predict(
            model, 
            x_test
        )$prediction %>% as.factor()

        # generate the confusion matrix

        confusion_matrix <- caret::confusionMatrix(
            model_predictions, 
            test_labels,
            mode = "everything"
        )

        # generate an id to uniquely identify the model
        #model_id <- uuid::UUIDgenerate()

        # append performance data to the logs for later comparison
        log_model_results(
            model_id = model_id,
            confusion_matrix = confusion_matrix,
            custom = log_string,
            distribition = model_predictions %>% as.factor() %>% table(),
            logpath = "./gs5.log")

        # track what levels are associated with the UUID

        # save the model using the model UUID
        save(model, file = paste0("mle/models/gs/", model_id, ".rda"))
        
        return(
            list(
                model = model,
                confusion = confusion_matrix %>% as.list()
                )
        )
}

# %%
base_paths <- c(
    #"img_raw_raw.csv",
    #"grd_raw_raw.csv",
    "img_indices_only.csv",# include veg indices
    "grd_raw_corrected.csv"
    #"grd_indices_only.csv"
)
# 

# %%
calculate_posterior_weights <- function(validation_path ="figures/merged_validation_s.csv" ){

    validation_df <- read.csv(validation_path, header = TRUE)
    #print(head(validation_df))

    total_observations <- sum(validation_df$validation_counts)
    #print(total_observations)
    weights <- (1/ validation_df$validation_prop)
    #print(validation_df$validation_prop)

    total_by_fg1 <- aggregate(
        x = validation_df$validation_counts,
        by = list(validation_df$key),
        FUN = sum
    )

    fg1_weight_list <- list()

    for( row_idx in seq(nrow(total_by_fg1))){
        name <- total_by_fg1$Group.1[[row_idx]]
        value <- total_by_fg1$x[[row_idx]]
        fg1_weight_list[name] <- value
    }
    
    return(fg1_weight_list)
}

get_posterior_weights_from_targets <- function(target_factor, posterior_weight = calculate_posterior_weights()){
    unbiased_weights <- targets_to_weights(target_factor)

    target_name_char <- target_factor %>% as.character()

    output_weights <- seq_along(target_factor)

    for(i in seq_along(target_factor)){
        if(posterior_weight[[target_name_char[[i]]]] > 0){
            fg1_weight <- 1 / posterior_weight[[target_name_char[[i]]]]
        } else {
            fg1_weight <- 0
        }
        output_weights[[i]] <- unbiased_weights[[i]] * fg1_weight
    }

    return(output_weights)
}

# %% [markdown]
# ## Hypotheses
# 
# Peter
# * Increasing number of trees will improve the accuracy/validation mismatch
# * bias in 
# 
# Ken
# * decreasing the number of the trees will decrease the impact of the posterior weighting on chi-squared statistic

# %%
outlier_functions <- list(
    clip = load_model("./mle/clip_transform.rda"),
    no_treatment = function(x, ignore_cols = NULL){return(x)}# no transform
)

outlier_treatments <- c(
    "no_treatment"
#    "clip"
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
#    "no_treatment",
    "balanced"
#    "posterior"
)

preprocessing_treatments <- c(
    #"robust",
    #"min_max",
    "no_treatment"#,
    #"standard"
)

# %%
test_data <- read.csv("Data/gs/x_test/img_raw_raw.csv")
test_labels <- read.csv("Data/gs/y_test/img_raw_raw.csv")$x %>% as.factor()
#train_labels <- read.csv("Data/gs/y_train/img_raw_raw.csv")$x %>% as.factor()

# %%
for(filepath in base_paths){
    train_data <- subset(read.csv(paste0("Data/gs/x_train/", filepath)), select = -c(X))
    labels <- read.csv(paste0("Data/gs/y_train/", filepath))$x %>% as.factor()

    for(o_treatment in outlier_treatments){
        for(p_treatment in preprocessing_treatments){
            for(w_treatment in weight_treatments){

                print(p_treatment)
                model_id <- uuid::UUIDgenerate()
                save_path <- paste0("mle/experiments/gs/", model_id, "/")
                if(!dir.exists(save_path)){
                    dir.create(save_path)
                }

                rf_model_results <- train_model(
                    train_data, 
                    labels, 
                    test_data,
                    test_labels,
                    outlier_fn = outlier_functions[[o_treatment]],
                    preprocess_fn = preprocess_functions[[p_treatment]],
                    weight_fn = weight_functions[[w_treatment]],
                    model_id = model_id,
                    seed=61718,
                    log_string = paste(filepath, o_treatment, p_treatment, w_treatment)
                )

                rf_model <- rf_model_results$model
                acc <- as.list(rf_model_results$confusion$overall)$Accuracy
                print(acc)



                results <- validate_model(
                    rf_model, 
                    save_path, 
                    outlier_processing = outlier_functions[[o_treatment]],
                    transform_type = preprocess_functions[[p_treatment]],
                )

                aggregated_results <- aggregate_results(save_path)

                # calculate validation statistics
                chi2 <- calculate_chi_squared_probability(aggregated_results)
                r2 <- calculate_validation_r2(aggregated_results)

                add_model_to_manifest(
                    model_id = model_id,
                    outlier = o_treatment,
                    preprocessing = p_treatment,
                    source = filepath,
                    weight = w_treatment,
                    oob_error = rf_model$prediction.error,
                    accuracy = acc,
                    r2 = r2,
                    chi2prob = chi2,
                    logpath="./gs_manifest_7.csv"
                )

                plot_by_pft(
                    aggregated_results,
                    save_path = paste0(save_path, "aggregate.html"),
                    open = FALSE,
                    image_path = NULL
                )
            #
                write_validation_table(
                    aggregated_results,
                    save_path = paste0(save_path, "table.html"),
                    open = FALSE
                )
            }
        }
    }
}

# %%
print(as.list(rf_model_results$confusion$overall)$Accuracy)

# %%
r2 <- calculate_validation_r2(aggregated_results)

# %%
r2$adj.r.squared

# %%
names(rf_model)

# %%


# %%
rf_model$confusion.matrix

# %%



