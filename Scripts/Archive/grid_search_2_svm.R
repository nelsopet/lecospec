#######################################################
#       Load Lecospec Core Functions
#######################################################
if(!dir.exists("Functions/")){
    setwd("../")
    if(!dir.exists("Functions")){
        setwd("M:/lecospec/lecospec/")
    }
}
source("Functions/lecospectR.R", echo = FALSE)
source("Scripts/search_control.R", echo = FALSE)


#######################################################
#       Load Data
#######################################################
#train_labels <- read.csv("Data/gs/y_train/img_raw_raw.csv")$x %>% as.factor()
manifest_path <- "./gs_manifest_reduced_val_4sites.csv"
training_path <- "Data/v2/train.csv"
test_path <- "Data/v2/test.csv"

test_data_full <- read.csv(test_path)
test_labels <- test_data_full$FncGrp1 %>% as.factor()
test_data <- subset(
    test_data_full,
    select = -c(
        X,
        UID,
    	FncGrp1,
        Site,
        site
        ))


rm(test_data_full)
gc()


train_data_full <- read.csv(training_path)
train_data <- subset(
    train_data_full,
    select = -c(
        X,
        UID,
    	FncGrp1,
        Site,
        site
        ))
labels <- train_data_full$FncGrp1 %>% as.factor()

rm(train_data_full)
gc()

#######################################################
#       Training Loop: RF
#######################################################
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
                    outlier_fn = outlier_functions[["no_treatment"]],
                    preprocess_fn = preprocess_functions[["no_treatment"]],
                    weight_fn = weight_functions[["balanced"]],
                    model_id = model_id,
                    seed = seed,
                    log_string = paste(n, " v2 - Image")
                )
                #print(rf_model_results)

                rf_model <- rf_model_results$model
                #rf_model <- load_model("mle/models/gs/3012f5ed-7d17-4e94-a454-24d8a65f5b4f.rda")
                acc <- as.list(rf_model_results$confusion$overall)$Accuracy
                #print(acc)

                if(acc > 0.6){


                results <- validate_model(
                    rf_model, 
                    save_path, 
                    outlier_processing = outlier_functions[["no_treatment"]],
                    transform_type = preprocess_functions[["no_treatment"]],
                    pft_aggregation = 0
                )

                aggregated_results <- aggregate_results(save_path)

                # calculate validation statistics
                #chi2 <- calculate_chi_squared_probability(aggregated_results)
                r2 <- calculate_validation_r2(aggregated_results)
                rpd <- calculate_rpd(aggregated_results)

                add_model_to_manifest(
                    model_id = model_id,
                    outlier = "no_treatment",
                    preprocessing = "no_treatment",
                    source = "v2",
                    weight = "balanced",
                    n = n,
                    oob_error = rf_model$prediction.error,
                    accuracy = acc,
                    r2 = r2,
                    chi2prob = rpd,
                    seed = seed,
                    logpath = manifest_path
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
                        outlier = "no_treatment",
                        preprocessing = "no_treatment",
                        source = "v2",
                        weight = "balanced",
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
