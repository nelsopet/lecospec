source("Functions/lecospectR.R")

########################################
##  Define Grid Search Parametrs
########################################

# model-independent search parameters
max_per_pft <- c(125, 300, 500, 750, 1000, 2000)
bandwidths <- c(5, 10, 25, 50)
correlation_thresholds <- c(0.99, 1.00)
# TODO: add aggregation_level <- c(0, 1)
#filter_features <- c(TRUE, FALSE)
transform_names <- c(
    "Nothing",
    "bin10",
    "bin20")

get_filename <- function(
    bandwidth, 
    count, 
    is_train = TRUE, 
    base_path = "Data/v2/") {
    if (is_train) {
        train_test_string <- "train"
    } else {
        train_test_string <- "test"
    }

    return(
        paste0(
            base_path,
            train_test_string,
            "_",
            bandwidth,
            "nm_",
            count,
            ".csv"
        )
    )
}

# TODO: investigate include_site <- c(TRUE, FALSE)



# model hyperparameters
num_components <- c(5,6,7,8,9,10,11,12,13,14,15)# fine tuning

# number of states:
#num_states <- length(num_components) *
#    length(alpha) *
#    length(filter_features) *
#    length(max_per_pft) *
#    length(bandwidths) *
#    length(correlation_thresholds) *
#    length(transform_names)

#print(paste0("Grid search states: ", num_states))
#print(paste0("Grid search estimated time: ", 7 / 60 * num_states, " hours"))


########################################
##  Define Assets
########################################
manifest_path <- "./gs3_ranger_5.csv"

transforms <- list()
transforms[["Nothing"]] <- function(dx) {
    return(dx)
}
transforms[["bin10"]] <- function(df) {
    return(bin_df(df, num_bins = 10))
}
transforms[["bin20"]] <- function(df) {
    return(bin_df(df, num_bins = 20))
}


########################################
##  Load Data
########################################
training_path <- "Data/v2/train.csv"
test_path <- "Data/v2/test.csv"


#rm(train_data_full)
gc()

selected_cols <- c(
    "PRInorm",
    "SRPI",
    "PSND",
    "Carter3",
    "Datt4",
    "SR8",
    "Carter5",
    "X442.593_5nm",
    "X662.593_5nm",
    "Datt3",
    "EVI"
)
variable_importance <- read.csv("./assets/variable_importance.csv")

# raster::beginCluster()

########################################
##  Run Grid Search
########################################



for (bandwidth_index in seq_along(bandwidths)) {
    bandwidth <- bandwidths[[bandwidth_index]]
    for (count in max_per_pft) {
        test_path <- get_filename(
            bandwidth = bandwidth,
            count = count,
            is_train = FALSE
        )
        training_path <- get_filename(
            bandwidth = bandwidth,
            count = count
        )

        test_data_full <- read.csv(test_path)
        test_labels <- test_data_full$FncGrp1 %>% as.factor()
        test_data <- subset(
            test_data_full,
            select = -c(
                X,
                UID,
                FncGrp1,
                Site
            )
        )

        rm(test_data_full)
        gc()

        train_data_full <- read.csv(training_path)
        train_data <- subset(
            train_data_full,
            select = -c(
                X,
                UID,
                FncGrp1,
                Site
            )
        )
        labels <- train_data_full$FncGrp1 %>% as.factor()

        variable_importance_model <- ranger::ranger(
            num.trees = 1000,
            importance = "impurity_corrected",
            replace = TRUE,
            classification = TRUE,
            x = train_data,
            y = labels
        )

        #print(names(sort(variable_importance_model$variable.importance, decreasing = TRUE)))

        variable_importance_list <- names(
            sort(
                variable_importance_model$variable.importance, 
                decreasing = TRUE)
                )

        # for(use_filter in filter_features){
        for (transform_name in transform_names) {
            for (max_correlation in correlation_thresholds) {
                # iterate over model hyperparameters

                data <- remove_intercorrelated_variables(
                        train_data,
                        col_order = variable_importance_list,
                        threshold = max_correlation
                    )

                for (n in num_components) {
                    # for(a in alpha){

                    model_id <- uuid::UUIDgenerate()
                    model_dir <- paste0("mle/experiments/manual/", model_id, "/")
                    set.seed(61718)
                    dir.create(model_dir)
                    print(model_dir)

                    print(head(transforms[[transform_name]](data)))
                    model <- ranger::ranger(
                        num.trees = n,
                        replace = TRUE,
                        classification = TRUE,
                        # alpha = a,
                        case.weights = targets_to_weights(labels),
                        x = impute_spectra(transforms[[transform_name]](data)),
                        y = labels
                    )
                    if (("Forb" %in% levels(labels)) && !("Forb" %in% levels(test_labels))) {
                        levels(test_labels) <- c(levels(test_labels), "Forb")
                    }

                    # create predictions (ranger)
                    model_predictions <- predict(
                        model,
                        test_data
                    )$prediction %>% as.factor()

                    confusion_matrix <- caret::confusionMatrix(
                        model_predictions %>% to_fg0(),
                        test_labels %>% as.factor() %>% to_fg0() %>% add_forb(),
                        mode = "everything"
                    )

                    acc <- as.list(confusion_matrix$overall)$Accuracy
                    print(paste0("Model Accuracy: ", acc))

                    validate_model(
                        model,
                        save_directory = model_dir
                    )

                    aggregated_results <- aggregate_results(model_dir)
                    r2 <- calculate_validation_r2(aggregated_results)
                    rpd <- calculate_rpd(aggregated_results)

                    print(r2)

                    save(model, file = paste0(model_dir, "model.rda"))

                    print(model_dir)
                    plt <- plot_by_pft(
                        aggregated_results,
                        save_path = paste0(model_dir, "aggregate.html"),
                        open = FALSE,
                        image_path = NULL,
                        aggregation = 0
                    )
                    
                    add_model_to_manifest(
                        model_id = model_id,
                        model_type = "Random Forest",
                        bandwidth = bandwidth,
                        max_count = count,
                        preprocessing = paste0(
                            transform_name
                        ),
                        max_correlation = max_correlation,
                        weight = "balanced",
                        hyperparam1 = n,
                        # oob_error = model$prediction.error,
                        accuracy = acc,
                        r2 = r2,
                        rpd = rpd,
                        seed = 61718L,
                        logpath = manifest_path
                    )
                }
            }
        }
    }
}


sapply(transforms[[transform_name]](data), function(x) sum(is.nan(x)))
sapply(transforms[[transform_name]](data), function(x) sum(is.na(x)))
