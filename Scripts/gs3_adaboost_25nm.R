source("Functions/lecospectR.R")

########################################
##  Define Grid Search Parametrs
########################################

# model-independent search parameters
max_per_pft <- c(125, 300, 500, 750, 1000, 2000)
bandwidths <- c(25)
correlation_thresholds <- c(0.99, 1.00)
#TODO: add aggregation_level <- c(0, 1)
filter_features <- c(TRUE, FALSE)
transform_names <- c("Nothing")
weighted <- c(TRUE, FALSE)


# model hyperparameters
num_components <- c(2, 4, 8, 16, 32, 64, 128, 256, 512) # for finetuning use: c(5,6,7,8,9,10,11,12,13,14,15)
max_depths <- c(5, 10, 15, 20, 25, 30)
# for finetuning use c(11,12,13,14,15,16,17,18,19)
alpha <- seq(0, 1, 0.1)

########################################
##  Define Assets
########################################
manifest_path <- "./gs3_adaboost_4.csv"
identity_fn <- function(dx){
    return(dx)
}


get_filename <- function(
    bandwidth, 
    count, 
    is_train = TRUE, 
    base_path = "Data/v2/") {
        if(is_train){
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

transforms <- list()
transforms[["Nothing"]] <- identity_fn
transforms[["bin10"]] <- function(df) {
    return(bin_df(df, num_bins = 10))
}
transforms[["bin20"]] <- function(df) {
    return(bin_df(df, num_bins = 20))
}
print(transforms[["Nothing"]]("test"))

########################################
##  Load Data 
########################################
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
        Site
        ))


train_data_full <- read.csv(training_path)
train_data <- subset(
    train_data_full,
    select = -c(
        X,
        UID,
    	FncGrp1,
        Site
        ))
labels <- train_data_full$FncGrp1 %>% as.factor()

rm(train_data_full)
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

caret::getModelInfo("AdaBoost.M1")

########################################
##  Run Grid Search
########################################

for(bandwidth_index in seq_along(bandwidths)){
    bandwidth <- bandwidths[[bandwidth_index]]
for(count in max_per_pft){


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
            Site
            ))
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

    variable_importance <- names(
        sort(
            variable_importance_model$variable.importance, 
            decreasing = TRUE)
            )

    rm(train_data_full)
    gc()

    for(use_filter in filter_features){
        for(transform_name in transform_names){
            for(n_comp in num_components){
                for(max_depth in max_depths){
                for(max_correlation in correlation_thresholds){
                for(weight_toggle in weighted){





                    model_id <- uuid::UUIDgenerate()
                    model_dir <- paste0("mle/experiments/manual/", model_id, "/")
                    set.seed(61718)
                    dir.create(model_dir)
                    print(model_dir)

                   

                    data <- remove_intercorrelated_variables(
                            train_data,
                            col_order = variable_importance,
                            threshold = max_correlation)
                    

                    training_options <- caret::trainControl(
                        method = "boot",
                        #repeats = 3,
                        #number = 3,
                        search = "grid"
                    )
                    
                    grid <- expand.grid(
                        mfinal = n_comp,
                        maxdepth = max_depth,
                        coeflearn = c("Breiman", "Freund", "Zhu")
                    )
                    if(weight_toggle) {
                        model <- caret::train(
                            x = transforms[[transform_name]](data),
                            y = labels,
                            method = "AdaBoost.M1",
                            preProcess = c("center", "scale"),
                            weights = targets_to_weights(labels),
                            trControl = training_options,
                            tuneGrid = grid
                        )
                    } else {

                    model <- caret::train(
                        x = transforms[[transform_name]](data),
                        y = labels,
                        method = "AdaBoost.M1",
                        preProcess = c("center", "scale"),
                        trControl = training_options,
                        tuneGrid = grid
                    )
                    }
                    #n_comp <- #min(length(used_cols), 32)

                #ranger::ranger(
                #            num.trees = n_comp,
                #            replace = TRUE,
                #            case.weights = targets_to_weights(labels),
                #            classification = TRUE,
                #            #alpha = 0.75,
                #            x = train_data[, used_cols],
                #            y = labels
                #        )
                    if(("Forb" %in% levels(labels)) && !("Forb"  %in% levels(test_labels))){
                            levels(test_labels) <- c(levels(test_labels), "Forb")
                            }

                    # create predictions (ranger)
                    model_predictions <- predict(
                        model, 
                        test_data
                    )#$prediction %>% as.factor()

                    # generate the confusion matrix
                    print(model_predictions %>% levels())
                    print(test_labels %>% levels())
                    print(model_predictions %>% to_fg0() %>% levels())
                    print(test_labels %>% to_fg0() %>% levels())


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
                        model_type = "AdaBoost",
                        bandwidth = bandwidth,
                        max_count = count,
                        preprocessing = paste0(
                            "center & ",
                            "scale & ",
                            transform_name),
                        max_correlation = max_correlation,
                        weight = weight_toggle,
                        hyperparam1 = n_comp,
                        hyperparam2 = max_depth,
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
}}}

