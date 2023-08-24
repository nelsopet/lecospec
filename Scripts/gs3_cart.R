source("Functions/lecospectR.R")

########################################
##  Define Grid Search Parametrs
########################################

# model-independent search parameters
max_per_pft <- c(75, 150)
bandwidths <- c(5, 25, 50)
correlation_thresholds <- c(0.95)
#TODO: add aggregation_level <- c(0, 1)
filter_features <- c(TRUE, FALSE)
transform_names <- c("Nothing")

# model hyperparameters
num_components <- 2^seq(0, 10)
alpha <- seq(0, 1, 0.1)

########################################
##  Define Assets
########################################
manifest_path <- "./gs3_cart.csv"
identity_fn <- function(dx){
    return(dx)
}
caret::getModelInfo("rpart")
transforms <- list()
transforms[["Nothing"]] <- identity_fn
print(transforms[["Nothing"]]("test"))


get_filename <- function(bandwidth, count, is_train = TRUE, base_path = "Data/v2/"){
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
        Site,
        site
        ))




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

caret::getModelInfo("rpart2")

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




    for(use_filter in filter_features){
        for(transform_name in transform_names){
            for(max_correlation in correlation_thresholds){
                #for(a in alpha){

                    


                    model_id <- uuid::UUIDgenerate()
                    model_dir <- paste0("mle/experiments/manual/", model_id, "/")
                    set.seed(61718)
                    dir.create(model_dir)
                    print(model_dir)


                       data <- NULL
                    if(use_filter){
                        data <- train_data[, selected_cols]
                    } else {
                        data <- remove_intercorrelated_variables(
                            train_data,
                            col_order = variable_importance$variable,
                            threshold = max_correlation
                            )
                    }


                    training_options <- caret::trainControl(
                        method = "cv",
                        number = 3,
                        search = "grid"
                    )
                    


                    #n_comp <- #min(length(used_cols), 32)
                    model <- caret::train(
                        x = transforms[[transform_name]](data),
                        y = labels,
                        method = "rpart1SE",
                        trControl = training_options#,
                    )

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
                        outlier = "CART (non-ordinal/SE)",
                        preprocessing = as.character(transform_name),
                        source = "v2",
                        weight = "balanced",
                        n = n,
                        oob_error = "na", #model$prediction.error,
                        accuracy = acc,
                        r2 = r2,
                        chi2prob = rpd,
                        seed = 61718,
                        logpath = manifest_path
                    )

               # }
            }
        }
    }
}}
