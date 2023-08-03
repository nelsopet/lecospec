source("Functions/lecospectR.R")

########################################
##  Define Grid Search Parametrs
########################################

# model-independent search parameters
max_per_pft <- c(77, 100, 200, 300)
filter_features <- c(TRUE, FALSE)
transform_names <- c()

# model hyperparameters
num_components <- 2^seq(0, 10)
alpha <- seq(0, 1, 0.1)

########################################
##  Define Assets
########################################
manifest_path <- "./gs3_gbm.csv"
identity_fn <- function(dx){
    return(dx)
}

transforms <- list()
transforms[["Nothing"]] <- identity_fn
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


########################################
##  Run Grid Search
########################################

for(count in max_per_pft){
    for(use_filter in filter_features){
        for(transform_name in transform_names){
            for(n in num_components){
                for(a in alpha){




                    model_id <- uuid::UUIDgenerate()
                    model_dir <- paste0("mle/experiments/manual/", model_id, "/")
                    set.seed(61718)
                    dir.create(model_dir)
                    print(model_dir)

                    row_balance <- create_stratified_sample(
                            labels, 
                            samples_per_pft = count
                        )

                    data <- NULL
                    if(use_filter){
                        data <- train_data[row_balance, selected_cols]
                    } else {
                        data <- train_data[row_balance, ]
                    }

                    training_options <- caret::trainControl(
                        method = "cv",
                        repeats = 3,
                        number = 3,
                        search = "grid"
                    )
                    
                    grid <- expand.grid(
                        C = n,
                        loss = c("L1", "L2")
                    )

                    #n_comp <- #min(length(used_cols), 32)
                    model <- caret::train(
                        x = transforms[[transform_name]](data),
                        y = labels[row_balance],
                        method = "AdaBoost.M1",
                        preProcess = c("center", "scale"),
                        trControl = training_options#,
                    )

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
                        outlier = "no_treatment",
                        preprocessing = paste0(
                            "center & ",
                            "scale & ",
                            transform_name),
                        source = "v2",
                        weight = "balanced",
                        n = n,
                        oob_error = model$prediction.error,
                        accuracy = acc,
                        r2 = r2,
                        chi2prob = rpd,
                        seed = 61718L,
                        logpath = manifest_path
                    )
                }
            }
        }
    }
}