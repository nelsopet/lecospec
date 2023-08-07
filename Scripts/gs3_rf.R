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

#TODO: investigate include_site <- c(TRUE, FALSE)



# model hyperparameters
num_components <- 2^seq(1, 9, 2)# 1-1024, doubling each time
alpha <- seq(0.25, 1, 0.25)

# number of states: 
num_states <- length(num_components) * 
    length(alpha) * 
    length(filter_features) * 
    length(max_per_pft) * 
    length(bandwidths) * 
    length(correlation_thresholds) * 
    length(transform_names)

print(paste0("Grid search states: ", num_states))
print(paste0("Grid search estimated time: ", 7 / 60 * num_states, " hours"))


########################################
##  Define Assets
########################################
manifest_path <- "./gs3_ranger.csv"

transforms <- list()
transforms[["Nothing"]] <- function(dx){
    return(dx)
}

########################################
##  Load Data 
########################################
training_path <- "Data/v2/train.csv"
test_path <- "Data/v2/test.csv"


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

# raster::beginCluster()

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

            # iterate over model hyperparameters
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
                        data <- remove_intercorrelated_variables(
                            train_data[row_balance, ],
                            threshold = max_correlation
                            )
                    }

                    model <- ranger::ranger(
                            num.trees = n,
                            replace = TRUE,
                            classification = TRUE,
                            alpha = a,
                            x = transforms[[transform_name]](data),
                            y = labels[row_balance]
                        )
                    if(("Forb" %in% levels(labels)) && !("Forb"  %in% levels(test_labels))){
                            levels(test_labels) <- c(levels(test_labels), "Forb")
                            }

                    # create predictions (ranger)
                    model_predictions <- predict(
                        model, 
                        test_data
                    )$prediction %>% as.factor()

                    # generate the confusion matrix
                    #print(model_predictions %>% levels())
                    #print(test_labels %>% levels())
                    #print(model_predictions %>% to_fg0() %>% levels())
                    #print(test_labels %>% to_fg0() %>% levels())


                    confusion_matrix <- caret::confusionMatrix(
                        model_predictions %>% to_fg0(), 
                        test_labels %>% as.factor() %>% to_fg0() %>% add_forb(),
                        mode = "everything"
                    )
                    acc <- as.list(confusion_matrix$overall)$Accuracy
                    print(paste0("Model Accuracy: ", acc))

                    validate_model(
                        model,
                        save_directory = model_dir#,
                        #cluster = raster::getCluster()
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
                        outlier = "Random Forest",
                        preprocessing = paste0(
                            transform_name),
                        source = "v2",
                        weight = a,
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
    }
}

#raster::endCluster()
