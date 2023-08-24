source("Functions/lecospectR.R")

########################################
##  Define Grid Search Parametrs
########################################

# model-independent search parameters
max_per_pft <- c(75, 150, 300, 600)
bandwidths <- c(5, 10, 25)
# correlation_thresholds <- c(0.97, 0.98, 0.99, 1.00)
# TODO: add aggregation_level <- c(0, 1)
weight <- c(TRUE, FALSE)
# filter_features <- c(TRUE, FALSE)
# transform_names <- c("Nothing")

get_filename <- function(bandwidth, count, is_train = TRUE, base_path = "Data/v2/") {
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
            "_bands.csv"
        )
    )
}

# TODO: investigate include_site <- c(TRUE, FALSE)



# model hyperparameters
num_components <- seq(6, 32, 2) # 2-32
# alpha <- seq(0.25, 1, 0.25)



########################################
##  Define Assets
########################################
manifest_path <- "./gs3_pls.csv"

transforms <- list()
transforms[["Nothing"]] <- function(dx) {
    return(dx)
}

########################################
##  Load Data
########################################
training_path <- "Data/v2/train.csv"
test_path <- "Data/v2/test.csv"
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
                Site,
                site
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
                Site,
                site
            )
        )
        labels <- train_data_full$FncGrp1 %>% as.factor()




        # for(use_filter in filter_features){
        #for (transform_name in transform_names) {
            #for (max_correlation in correlation_thresholds) {
                # iterate over model hyperparameters

                for (nc in num_components) {
                    # for(a in alpha){
                    n_comp <- min(
                        ncol(train_data),
                        nc)
                    model_id <- uuid::UUIDgenerate()
                    model_dir <- paste0("mle/experiments/manual/", model_id, "/")
                    set.seed(61718)
                    dir.create(model_dir)
                    print(model_dir)


                    pls_model_results <- train_pls_lda(
                        train_data,
                        labels,
                        test_data,
                        test_labels,
                        n = n_comp,
                        model_id = model_id,
                        seed = 61718L,
                        log_string = "PLS"
                    )
                    print(pls_model_results)

                    model <- pls_model_results$model
                    acc <- as.list(pls_model_results$confusion$overall)$Accuracy
                    print(acc)
                    if (("Forb" %in% levels(labels)) && !("Forb" %in% levels(test_labels))) {
                        levels(test_labels) <- c(levels(test_labels), "Forb")
                    }

                    # create predictions (ranger)


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
                        outlier = "PLS",
                        preprocessing = "center + scale",
                        source = count,
                        weight = "balanced",
                        n = n_comp,
                        # oob_error = model$prediction.error,
                        accuracy = acc,
                        r2 = r2,
                        chi2prob = rpd,
                        seed = 61718L,
                        logpath = manifest_path
                    )
        }
    }
}
