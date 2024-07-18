#' adds noise to the data.frame
#'
#' Adds noise to the specified columns of a data.frame (or, 
#' if no columns are specified, all columns).  The amount of noise
#' added is controlled by the noise_power parameter.  The noise is 
#' always normally distributed with mean 0.
#'
#' @param df the dataframe to add noise to
#' @param noise_power the variance of the normal distribution
#' used to add noise
#' @param used_cols (optional, default NULL).  An optional character 
#' vector of column names to add noise to, or (default) NULL.  If NULL, 
#' then noise will be added to all columns.
#' @return a data.frame
#' @export
add_noise <- function(df, noise_power, used_cols = NULL) {
    output_df <- df
    total_observations <- nrow(df)
    for (col_index in seq_along(used_cols)) {
        active_column <- used_cols[col_index]
        output_df[, active_column] <- output_df[, active_column] +
            rnorm(total_observations, 0, noise_power)
    }
    return(output_df)
}


#' calculates the proportion of model predictions
#'
#' Counts the number of correct/incorrect classifications by
#' class.  Returns a list of the overall accuracy (at $accuracy)
#' and the count by each class (at $counts, itseld a list keyed by 
#' class names)
#'
#' @param df a data.frame of input data for making predictions
#' @param model a model to make predictions (must support apply_model)
#' @param targets the correct labels for each row of df
#' @return a list with two entries: acccuracy and counts.  
#' @export
#'
check_output_distribution <- function(df, model, targets) {
    predictions <- apply_model(df, model)$. %>% as.factor()
    correct_observations <- 0
    print(head(predictions))
    print(head(targets))

    for (i in seq_along(predictions)) {
        if (predictions[[i]] == targets[[i]]) {
            correct_observations <- correct_observations + 1
        }
    }

    class_counts <- predictions %>% table()

    return(
        list(
            accuracy = correct_observations / nrow(df),
            counts = class_counts %>% as.list()
        )
    )
}

#' checks of a data.frame for infs, NAs, and calculates stats
#' 
#' First checks the data.frame for NAs and Inf values, then calculates
#' the global min, max, mean, and variance.  It also checks the 
#' minimum and maximum (columnwise) variance.  
#' 
#' @param input_df a data.frame 
#' @param ignore_cols (optional, default NULL) a list of columns that 
#' should be ignored.  If NULL, then all columns will be used.
automated_check <- function(input_df, ignore_cols = NULL) {
    used_cols <- colnames(input_df)
    if (!is.null(ignore_cols)) {
        used_cols <- setdiff(colnames(input_df), ignore_cols)
    }

    df <- input_df[, used_cols]

    indx <- apply(df, 2, function(x) any(is.na(x) | is.infinite(x)))
    if (length(indx > 0)) {
        print("NAs and/or infinite values detected in data")
    }

    column_max_values <- apply(df, 2, function(x) base::max(x, na.rm = TRUE))
    column_min_values <- apply(df, 2, function(x) base::min(x, na.rm = TRUE))
    column_median_values <- apply(df, 2, function(x) stats::median(x, na.rm = TRUE))
    column_mean_values <- apply(df, 2, function(x) base::mean(x, na.rm = TRUE))
    column_variances <- apply(df, 2, function(x) var(x, na.rm = TRUE))

    print(paste0("Global minimum: ", min(column_min_values)))
    print(paste0("Global maximum: ", max(column_max_values)))
    print(paste0("Global Mean: ", mean(column_mean_values)))
    print(paste0("Global Estimated Variance: ", mean(column_variances)))
    print(paste0("Global Minimum Variance: ", min(column_variances)))
    print(paste0("Global Maximum Variance: ", max(column_variances)))

    return(list(
        maxs = column_max_values,
        mins = column_min_values,
        means = column_mean_values,
        medians = column_median_values,
        vars = column_variances
    ))
}

#' converts a factor vector into weights
#'
#' Calculates the class weights from a raw vector
#' of factor levels, such that each factor has 
#' equal probability of being included in a sample.
#'
#' @param target_factors the vector of factor levels
#' @return a numeric vector of weights for each observation
#' @export
targets_to_weights <- function(target_factors) {
    weights_by_pft <- target_factors %>%
        table() %>%
        purrr::map(., function(x) {
            return(abs(1 / x))
        }) %>%
        as.list()
    weights_by_pft$Unknown <- 0 # should not include data with no label

    return(purrr::map(
        target_factors %>% as.character(),
        function(x) {
            return(weights_by_pft[[x]])
        }
    ) %>%
        as.numeric())
}

#' Compares shared columns (by name) across data.frames
#'
#' Matches columns by name across data.frames and performs a 
#' Kolmogorov–Smirnov test (KS test) to each pair of matched columns.
#' Columns that are not shared are ignored, as are any columns named
#' in the `ignore_cols` argument.  
#'
#' @param df1 a data.frame
#' @param df2 another data.frame with shared column names
#' @param ignore_cols (default NULL)
#' @return a list of KS test results
#' @export
test_transferrability <- function(df1, df2, ignore_cols = NULL) {
    used_cols <- intersect(colnames(df1), colnames(df2))
    if (!is.null(ignore_cols)) {
        used_cols <- setdiff(used_cols, ignore_cols)
    }

    results <- list()

    for (col in used_cols) {
        if (is.numeric(df1[, col]) & is.numeric(df2[, col])) {
            test_result <- stats::ks.test(
                df1[, col],
                df2[, col]
            )
        }

        results[[col]] <- test_result
    }
    return(results)
}


#' Calculates the class weights to match a posterior distribution
#' 
#' Loads the validation data for a model and then creates 
#' weights for each output class to make the training data 
#' weights match the frequency in the posterior (target) distribution.
#' 
#' @param validation_path the file of validation data to load
#' @return a list of weights by class
#' @seealso get_posterior_weights_from_targets
#' @export
calculate_posterior_weights <- function(
    validation_path = "figures/merged_validation_s.csv") {
    validation_df <- read.csv(validation_path, header = TRUE)
    # print(head(validation_df))

    total_observations <- sum(validation_df$validation_counts)
    # print(total_observations)
    weights <- (1 / validation_df$validation_prop)
    # print(validation_df$validation_prop)

    total_by_fg1 <- aggregate(
        x = validation_df$validation_counts,
        by = list(validation_df$key),
        FUN = sum
    )

    fg1_weight_list <- list()

    for (row_idx in seq(nrow(total_by_fg1))) {
        name <- total_by_fg1$Group.1[[row_idx]]
        value <- total_by_fg1$x[[row_idx]]
        fg1_weight_list[name] <- value
    }

    return(fg1_weight_list)
}


#' Calculates class weights from for training data
#' 
#' Calculates the weights for each training sample such that it
#' matches the posterior (target) distribution.
#' 
#' @param target_factor a factor vector of target labels for a 
#' classifier
#' @param posterior the frequency (or relative frequency) of each class
#' from the posterior distribution.  By default, it is calculated
#' from `calculate_posterior_weights`
#' @param unbiased_weights the weights (for each class) that would 
#' make each class eaqually likely.  By default this is calculated
#' for you by targets_to_weights(target_factor)
#' @return a numeric vector of class weights for each observation
#' @export 
#' @seealso calcualte_posterior_weights, targets_to_weights
get_posterior_weights_from_targets <- function(
    target_factor, 
    posterior_weight = calculate_posterior_weights()) {
    unbiased_weights <- targets_to_weights(target_factor)

    target_name_char <- target_factor %>% as.character()

    output_weights <- seq_along(target_factor)

    for (i in seq_along(target_factor)) {
        if (posterior_weight[[target_name_char[[i]]]] > 0) {
            fg1_weight <- 1 / posterior_weight[[target_name_char[[i]]]]
        } else {
            fg1_weight <- 0
        }
        output_weights[[i]] <- unbiased_weights[[i]] * fg1_weight
    }

    return(output_weights)
}

#' writes model output to a log file
#' 
#' Calculates model metrics and writes them to a log file.
#' @param model_id a unique identifier for the model, e.g. an
#' loop index or UUID
#' @param confusion_matrix Intended tp be a caret confision matrix, 
#' but can be anything you can print
#' @param distribition intended to be a measure of the output distribution, 
#' but could be anything that supports print
#' @param custom (Default NULL) an optional additional (small) item to log
#' @param logpath the location to write the log: deaults to ./gs.log
#' @return  Nothing
#' @export
log_model_results <- function(
    model_id, 
    confusion_matrix, 
    distribition, 
    custom = NULL, 
    logpath = "./gs.log") {
    # append performance data to the logs for later comparison
    sink(file = logpath, append = TRUE)
    print("-------------------------------------------------------")
    print("---------------------- Model Data ---------------------")

    print(paste0("Model Type: RF (plsgenomics)"))
    print(paste0("Data Index: ", custom))
    print(paste0("Model UUID: ", model_id))
    print("---------------------- Confusion Matrix ---------------------")
    print(confusion_matrix)
    print("---------------------- Class Distribution ---------------------")
    print(distribition)
    print("-------------------------------------------------------")
    sink(NULL)
}

#' Adds a model to the grid search manifest
#' 
#' This function is used for tracking modeling experiments
#' by creating a manifest.  This includes a number of metrics that
#' are tracked in a CSV file.  See below for a complete list of metrics
#' 
#' @param model_id (required), the unique identifier of the model
#' @param model_type (optional, default "") The type of model (any string)
#' @param max_count (optional, default "") The number of training samples used
#' @param bandwidth (optional, default "") The model bandwidth
#' @param max_correlation (optional, default "") The maximum inter-correlation
#' allowed between vairables
#' @param preprocessing (optional, default "") The preprocessing scheme used
#' @param weight (optional, default "") The class weights used 
#' @param hyperparam1 (optional, default "") a hyperparameter value
#' @param hyperparam2 (optional, default "") another hyperparameter value
#' @param accuracy (optional, default "") the model accuracy
#' @param r2 (optional, default "") the model r-squared
#' @param rpd (optional, defalut "") the Relative Percent Difference 
#' @param seed (optional, default "") The random seed used
#' @param logpath (optional, default ./gs_manifest.csv) the location to 
#' save the results
add_model_to_manifest <- function(
    model_id,
    model_type = "",
    max_count = "",
    bandwidth = "",
    max_correlation = "",
    preprocessing = "",
    weight = "",
    hyperparam1 = "",
    hyperparam2 = "",
    accuracy = "",
    r2 = "",
    rpd = "",
    seed = "",
    logpath = "./gs_manifest.csv") {
    if (!file.exists(logpath)) {
        header <- "maxCount,bandwidth,maxCorrelation,preprocessing,weight,hyperparam1,hyperparam2,accuracy,r2,rpd,seed,model_id"
        write(header, file = logpath)
    }

    line <- paste(
        max_count,
        bandwidth,
        max_correlation,
        preprocessing,
        weight,
        hyperparam1,
        hyperparam2,
        accuracy,
        r2,
        rpd,
        seed,
        sep = ","
    )
    line <- paste0(line, ",", model_id)

    write(line, file = logpath, append = TRUE)
}

#' filters a data.frame to have only vegetation indices
#' 
#' Extracts columns from a data.frame whose names correspond to
#' the vegetation indices supported by the hsdar R package.
#' This assumes that '/' and other characters that are not 
#' valid dataframe column names are simply removed (rather than,
#' for example, replacing them with '_').
#' 
#' @param df a data.frame
#' @return a data.frame with fewer columns
#' @export
filter_features <- function(df) {
    df_cols <- colnames(df) %>% as.character()
    target_cols <- c(
        "TCARIOSAVI",
        "TCARIO2SAVI2",
        "Vogelmann3",
        "Vogelmann4",
        "CI",
        "Carter",
        "Carter2",
        "Carter3",
        "Carter4",
        "Carter5",
        "Carter6",
        "D1",
        "D2",
        "Datt",
        "Datt3",
        "Datt5",
        "DPI",
        "EVI",
        "Gittelson",
        "PARS",
        "Maccioni",
        "mSR",
        "mSR705",
        "MTCI",
        "PRInorm",
        "REPLE",
        "REPLi",
        "SIPI"
    )

    used_cols <- setdiff(df_cols, target_cols)
    print(paste0(
        "Removed the following columns: ",
        intersect(df_cols, target_cols)
    ))
    return(df[, used_cols])
}

#' filters a data.frame to have a very specific list of columns
#' 
#' Filters a data.frame to have onlye the 10 most important
#' features for vegetation classification.  Specifically, this is
#' the features named "Vogelmann", "TVI", "CRI4", "Datt5",
#' "DWSI4", "GDVI", "MCARI", "MTVI", "NPCI", "PARS", "X417.593_5nm",
#' "X787.593_5nm", "X892.593_5nm"
#' 
#' @param df a data.frame
#' @return a data.frame with 
filter_target_subset <- function(df) {
    df_cols <- colnames(df) %>% as.character()
    target_cols <- c(
        "Vogelmann",
        "TVI",
        "CRI4",
        "Datt5",
        "DWSI4",
        "GDVI",
        "MCARI",
        "MTVI",
        "NPCI",
        "PARS",
        "X417.593_5nm",
        "X787.593_5nm",
        "X892.593_5nm"
    )

    used_cols <- intersect(df_cols, target_cols)
    print(paste0("Filtering Data frame to the following columns: ", used_cols))
    return(df[, used_cols])
}

#' trains a Random Forest (ranger) model
#' 
#' Trains a ranger classifier based on the supplied configuration
#' 
#' @param train_df the model training data (no labels)
#' @param train_labels the target labels for the training data
#' @param test_df The independent test data
#' @param test_labels the class labels for the test data
#' @param ntree (default 50) the number of trees for the classitier
#' @param max_depth (default NULL) the maximum tree depth for the model
#' @param mtry (default NULL) the model mtry, or number of predictors per tree
#' @param outlier_fn (default NULL) Outlier handling to used. None is used by
#' default, but a function can be supplied.  The function must take a single
#' argument (the training/test data)
#' and return a data frame
#' @param preprocessing_fn (default NULL) the pre-processing to be applied
#' to the data.  None is used by default, but a function can be
#' supplied.  The function must take a single argument (the training/test data)
#' and return a data frame
#' @param weight_fn (default targets_to_weights) the class weights to use, 
#' defaults to each class having equal representation
#' @param model_id (default uuid::UUIDgenerate()) a unique identifier for 
#' the model.  Generates a UUID version 4 by default
#' @param ignore_cols columns that should not be affected by outlier_fn or 
#' preproess_fn
#' @param seed (default NULL) a random seed to set.  If NULL, no seed is set.
#' @param log_string (default "") addionitional information for the logs (
#' e.g a batch id)
train_model <- function(
    train_df,
    train_labels,
    test_df,
    test_labels,
    ntree = 50,
    max_depth = NULL,
    mtry = NULL,
    outlier_fn = NULL,
    preprocess_fn = NULL,
    weight_fn = targets_to_weights,
    model_id = uuid::UUIDgenerate(),
    ignore_cols = NULL,
    seed = NULL,
    log_string = "") {
    if (!is.null(seed)) {
        set.seed(seed)
    }

    x_train <- train_df %>% as.data.frame()
    x_test <- test_df %>% as.data.frame()
    if (is.function(outlier_fn)) {
        x_train <- outlier_fn(x_train)
    }
    if (is.function(preprocess_fn)) {
        x_train <- preprocess_fn(x_train)
        x_test <- preprocess_fn(x_test)
    }

    sample_perm <- permute::shuffle(length(train_labels))
    sample_index <- create_stratified_sample(
        train_labels,
        permutation = sample_perm,
        samples_per_pft = 300
    )
    x_train <- x_train[sample_perm, ][sample_index, ]
    train_labels <- train_labels[sample_perm][sample_index] %>% as.factor()

    model <- ranger::ranger(
        num.trees = ntree,
        mtry = mtry,
        max.depth = max_depth,
        replace = TRUE,
        # case.weights = weight_fn(train_labels),
        classification = TRUE,
        x = x_train,
        y = train_labels
    )

    if (("Forb" %in% levels(train_labels)) && !("Forb" %in% levels(test_labels))) {
        levels(test_labels) <- c(levels(test_labels), "Forb")
    }

    # create predictions (ranger)
    model_predictions <- predict(
        model,
        x_test
    )$prediction %>% as.factor()

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

    # generate an id to uniquely identify the model
    # model_id <- uuid::UUIDgenerate()

    # append performance data to the logs for later comparison
    log_model_results(
        model_id = model_id,
        confusion_matrix = confusion_matrix,
        custom = log_string,
        distribition = model_predictions %>% as.factor() %>% table(),
        logpath = "./gs_pls_lda.log"
    )

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


#' Calculates the class weights to match a posterior distribution
#' 
#' Loads the validation data for a model and then creates 
#' weights for each output class to make the training data 
#' weights match the frequency in the posterior (target) distribution.
#' 
#' @param validation_path the file of validation data to load
#' @return a list of weights by class
#' @seealso get_posterior_weights_from_targets
#' @export
calculate_posterior_weights <- function(
    validation_path = "figures/merged_validation_s.csv") {
    validation_df <- read.csv(validation_path, header = TRUE)
    # print(head(validation_df))

    total_observations <- sum(validation_df$validation_counts)
    # print(total_observations)
    weights <- (1 / validation_df$validation_prop)
    # print(validation_df$validation_prop)

    total_by_fg1 <- aggregate(
        x = validation_df$validation_counts,
        by = list(validation_df$key),
        FUN = sum
    )

    fg1_weight_list <- list()

    for (row_idx in seq(nrow(total_by_fg1))) {
        name <- total_by_fg1$Group.1[[row_idx]]
        value <- total_by_fg1$x[[row_idx]]
        fg1_weight_list[name] <- value
    }

    return(fg1_weight_list)
}


#' Calculates class weights from for training data
#' 
#' Calculates the weights for each training sample such that it
#' matches the posterior (target) distribution.
#' 
#' @param target_factor a factor vector of target labels for a 
#' classifier
#' @param posterior the frequency (or relative frequency) of each class
#' from the posterior distribution.  By default, it is calculated
#' from `calculate_posterior_weights`
#' @param unbiased_weights the weights (for each class) that would 
#' make each class eaqually likely.  By default this is calculated
#' for you by targets_to_weights(target_factor)
#' @return a numeric vector of class weights for each observation
#' @export 
#' @seealso calcualte_posterior_weights, targets_to_weights
get_posterior_weights_from_targets <- function(
    target_factor, 
    posterior_weight = calculate_posterior_weights()) {
    unbiased_weights <- targets_to_weights(target_factor)

    target_name_char <- target_factor %>% as.character()

    output_weights <- seq_along(target_factor)

    for (i in seq_along(target_factor)) {
        if (posterior_weight[[target_name_char[[i]]]] > 0) {
            fg1_weight <- 1 / posterior_weight[[target_name_char[[i]]]]
        } else {
            fg1_weight <- 0
        }
        output_weights[[i]] <- unbiased_weights[[i]] * fg1_weight
    }

    return(output_weights)
}

#' Converts a factor to functional group 0
#' 
#' Converts a factor from another taxonomic level to 
#' functional group 0 (the coarsest)
#' 
#' Specific to the arctic use-case
#' 
#' @param target_factor a factor to convert
#' @return a factor of the entries converted to the
#' coarest taxonomic resolution
to_fg0 <- function(target_factor) {
    return(
        change_aggregation(
            target_factor,
            0,
            rjson::fromJSON(file = "./assets/pft_adj_list.json")
        ) %>%
            as.factor()
    )
}

#' Adds 'forb' to a factor.
#' 
#' Extends the levels of a factor to include "Forb"
#' 
#' @param factor_vec a factor
#' @return the factor with an additional level
add_forb <- function(factor_vec) {
    f_vec <- factor_vec %>% as.factor()

    if (!("Forb" %in% levels(f_vec))) {
        levels(f_vec) <- c(levels(f_vec), c("Forb"))
    }

    return(f_vec)
}


#' trains a PLS-DA model
#' 
#' Trains a pls classifier based on the supplied configuration
#' 
#' @param train_df the model training data (no labels)
#' @param train_labels the target labels for the training data
#' @param test_df The independent test data
#' @param test_labels the class labels for the test data
#' @param n (default 32) the number of trees for the classitier
#' @param outlier_fn (default NULL) Outlier handling to used. None is used by
#' default, but a function can be supplied.  The function must take a single
#' argument (the training/test data)
#' and return a data frame
#' @param preprocessing_fn (default NULL) the pre-processing to be applied
#' to the data.  None is used by default, but a function can be
#' supplied.  The function must take a single argument (the training/test data)
#' and return a data frame
#' @param weight_fn (default targets_to_weights) the class weights to use, 
#' defaults to each class having equal representation
#' @param model_id (default uuid::UUIDgenerate()) a unique identifier for 
#' the model.  Generates a UUID version 4 by default
#' @param ignore_cols columns that should not be affected by outlier_fn or 
#' preproess_fn
#' @param seed (default NULL) a random seed to set.  If NULL, no seed is set.
#' @param log_string (default "") addionitional information for the logs (
#' e.g a batch id)
train_pls_lda <- function(
    train_df,
    train_labels,
    test_df,
    test_labels,
    n = 32,
    outlier_fn = NULL,
    preprocess_fn = NULL,
    weight_fn = targets_to_weights,
    model_id = uuid::UUIDgenerate(),
    ignore_cols = NULL,
    save_path = "./mle/models/gs/",
    seed = NULL,
    log_string = "") {
    if (!is.null(seed)) {
        set.seed(seed)
    }

    x_train <- train_df %>% as.data.frame()
    x_test <- test_df %>% as.data.frame()
    if (is.function(outlier_fn)) {
        x_train <- outlier_fn(x_train)
    }
    if (is.function(preprocess_fn)) {
        x_train <- preprocess_fn(x_train)
        x_test <- preprocess_fn(x_test)
    }

    if (("Forb" %in% levels(train_labels)) && !("Forb" %in% levels(test_labels))) {
        levels(test_labels) <- c(levels(test_labels), "Forb")
    }

    train_ctrl <- caret::trainControl(
        method = "repeatedcv",
        number = 10,
        sampling = "up",
        repeats = 3
    )
    pls_model <- caret::train(
        x_train,
        train_labels,
        maxit = 100000,
        method = "pls",
        preProcess = c("center", "scale"),
        weights = weight_fn(train_labels),
        trControl = train_ctrl,
        tuneLength = n
    )

    print(pls_model)

    save(
        pls_model,
        file = file.path(save_path, paste0(model_id, ".rda"))
    )

    # create predictions (ranger)
    model_predictions <- predict(
        pls_model,
        newdata = x_test
    )

    print(model_predictions)


    # generate the confusion matrix

    confusion_matrix <- caret::confusionMatrix(
        test_labels,
        model_predictions %>% as.factor(),
        mode = "everything"
    )

    log_model_results(
        model_id = model_id,
        confusion_matrix = confusion_matrix,
        custom = log_string,
        distribition = model_predictions %>% as.factor() %>% table(),
        logpath = "./gs_pls_n.log"
    )

    return(
        list(
            model = pls_model,
            confusion = confusion_matrix %>% as.list()
        )
    )
}

#' create a train-test split that stratifies twice
#' 
#' Selects `test_count` observations from the data.frame provided, 
#' where an equal representation of 'class_col' classes while 
#' selecting observations evenly from across levels of `patch_col`.
#' 
#' Observations are selected from each 'patch' until the target number
#' of observations from each 'class' are selected.  This is intended for 
#' creating class-balanced samples that equally represent observational units 
#' (digitized patches in geospatial research, institutions in a clinical trial,
#' etc).  
#' 
#' The balancing of the class variable is exact provided there is sufficient 
#' data.  If there is not sufficient data, then all of the observations for 
#' that class are returned.  Patch-balancing is done on a best-effort round-
#' robin basis where the patches are as evenly sampled as possible.
#' 
#' @param data a data.frame of observations to be sampled
#' @param patch_col The name of the column in data that should
#' be used for assignment of observations
#' @param class_col the labels that should be balanced.
#' @param test_count the number of observations for each class \
#' @param train_count the maximum number of samples to keep in the 
#' data not selected for the test set.  That is, the remaining data will
#' have no more than train_count samples.
#' @param seed (optional, default NULL) a random seed for the sampling.  
#' You can also manually call set.seed, but this is provided for convenience
#' and reproducalbility
#' @param verbose (optional, default false) determines whether to print
#' information to stdout.  If false, stdout will be kept clean.  If true, it
#' prints the internal state of the sampler.
#' @return a list with four entries; 
#' - selection (the balanced sample)
#' - remainder the remaining data, up to train_count samples per class
#' - samples_per_patch the number of samples from each patch
#' for verification purposes
#' - samples_per_class the number of samples per class, again for verification
#' @export 
create_patch_balanced_sample <- function(
    data,
    patch_col = "UID",
    class_col = "FncGrp1",
    test_count = 15L,
    train_count = 300L, # rename this to reflect function
    seed = NULL,
    verbose = FALSE) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    train_samples <- NULL
    test_samples <- NULL

    split_data <- list()
    patches_by_pft <- list()
    samples_per_patch <- list()
    samples_per_pft <- list()

    split_levels <- unique(as.character(data[, patch_col]))
    class_levels <- unique(as.character(data[, class_col]))

    count_per_patch <- as.list(table(as.character(data[, patch_col])))


    for (li in class_levels) {
        patches_by_pft[[li]] <- list()
        samples_per_pft[[li]] <- 0
    }

    for (p in split_levels) {
        samples_per_patch[[p]] <- 0
    }

    if (verbose) {
        print("Building data for the following classes:")
        print(class_levels)
        print("Splitting evenly across the following patches")
        print(split_levels)
    }

    for (i in seq_along(split_levels)) {
        # extract data for the given level
        patch_id <- split_levels[[i]]
        filtered_data <- data[data[, patch_col] == patch_id, ]
        # get the PFT for the patch
        patch_pft <- filtered_data[1, class_col]
        # store the data in the data structures for the processing
        split_data[[patch_id]] <- filtered_data
        patches_by_pft[[patch_pft]] <- append(
            patches_by_pft[[patch_pft]],
            patch_id
        )
    }

    # calculate the number of samples from each patch
    for (j in seq_along(class_levels)) {
        # some
        pft <- class_levels[[j]]
        for (k in 1:(test_count)) {
            for (m in seq_along(patches_by_pft[[pft]])) {
                if (samples_per_pft[[pft]] < test_count) {
                    current_patch_id <- patches_by_pft[[pft]][[m]]
                    current_patch_size <- count_per_patch[[current_patch_id]]
                    if (samples_per_patch[[current_patch_id]] < current_patch_size) {
                        samples_per_patch[[current_patch_id]] <- samples_per_patch[[current_patch_id]] + 1
                        samples_per_pft[[pft]] <- samples_per_pft[[pft]] + 1
                    }
                }
            }
        }
    }

    if (verbose) {
        print("Samples per patch")
        print(samples_per_patch)
        print("Samples per PFT")
        print(samples_per_pft)
    }


    # get the samples from the patches
    for (st in split_levels) {
        df <- split_data[[st]]
        n_samp <- samples_per_patch[[st]]
        permutation <- permute::shuffle(nrow(df))
        if (n_samp > 0) {
            test_indices <- permutation[1:n_samp]

            if (is.null(test_samples)) {
                test_samples <- df[test_indices, ]
            } else {
                test_samples <- rbind(test_samples, df[test_indices, ])
                if (verbose) {
                    print(table(test_samples$FncGrp1))
                }
            }
        }

        num_train <- min(train_count, nrow(df) - test_count)
        train_indices <- permutation[(n_samp + 1):(num_train + test_count)] %>% 
            as.numeric()
        if (is.null(train_samples)) {
            train_samples <- df[train_indices, ]
        } else {
            train_samples <- rbind(train_samples, df[train_indices, ])
        }
    }

    # verify that the number of samples of each class matches,
    # otherwise sample more at random from

    for (pft in class_levels) {

    }

    if (sum(unlist(samples_per_patch)) != nrow(test_samples)) {
        warning("the number of rows in the dataframe are not as expected")
    }

    return(list(
        selection = test_samples,
        remainder = train_samples,
        samples_per_patch = samples_per_patch,
        samples_per_class = samples_per_pft
    ))
}



#' removes inter-correlated vaiables from a data frame
#' 
#' Sequentially removes inter-correlated variables from 
#' a data.frame.  The order of selection of columns
#' can be tuned by the user, as can the threshold for elmimination 
#' and the method of calculating the correlations
#' 
#' @param data the data.frame of observations
#' @param col_order the ordering of columns for selection.  
#' The entries in this character vector should be the 
#' names of columns in data.  The order of the entries in 
#' this vector will be the order of preference for the 
#' coulumns used.
#' @param threshold (default 0.95) the maximum cutoff for the
#' correlation between variables.  
#' @param method ("pearson" or "spearman") determines the way
#' the correlation coefficients are correlated
#' @return a data.frame with inter-correlated 
#' columns removed
#' @export 
remove_intercorrelated_variables <- function(
    data,
    col_order,
    threshold = 0.95,
    method = "pearson") {
    correlation_matrix <- cor(
        data[, col_order],
        use = "pairwise.complete.obs",
        method = method
    )

    print(setdiff(col_order, colnames(data)))

    is_included <- rep_len(TRUE, length(col_order))

    for (i in seq_along(col_order)) {
        if (is_included[[i]]) {
            if (i < length(col_order) - 1) {
                for (j in (i + 1):length(col_order)) {
                    if (!is.nan(correlation_matrix[i, j]) &&
                        !is.na(correlation_matrix[i, j]) &&
                        !is.null(correlation_matrix[i, j])) {
                        if (abs(correlation_matrix[i, j]) > threshold) {
                            is_included[[j]] <- FALSE
                        }
                    }
                }
            }
        }
    }

    return(data[, col_order[is_included]])
}
