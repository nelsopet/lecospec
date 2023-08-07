
#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
add_noise <- function(df, noise_power, used_cols = NULL){
    output_df <- df
    total_observations <- nrow(df)
    for (col_index in seq_along(used_cols)) {
        active_column <- used_cols[col_index]
        output_df[, active_column] <- output_df[, active_column] +
            rnorm(total_observations, 0, noise_power)
    }
    return(output_df)
}


#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
check_output_distribution <- function(df, model, targets){
    predictions <- apply_model(df, model)$. %>% as.factor()
    correct_observations <- 0
    print(head(predictions))
    print(head(targets))

    for(i in seq_along(predictions)){
        if( predictions[[i]] == targets[[i]]){
            correct_observations <- correct_observations + 1
        }
    }

    class_counts <- predictions  %>% table()

    return( 
        list(
            accuracy = correct_observations / nrow(df),
            counts = class_counts %>% as.list()
        ))
}

automated_check <- function(input_df, ignore_cols = NULL){
    used_cols <- colnames(input_df)
    if(!is.null(ignore_cols)){

        used_cols <- setdiff(colnames(input_df), ignore_cols)
    }

    df <- input_df[, used_cols]

    indx <- apply(df, 2, function(x) any(is.na(x) | is.infinite(x)))
    if(length(indx > 0)){
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

    return( list(
        maxs = column_max_values,
        mins = column_min_values,
        means = column_mean_values,
        medians = column_median_values,
        vars = column_variances
    ))
}

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
targets_to_weights <- function(target_factors){
    weights_by_pft <- target_factors %>% 
        table() %>% 
        purrr::map(., function(x) return(abs(1/x))) %>% 
        as.list() 
    weights_by_pft$Unknown <- 0# should not include data with no label

    return(purrr::map(
        target_factors %>% as.character(),
        function(x){
            return(weights_by_pft[[x]])
        }   
    ) %>% 
    as.numeric()
    )



}

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
test_transferrability <- function(df1, df2, ignore_cols = NULL){
    used_cols <- intersect(colnames(df1), colnames(df2))
        if(!is.null(ignore_cols)){
        used_cols <- setdiff(used_cols, ignore_cols)
    }

    results <- list()

    for(col in used_cols){
        if(is.numeric(df1[,col]) & is.numeric(df2[,col])){
            test_result <- stats::ks.test(
                df1[,col],
                df2[,col]
            )
        }

        results[[col]] <- test_result
    }
    return(results)
}


create_grid_search_registrator <- function(
    base_path
) {
    
}

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


log_model_results <- function(model_id, confusion_matrix, distribition, custom = NULL, logpath = "./gs.log"){
    # append performance data to the logs for later comparison
    sink(file = logpath, append = TRUE)
    print("-------------------------------------------------------")
    print("---------------------- Model Data ---------------------")
    
    print(paste0("Model Type: RF (plsgenomics)"))
    print(paste0("Data Index: ",custom))
    print(paste0("Model UUID: ", model_id))
    print("---------------------- Confusion Matrix ---------------------")
    print(confusion_matrix)
    print("---------------------- Class Distribution ---------------------")
    print(distribition)
    print("-------------------------------------------------------")
    sink(NULL)
}

add_model_to_manifest <- function(
    model_id, 
    outlier = "", 
    preprocessing = "",
    source = "", 
    weight = "",
    n = "",
    oob_error = "",
    accuracy = "",
    r2 = "",
    chi2prob = "",
    seed = "",
    logpath="./gs_manifest.csv"){
    if(!file.exists(logpath)){
        header <- "source,outliers,preprocessing,weight,n,oob,accuracy,r2,rpd,seed,model_id"
        write(header, file = logpath)
    }

    line <- paste(
        source,
        outlier,
        preprocessing,
        weight,
        n,
        oob_error,
        accuracy,
        r2,
        chi2prob,
        seed,
        sep=","
    )
    line <- paste0(line, ",", model_id)

    write(line, file=logpath, append = TRUE)
}


filter_features <- function(df){

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
    print(paste0("Removed the following columns: ", 
    intersect(df_cols, target_cols)))
    return(df[,used_cols])
}


filter_target_subset <- function(df){

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
    return(df[,used_cols])
}


read_pls_lda_model <- function(pls_lda_obj){
    x_train

}


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

    sample_perm <- permute::shuffle(length(train_labels))
    sample_index <- create_stratified_sample(
        train_labels,
        permutation = sample_perm,
        samples_per_pft = 300
    )
    x_train <- x_train[sample_perm,][sample_index,]
    train_labels <- train_labels[sample_perm][sample_index] %>% as.factor()

    model <- ranger::ranger(
            num.trees = ntree,
            mtry = mtry,
            max.depth = max_depth,
            replace = TRUE,
            #case.weights = weight_fn(train_labels),
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
        #model_id <- uuid::UUIDgenerate()

        # append performance data to the logs for later comparison
        log_model_results(
            model_id = model_id,
            confusion_matrix = confusion_matrix,
            custom = log_string,
            distribition = model_predictions %>% as.factor() %>% table(),
            logpath = "./gs_pls_lda.log")

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


calculate_posterior_weights <- function(validation_path ="figures/merged_validation_s.csv" ){

    validation_df <- read.csv(validation_path, header = TRUE)
    #print(head(validation_df))

    total_observations <- sum(validation_df$validation_counts)
    #print(total_observations)
    weights <- (1 / validation_df$validation_prop)
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


to_fg0 <- function(target_factor){
    return(
        change_aggregation(
            target_factor,
            0,
            rjson::fromJSON(file="./assets/pft_adj_list.json")
        ) %>% 
        as.factor()
    )
}

add_forb <- function(factor_vec){
    f_vec <- factor_vec %>% as.factor()

    if(!("Forb" %in% levels(f_vec))){
        levels(f_vec) <- c(levels(f_vec), c("Forb"))

    }

    return(f_vec)
}



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

    if(("Forb" %in% levels(train_labels)) && !("Forb"  %in% levels(test_labels))){
        levels(test_labels) <- c(levels(test_labels), "Forb")
        }
        
    train_ctrl <- caret::trainControl(
        method = "repeatedcv",
        number = 10,
        sampling = 'up',
        repeats = 3
    )
    
    pls_model <- caret::train(
        x_train, 
        train_labels, 
        maxit = 100000,
        method="pls",
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
            logpath = "./gs_pls_n.log")

    return(
        list(
                model = pls_model,
                confusion = confusion_matrix %>% as.list()
                )
    )

    

}


extract_features <- function(data){


    return(
        subset(
            data, 
            select = c(

            )
        )
    )

}


create_patch_balanced_sample <- function(
    data, 
    patch_col = "UID", 
    class_col = "FncGrp1", 
    test_count = 15L, 
    train_count = 300L,# rename this to reflect function
    seed = NULL,
    verbose = FALSE){

        if(!is.null(seed)){
            set.seed(seed)
        }
        train_samples <- NULL
        test_samples <- NULL

        split_data <- list()
        patches_by_pft <- list()
        samples_per_patch <- list()
        samples_per_pft <- list()

        split_levels <- unique(as.character(data[,patch_col]))
        class_levels <- unique(as.character(data[,class_col]))

        count_per_patch <- as.list(table(as.character(data[,patch_col])))
        
        
        for(li in class_levels){
            patches_by_pft[[li]] <- list()
            samples_per_pft[[li]] <- 0
            
        }

        for(p in split_levels){
            samples_per_patch[[p]] <- 0
        }

        if(verbose){
            print("Building data for the following classes:")
            print(class_levels)
            print("Splitting evenly across the following patches")
            print(split_levels)
        }

        for( i in seq_along(split_levels)){
            # extract data for the given level
            patch_id <- split_levels[[i]]
            filtered_data <- data[data[,patch_col] == patch_id, ]
            # get the PFT for the patch
            patch_pft <- filtered_data[1,class_col]
            # store the data in the data structures for the processing
            split_data[[patch_id]] <- filtered_data
            patches_by_pft[[patch_pft]] <- append(
                patches_by_pft[[patch_pft]],
                patch_id
                )
        }

        # calculate the number of samples from each patch
        for(j in seq_along(class_levels)){
            # some
            pft <- class_levels[[j]]
            for(k in 1:(test_count)){
                for(m in seq_along(patches_by_pft[[pft]])){
                    if(samples_per_pft[[pft]] < test_count){
                        current_patch_id <- patches_by_pft[[pft]][[m]]
                        current_patch_size <- count_per_patch[[current_patch_id]]
                        if(samples_per_patch[[current_patch_id]] < current_patch_size){
                            samples_per_patch[[current_patch_id]] <- samples_per_patch[[current_patch_id]] + 1
                            samples_per_pft[[pft]] <- samples_per_pft[[pft]] + 1
                        }
                    }
                }
            }

        }

        if(verbose){
            print("Samples per patch")
            print(samples_per_patch)
            print("Samples per PFT")
            print(samples_per_pft)
        }
        

        # get the samples from the patches
        for(st in split_levels){
            
            df <- split_data[[st]]
            n_samp <- samples_per_patch[[st]]
            permutation <- permute::shuffle(nrow(df))
            if(n_samp > 0){

                test_indices <- permutation[1:n_samp]

                if(is.null(test_samples)){
                    test_samples <- df[test_indices, ]
                } else {
                    test_samples <- rbind(test_samples, df[test_indices,])
                    if(verbose){
                        print(table(test_samples$FncGrp1))
                    }

                }
            }

            num_train <- min(train_count, nrow(df) - test_count)
            train_indices <- permutation[(n_samp+1):(num_train + test_count)] %>% as.numeric()
            if(is.null(train_samples)){
                train_samples <- df[train_indices,]
            } else {
                train_samples <- rbind(train_samples, df[train_indices,])
            }
        }

        # verify that the number of samples of each class matches,
        # otherwise sample more at random from

        for(pft in class_levels){

        }

        if(sum(unlist(samples_per_patch)) != nrow(test_samples)){
            warning("the number of rows in the dataframe are not as expected")
        }

    return(list(
        selection = test_samples,
        remainder = train_samples,
        samples_per_patch = samples_per_patch,
        samples_per_class = samples_per_pft
        )
    )
}