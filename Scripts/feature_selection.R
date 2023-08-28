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
manifest_path <- "./feature_selection.mainfest.csv"
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

model <- ranger::ranger(
        num.trees = 10000,
        replace = TRUE,
        case.weights = targets_to_weights(labels),
        classification = TRUE,
        alpha = 1,
        importance = "impurity_corrected",
        x=train_data,
        y=labels
    )
var_imp <- as.data.frame(sort(model$variable.importance, decreasing = TRUE))
colnames(var_imp) <- c("importance")
var_imp
write.csv(var_imp, file="./assets/variable_importance.csv")
sqrt(ncol(train_data))
######################################################
## Progressive Elimination Feature Selection
######################################################
#column_shuffle <- permute::shuffle(ncol(train_data))
#features <- colnames(test_data)[column_shuffle]
features <- row.names(var_imp)[2:nrow(var_imp)]
print(features)

head(var_imp)

remove_intercorrelated_variables <- function(data, col_order, threshold = 0.95){

    correlation_matrix <- cor(data[, col_order], method = "spearman")

    is_included <- rep_len(TRUE, length(col_order))

    for(i in seq_along(col_order)){
        if(is_included[[i]]){
            if(i < length(col_order) - 1){

            for(j in (i+1):length(col_order)){
                if(abs(correlation_matrix[i,j]) > threshold){
                    is_included[[j]] <- FALSE
                }
            }
            }
        }
    }

    return(data[,col_order[is_included]])
}

uncorrelated_features <- remove_intercorrelated_variables(
    train_data,
    row.names(var_imp),
    threshold = 0.8
)
print(colnames(uncorrelated_features))
print(colnames(train_data))
included_columns <- c(        UID,
    colnames(uncorrelated_features)[1]
)
cor(train_data$NPCI, train_data$SRPI)
svm_grid <- expand.grid(
    cost = c(1,3,6,9,12,15,18),
    Loss = c("L1")
)

training_options <- caret::trainControl(
    method = "boot",
    number = 100,
    search = "random"
)
training_options <- caret::trainControl(
    method = "boot",
    number = 10,
    search = "grid"
)


removed_features <- c()
best_metric <- -1.0

best_model <- NULL# initialize with model with everything

for(feature in colnames(uncorrelated_features)[2:ncol(uncorrelated_features)]){

    used_cols <- c(included_columns, c(feature))
    # train model with the specified columns
    print(used_cols)
    
        

    model_id <- uuid::UUIDgenerate()
    model_dir <- paste0("mle/experiments/manual/", model_id, "/")
    set.seed(61718)
    dir.create(model_dir)
    print(model_dir)

    row_balance <- create_stratified_sample(
            labels, 
            samples_per_pft = 77
        )

    

    n_comp <- 8 #min(length(used_cols), 32)
    model <- ranger::ranger(
            num.trees = n_comp,
            replace = TRUE,
            case.weights = targets_to_weights(labels),
            classification = TRUE,
            #alpha = 0.75,
            x = train_data[, used_cols],
            y = labels
        )
colnames(train_data)
uncorrelated_features
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
        preprocessing = "no_treatment",
        source = "v2",
        weight = "balanced",
        n = n_comp,
        oob_error = model$prediction.error,
        accuracy = acc,
        r2 = r2,
        chi2prob = rpd,
        seed = "Forward Feature Selection",
        logpath = manifest_path
    )


    print(paste0("Model r-squared: ", r2))



    # calculate acc and r2
    current_metric <- acc * r2
    
    if(current_metric > best_metric){
        print("New best model!")
        print(paste0("Adding variable: ", feature))
        best_model <- model
        best_metric <- current_metric
        included_columns <- used_cols
    } else {
        removed_features <- append(removed_features, feature)
        print(paste0("Removing variable: ", feature))
    }

}

print(included_columns)

write(included_columns, file = "./included_cols_svm.txt")
print(removed_features)







print(var_imp)
var_imp$feature <- row.names(var_imp)
print(var_imp[1,])
head(var_imp)
as.data.frame(model$variable.importance)

X11();plot(
    train_data[, "SRPI"],
    train_data[, "NPCI"],
    )

pairs(train_data[,included_columns])

