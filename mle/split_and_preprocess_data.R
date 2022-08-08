# load main "package"
source("Functions/lecospectR.R")

# Add libraries for model training
library(caret)
library(sf)
library(ranger)
library(tidyverse)

# define hyperparameters
NOISE_POWER <- 0.01
TRAINING_PROPORTION <- 0.8


model_filepath <- "mle/fg1_model.rda"
training_filepath <- "mle/training_data_redux.csv"
test_filepath <- "mle/test_data_redux.csv"

speclib_filepath <- "Output/D_002_SpecLib_Derivs.csv"
speclib <- read.csv(speclib_filepath, header = TRUE)


print(colnames(speclib)[[1]])


data <- caret::createDataPartition(
    speclib$Functional_group1,
    times = 1,
    p = TRAINING_PROPORTION,
    list = TRUE,
    groups = 9
)

print(names(data))

training_data <- speclib[data$Resample1,]
test_data <- speclib[-data$Resample1,]
print(head(training_data))
print(head(test_data))

data_cols <- colnames(training_data)[40:length(training_data)]
print(data_cols)
num_observations <- nrow(training_data)

for(col_index in seq_along(data_cols)){
    active_column <- data_cols[col_index]
    training_data[, active_column] <- training_data[, active_column] + 
        rnorm(num_observations, 0, NOISE_POWER)
}

print(head(training_data))

write.csv(training_data, training_filepath)
write.csv(test_data, test_filepath)


# the data is now noised   We can move on to build some models
###############################
## Ranger
###############################
fg1_model <- ranger::ranger(
    #data = training_data[data_cols],
    num.trees = 128,
    importance = "impurity_corrected",
    replace = TRUE,
    #verbose = TRUE,
    classification = TRUE,
    x = training_data[,data_cols],
    y = as.factor(training_data[,"Functional_group1"])
)

print(fg1_model)

save(fg1_model, file = model_filepath)


############################
## PCA/LDA
############################

library(MASS)

pca <- prcomp(training_data[,data_cols],
              center = TRUE,
              scale. = TRUE)

prop_pca <- pca$sdev^2/sum(pca$sdev^2)

pca_lda <- MASS::lda(pca$x, grouping=training_data[,"Functional_group1"])
print(pca_lda)

###########################
## Gradient Boosting
###########################

library(lightgbm)

lightgbm_formatted_data_path <- "mle/lgbm_train.csv"
write.csv(training_data[,data_cols], lightgbm_formatted_data_path)
#lightgbm_formatted_target_path <- "mle/lgbm_target.csv"

gb_data <- lightgbm::lgb.Dataset(
    as.matrix(training_data[,data_cols]), 
    label = training_data[, "Functional_group1"]
)
data_file <- tempfile(fileext = ".data")
lightgbm::lgb.Dataset.save(gb_data, data_file)
dtrain <- lightgbm::lgb.Dataset(data_file)
lightgbm::lgb.Dataset.construct(dtrain)

gb_model <- lightgbm::lightgbm(dtrain,
    params = {
        "objective": "multiclass"
    })
