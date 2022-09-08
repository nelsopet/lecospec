# load main "package"
source("Functions/lecospectR.R")

# Add libraries for model training
library(caret)
library(ranger)

USE_CAL_VAL_SPLIT <- FALSE
ADD_NOISE <- FALSE


# define hyperparameters
NOISE_POWER <- 0.00
TRAINING_PROPORTION <- 0.8


model_filepath <- "mle/fg1_model_normed_weighted.rda"
training_filepath <- "mle/training_data_redux.csv"
test_filepath <- "mle/test_data_redux.csv"

speclib_filepath <- "Output/D_002_SpecLib_Derivs.csv"
speclib <- read.csv(speclib_filepath, header = TRUE)

total_observations <- nrow(speclib)
print(colnames(speclib)[[1]])
total_by_genus <- speclib %>% group_by(Genus) %>% tally()
total_by_genus$weights <- 0.00001 + (5 + total_by_genus$n) / total_observations
print(total_by_genus)

genus_weights <- list()
for(row_idx in seq(nrow(total_by_genus))){
    genus_i <- stringr::str_to_lower(total_by_genus$Genus[[row_idx]]) 
    genus_weights[genus_i] <- total_by_genus$weights[[row_idx]]
}
print(genus_weights)

get_weight_by_genus <- function(genus_name){
    return(genus_weights[[stringr::str_to_lower(genus_name)]])
}
adjancy_list <- rjson::fromJSON(file = "./assets/pft_adj_list.json")

metadata_columns_dropped <- c(
  "ScanID", "Area", "Code_name", "Species_name", "Functional_group1",
  "Functional_group2", "Species_name_Freq", "Functional_group1_Freq",
  "Functional_group2_Freq", "Genus", "Version", "File.Name.1", "Instrument",
  "Detectors", "Measurement", "Date", "Time", "Battery.Voltage.1", "Averages",
  "Integration1", "Integration2", "Integration3", "Dark.Mode.1", "Foreoptic",
  "Radiometric.Calibration.1", "Units", "Latitude", "Longitude", "Altitude",
  "GPS.Time.1", "Satellites", "Calibrated.Reference.Correction.File.1",
  "Channels", "File.Name", "Battery.Voltage", "Dark.Mode",
  "Radiometric.Calibration", "GPS.Time",
  "Calibrated.Reference.Correction.File", 
  "weights"
)


weights <- lapply(X = speclib$Genus, FUN = get_weight_by_genus)
used_cols <- setdiff(colnames(speclib), metadata_columns_dropped)
training_data <- speclib[, used_cols]
print(colnames(training_data))

print(weights)

if(USE_CAL_VAL_SPLIT){


    data <- caret::createDataPartition(
        speclib$Functional_group1,
        times = 1,
        p = TRAINING_PROPORTION,
        list = TRUE,
        groups = 9,
        we
    )

    print(names(data))

    training_data <- speclib[data$Resample1,]

    test_data <- speclib[-data$Resample1,]
    print(head(training_data))
    print(head(test_data))

    data_cols <- colnames(training_data)[40:length(training_data)]
    print(data_cols)
    num_observations <- nrow(training_data)
}

if(ADD_NOISE){


    for(col_index in seq_along(data_cols)){
        active_column <- data_cols[col_index]
        training_data[, active_column] <- training_data[, active_column] + 
            rnorm(num_observations, 0, NOISE_POWER)
    }

    print(head(training_data))
}

write.csv(training_data, training_filepath)
write.csv(test_data, test_filepath)

speclib$weights <- as.numeric(weights)

# the data is now noised   We can move on to build some models
###############################
## Ranger
###############################
fg1_model <- ranger::ranger(
    #data = training_data[data_cols],
    num.trees = 256,
    importance = "impurity",
    replace = TRUE,
    #verbose = TRUE,
    case.weights = speclib$weights,
    classification = TRUE,
    x = speclib[, used_cols],
    y = as.factor(speclib$Functional_group1)
)

print(base::min(as.numeric(weights)))

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
data_file <- base::tempfile(fileext = ".data")
lightgbm::lgb.Dataset.save(gb_data, data_file)
dtrain <- lightgbm::lgb.Dataset(data_file, colnames = data_cols)
lightgbm::lgb.Dataset.construct(gb_data)

gb_model <- lightgbm::lightgbm(dtrain, objective = 'multiclass')
