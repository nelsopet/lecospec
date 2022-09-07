source("Functions/lecospectR.R")
library(caret)
library(ranger)
library(xgboost)


genus_validation_df <- read.csv("figures/merged_validation_t.csv", header = TRUE)
total_observations <- sum(genus_validation_df$validation_counts)
weights <- (1/ genus_validation_df$validation_prop)
print(genus_validation_df$validation_prop)

total_by_genus <- genus_validation_df %>% group_by(key) %>% 
    summarise(., total = sum(validation_counts))

total_by_genus$weights <- (5 + total_by_genus$total) / total_observations
print(total_by_genus)
total_by_genus %>% do(.,function(x){})


genus_weights <- list()
for(row_idx in seq(nrow(total_by_genus))){
    genus_i <- stringr::str_to_lower(total_by_genus$key[[row_idx]]) 
    genus_weights[genus_i] <- total_by_genus$weights[[row_idx]]
}
print(genus_weights)

get_weight_by_genus <- function(genus_name){
    return(genus_weights[[stringr::str_to_lower(genus_name)]])
}
adjancy_list <- rjson::fromJSON(file = "./assets/pft_adj_list.json")


################################################
##        Train the Model
################################################

base_data_path <- "Output/D_002_SpecLib_Derivs.csv"
augmented_data_path <- "./mle/validation100_stratified.csv"
spec_lib <- read.csv(augmented_data_path)

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
  "Calibrated.Reference.Correction.File"
)


weights <- lapply(X = spec_lib$Genus, FUN = get_weight_by_genus)
used_cols <- setdiff(colnames(spec_lib), metadata_columns_dropped)
training_data <- spec_lib[, used_cols]
print(colnames(training_data))

print(weights)

fg1_model <- ranger::ranger(
    #data = training_data[data_cols],
    num.trees = 128,
    importance = "impurity_corrected",
    replace = TRUE,
    case.weights = as.vector(weights, mode = "numeric"),
    verbose = TRUE,
    classification = TRUE,
    x = training_data,
    y = as.factor(spec_lib$Functional_group1)
)

save(fg1_model, file = "mle/sample_weighted_model.rda")
