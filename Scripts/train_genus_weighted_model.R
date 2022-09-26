source("Functions/lecospectR.R")
library(caret)
library(ranger)
library(xgboost)


genus_validation_df <- read.csv("figures/merged_validation_t.csv", header = TRUE)
print(head(genus_validation_df))

total_observations <- sum(genus_validation_df$validation_counts)
print(total_observations)
weights <- (1/ genus_validation_df$validation_prop)
print(genus_validation_df$validation_prop)

total_by_genus <- aggregate(
    x = genus_validation_df$validation_counts,
    by = list(genus_validation_df$key),
    FUN = sum
)
print(total_by_genus)

total_by_genus$weights <- (5 + total_by_genus$x) / total_observations
print(dim(total_by_genus))

genus_weights <- list()
for(row_idx in seq(nrow(total_by_genus))){
    genus_i <- stringr::str_to_lower(total_by_genus$Group.1[[row_idx]]) 
    genus_weights[genus_i] <- total_by_genus$weights[[row_idx]]
}
print(genus_weights)

genus_weights_json <- rjson::toJSON(genus_weights)
write(genus_weights_json, file = "mle/fg1_weights.json")

get_weight_by_genus <- function(genus_name){
    return(genus_weights[[stringr::str_to_lower(genus_name)]])
}
adjancy_list <- rjson::fromJSON(file = "./assets/pft_adj_list.json")


################################################
##        Train the Model
################################################

base_data_path <- "Output/D_002_SpecLib_Derivs.csv"
augmented_data_path <- "./mle/validation100_stratified.csv"
spec_lib <- read.csv(base_data_path)

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
print(weights)
write.csv(weights, "mle/pweights.csv")
json_weights_str <- rjson::toJSON(weights)
write(json_weights_str, file = "mle/pweights.json")

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
