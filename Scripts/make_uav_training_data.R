source("Functions/lecospectR.R")

uav_speclib_df <- read.csv(
    "Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv", 
    header = TRUE)

print(colnames(uav_speclib_df))

# separate metadata from the spectra
metadata_cols <- c(
    "X",
    "UID",
    "ScanNum",
    "sample_name",
    "PFT",
    "FncGrp1"
)
data_cols <- setdiff(colnames(uav_speclib_df), metadata_cols)
target_labels <- uav_speclib_df$FncGrp1
training_spectra <- uav_speclib_df[, data_cols]

# convert band names to something that correctly casts to numeric
colnames(training_spectra) <- extract_bands(training_spectra)

training_data_normed <- resample_df(
    training_spectra,
    normalize = TRUE
)

training_data_no_norm <- resample_df(
    training_spectra,
    normalize = FALSE
)
min_max_scaled <- global_min_max_scale(training_data_no_norm)

# load a model that uses all vegetation indices
target_model <- load_model(
    "Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"
)


veg_indices_normed <- get_vegetation_indices(
    training_data_normed,
    target_model
)

vegetation_indices_no_norm <- get_vegetation_indices(
    training_data_no_norm,
    target_model
)

scaled_veg_indices <- get_vegetation_indices(
    min_max_scaled,
    target_model
)

# combine spectra and veg indices
scaled_training_data <- cbind(min_max_scaled, scaled_veg_indices)
training_normed_joined <- cbind(training_data_normed ,veg_indices_normed)
training_base_joined <- cbind(training_data_no_norm ,vegetation_indices_no_norm)

write.csv(training_base_joined, "Data/Ground_Validation/training/base.csv")
write.csv(training_normed_joined, "Data/Ground_Validation/training/normed.csv")
write.csv(scaled_training_data, "Data/Ground_Validation/training/scaled.csv")

# write the training labels (as JSON)
json_targets <- rjson::toJSON(uav_speclib_df$FncGrp1)
write(json_targets, file = "Data/Ground_Validation/training/labels.json")