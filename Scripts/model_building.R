source("Functions/lecospectR.R")

#########################################################
##     Load the Data
########################################################

# spectral library
base_path <- "./Output/C_001_SC3_Cleaned_SpectralLib.csv"
veg_index_path <- "./Data/D_002_SpecLib_Derivs.csv"
speclib <- read.csv(base_path)
veg_indices <- read.csv(veg_index_path)


# Targets 
targets <- veg_indices[!is.na(veg_indices$Functional_group1),"Functional_group1"] %>% as.factor()
print(summary(targets))
print(length(targets))
# weights
weights_by_pft <- targets_to_weights(targets)
print(length(weights_by_pft))# image-based validation data
min(weights_by_pft)

# image-based validation
image_validation <- read.csv(
    "Data/Ground_Validation/training/base.csv"
)

#########################################################
##     Clean + PCA
########################################################

print(colnames(veg_indices))
numeric_data <- veg_indices[!is.na(veg_indices$Functional_group1),32:ncol(veg_indices)]
print(nrow(numeric_data))
numeric_data <- inf_to_na(numeric_data)
imputed_data <- impute_spectra(numeric_data)
pca_fit <- stats::prcomp(imputed_data, center = TRUE, scale. = TRUE)
print(summary(pca_fit))
pca_training_data <- predict(pca_fit, imputed_data)[,1:30]

#########################################################
##     Define Grid Search Parameters
########################################################

datasets <- c(
    #imputed_data,
    pca_training_data
)

tree_counts <- c(
    256
    #512,
    #1024, 
    #1536,
    #2048
)

weights <- c(
    weights_by_pft

)


print(length(weights_by_pft))
print(length(data))
print(length(targets))

#########################################################
##     Run
########################################################

print(length(weights_by_pft))
print(length(pca_training_data))
print(length(targets))

model <- ranger::ranger(
    num.trees = 256,
    case.weights = weights_by_pft,
    classification = TRUE,
    x = pca_training_data %>% as.data.frame(),
    y = targets
)

print(model)

print(model$prediction.error)

pca_rf_comb <- LSModel(
    list(
        pca = pca_fit,
        rf = model
    ),
    function(df, model, ...){
        pca_df <- predict(model$pca, df)
        predictions <- predict()
    }
)
