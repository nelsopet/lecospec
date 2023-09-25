source("Functions/lecospectR.R")
library(class)

#########################################################
##     Load the Data
########################################################

# spectral library
base_path <- "./Output/C_001_SC3_Cleaned_SpectralLib.csv"
veg_index_path <- "./Data/D_002_SpecLib_Derivs.csv"
speclib <- read.csv(base_path)
veg_indices <- read.csv(veg_index_path)
print(dim(veg_indices))
print(colnames(veg_indices))

# Targets 
targets <- veg_indices[!is.na(veg_indices$Functional_group1),"Functional_group1"] %>% as.factor()
print(summary(targets))
print(length(targets))

# weights
weights_by_pft <- targets_to_weights(targets)
print(summary(weights_by_pft))# image-based validation data

# image-based validation
uav_speclib_df <- read.csv(
    "Data/Ground_Validation/PFT_image_spectra/PFT_Image_SpectralLib_Clean_unsmoothed.csv", 
    header = TRUE)
image_validation <- uav_speclib_df[,16:(ncol(uav_speclib_df) - 1)]
print(nrow(image_validation))
validation_labels <- uav_speclib_df$FncGrp1 %>% as.factor()
levels(validation_labels) <- c(
    levels(validation_labels),
    "Forb")
    print(length(validation_labels))
print(colnames(image_validation))

veg_index_names <- read.csv("assets/vegIndicesUsed.csv")$x
print(veg_index_names)
validation_indices <- get_vegetation_indices(image_validation, NULL)
# drop NAs
validation_indices <- inf_to_na(validation_indices)
dropped_rows <- !is.na(validation_indices$Carter)
validation_indices <- validation_indices[dropped_rows,]
print(summary(validation_indices))

validation_labels <- validation_labels[dropped_rows]
min_max_scaled_validation <- columnwise_min_max_scale(validation_indices)

image_weights <- targets_to_weights(validation_labels)
# get the veg indices for the I 
#########################################################
##     Clean + PCA
########################################################
library(vegan)
print(colnames(veg_indices))
numeric_data <- veg_indices[!is.na(veg_indices$Functional_group1),35:195]
print(nrow(numeric_data))
numeric_data <- inf_to_na(numeric_data)
imputed_data_1 <- impute_spectra(numeric_data)
imputed_data_no_outliers <- outliers_to_na(imputed_data_1)
imputed_data <- impute_spectra(imputed_data_no_outliers)
outlier_indices <- detect_outliers(imputed_data[,1:95])
filtered_data <- imputed_data[!outlier_indices,]
print(dim(filtered_data))
hist(dist(as.matrix(imputed_data)))
min_max_scaled_data <- columnwise_min_max_scale(imputed_data)

pca_fit <- stats::prcomp(imputed_data[,1:(ncol(numeric_data) - 66)], center = FALSE, scale. = FALSE)
print(summary(pca_fit))
pca_training_data <- predict(pca_fit, imputed_data[,1:(ncol(numeric_data) - 66)])[,1:64]
X11()
boxplot(vegan::scores(pca_training_data)[,2]~targets)
scores(pca_training_data)
# create the validation data 
print(colnames(validation_indices))
print(colnames(min_max_scaled_data))
print(summary(columnwise_min_max_scale(validation_indices)))
hist(validation_indices$Carter)
validation_outliers <- detect_outliers(validation_indices)
min_max_scaled_validation <- columnwise_min_max_scale(validation_indices[!validation_outliers,])
dim(min_max_scaled_validation)
hist(min_max_scaled_validation %>% as.matrix())
X11()
hist(min_max_scaled_data %>% as.matrix())
pca_validation_data <- predict(pca_fit, validation_indices[!validation_outliers,])[,1:64] %>% as.data.frame()
x11()
boxplot(vegan::scores(pca_validation_data)[,2]~validation_labels[!validation_outliers])
scores(pca_training_data)

# standardization
indice_standardizer <- caret::preProcess(imputed_data[,1:95])
standardized_indices <- predict(indice_standardizer, imputed_data[,1:95])
print(summary(standardized_indices))

val_standardizer <- caret::preProcess(validation_indices)
standardized_validation <- predict(val_standardizer, validation_indices)
print(summary(standardized_validation))

min_max

X11()
hist(standardized_indices$OSAVI2, breaks = 20)
X11()
hist(standardized_validation$OSAVI2, breaks = 20)
########################################################
##      Visualization, t-SNE embedding
########################################################
library(Rtsne)
unique_indices <- imputed_data[!duplicated(imputed_data),1:95]
normalized_veg_indices <- Rtsne::normalize_input(
    unique_indices %>% 
    as.matrix()
    )
embedding_2D <- Rtsne::Rtsne(normalized_veg_indices)
print(names(embedding_2D))


X11()
plot(embedding_2D$Y, col = as.factor(targets))
par(xpd=T)
legend("topright", legend = unique(targets), col = seq_along(unique(targets)),pch = 1)

#########################################################
##     Spectrometer Correction
########################################################

img_df <- resample_df(image_validation, normalize = FALSE)
shared_cols <- intersect(colnames(img_df), colnames(imputed_data))
img_matrix <- img_df[, shared_cols] %>% as.matrix()
grd_matrix <- imputed_data[,shared_cols] %>% as.matrix()
print(dim(img_matrix))
print(dim(grd_matrix))

calculate_transform <- function(image_data, ground_data){

}

x_grammian_inverse <- solve(t(img_matrix) %*% img_matrix)
dim(x_grammian_inverse)
y_x_trans <- t(grd_matrix) %*% (img_matrix)
dim(y_x_trans)

#########################################################
##     Vector Quantization
########################################################

# method 1 - 
grd_indices <- imputed_data[,1:95]
library(class)
codeBook <-  lvqinit(
    min_max_scaled_data[,1:95], 
    targets, 
10)
code_book_train <- class::olvq1(min_max_scaled_data[,1:95], targets, codeBook)
prediction <- class::lvqtest(code_book_train, min_max_scaled_data[,1:95])
lvq_conf <- caret::confusionMatrix(prediction, targets, mode = "everything")

print(lvq_conf)
print(nrow(min_max_scaled_validation))
image_prediction <- class::lvqtest(code_book_train, min_max_scaled_validation)
print(dim(min_max_scaled_validation))

lvq_validation_conf <- caret::confusionMatrix(image_prediction, validation_labels, mode = "everything")
print(length(validation_labels))
print(lvq_validation_conf)
# method 2 : manual feature extraction with dplyr ntile() function

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

rf_model <- ranger::ranger(
    num.trees = 256,
    case.weights = image_weights,
    classification = TRUE,
    x = validation_indices,
    y = validation_labels
)

print(rf_model)

print(rf_model$prediction.error)

predictions <- predict(rf_model, validation_indices)$predictions %>% 
    as.factor()
confusion_matrix <- caret::confusionMatrix(
    predictions, 
    validation_labels, 
    mode = "everything")
    print(confusion_matrix)
print(length(validation_labels))
print(length(predictions))

raster::beginCluster()
cl <- raster::getCluster()

agg_validation <- validate_model(
    rf_model, 
    "./mle/experiments/new/t1/",
    normalize_input = FALSE,
    scale_input = FALSE,
    cluster = cl
)

aggregated_results <- aggregate_results("./mle/experiments/") %>% as.data.frame()
plot_by_pft(aggregated_results, save_path = "aggregates.html")
write_validation_table(aggregated_results, save_path = "table.html")
save(rf_model, "mle/models/imageVI.rda")

print(confusion_matrix)
print(xtable::xtable(confusion_matrix %>% as.table()), type="html")


print(levels(predictions))
print(levels(validation_labels))
print(predictions %>% table())
print(predictions)







apply_ks_test()


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


