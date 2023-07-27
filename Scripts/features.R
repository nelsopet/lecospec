source("Functions/lecospectR.R")
img_base_path <- "Data/Ground_Validation/PFT_image_spectra/PFT_Image_SpectralLib_Clean.csv"
full_data <- read.csv(img_base_path)

head(full_data)

img_bands <- subset(
    full_data, 
    select=-c(
        X,
    	UID,
        ScanNum,
    	sample_name,
    	PFT,
    	FncGrp1,
        Site
    ))

metadata <- subset(
    full_data, 
    select = c(FncGrp1, Site, UID)
)

img_indices <- impute_spectra(get_vegetation_indices(img_bands, NULL))
img_resampled_bands <- resample_df(img_bands, delta = 100, drop_existing=TRUE)

band_correlation <- cor(img_resampled_bands)
index_correlation <- cor(img_indices)
X11();heatmap(index_correlation)
X11();heatmap(band_correlation)
print(band_correlation)


X11();qgraph::qgraph(
    band_correlation,
    minimum = 0.9,
    cut = 0.95,
    vsize = 4,
    legend = TRUE,
    borders = FALSE)
X11();qgraph::qgraph(
    index_correlation,
    minimum = 0.9,
    cut = 0.95,
    vsize = 4,
    legend = TRUE,
    borders = FALSE)

index_uncor <- abs(index_correlation) < 0.3
X11();qgraph::qgraph(
    index_uncor,
    minimum = 0.9,
    cut = 0.5,
    vsize = 4,
    legend = TRUE,
    borders = FALSE)

print(colnames(img_resampled_bands))

colnames(img_indices)

selected_bands <- c("X502.593_5nm", "X702.593_5nm", "X802.593_5nm")
NUM_INDICES <- 50

random_selection_indices <- permute::shuffle(ncol(img_indices))[1:NUM_INDICES]
tier_1_indices <- c(
    "D1",
    "Vogelmann3",
    "DPI",
    "mSR",
    "REPLE",
    "REPLi",
    "Datt",
    "Datt3"
)

tier_2_indices <- c(
    "MTCI",
    "PRI",
    "SR7",
    "Datt4",
    "Carter",
    "PRInorm",
    "CARI",
    "SRPI",
    "NPCI"
)
tier_3_indices <- c(
    "SIPI",
    "CRI3",
    "CRI4",
    "CRI2",
    "MTVI",
    "OSAVI2",
    "PARS",
    "OSAVI",
    "PSSR",
    "EGFR",
    "TGI",
    "TCARI",
    "SR8",
    "CRI1"
)
colnames(img_indices)


selected_cols <- c(
    selected_bands,
    tier_1_indices,
    tier_2_indices,
    tier_3_indices
)


#sample_perm <- permute::shuffle(nrow(img_indices))
data_split <- create_patch_balanced_sample(
    cbind(
    img_indices,
    img_resampled_bands,
    metadata
    ),
    train_count = 300
)

subsampled_data <- subsample_by_class(data_split$train, "FncGrp1")
x_train <- subset(subsampled_data, select = -c(FncGrp1, Site, UID))
#[,c(selected_bands, selected_indices)]

y_train <- subsampled_data$FncGrp1 %>% as.factor()
nrow(x_train)



svm_grid <- expand.grid(
    cost = c(11, 12, 13, 14, 15, 16, 17, 18, 19),
    Loss = c("L1", "L2")
)

rf_grid <- expand_grid(

)


training_options <- caret::trainControl(
    method = "repeatedcv",
    number = 100,
    repeats = 3,
    search = "random"
)

getModelInfo("AdaBoost.M1")
model_svm <- caret::train(
    x = round(x_train[, selected_cols], digits = 1),
    y = y_train[,included_columns],
    method = "svmLinear3",
    #preProcess = c("center", "scale", "knnImpute"),
    #weights = targets_to_weights(y_train),
    trControl = training_options,
    tuneGrid = svm_grid
)
model_svm

model_fit <- caret::train(
    x = x_train[, included_columns],
    y = y_train,
    method = "rpart",
    #preProcess = c("center", "scale", "knnImpute"),
    #weights = targets_to_weights(y_train),
    trControl = training_options#,
    #tuneGrid = grid
)
model_fit

model <- ranger::ranger(
    num.trees = 64,
    #max.depth = 8,
    case.weights = targets_to_weights(y_train),
    x = bin_df(x_train),
    y = y_train
)
print(model)
head(x_train)
print(model_fit)
print(model_fit$results)
model_svm

if(!dir.exists("mle/experiments/manual")){
    dir.create("mle/experiments/manual")
}

model_id <- uuid::UUIDgenerate()
model_dir <- paste0("mle/experiments/manual/", model_id, "/")
dir.create(model_dir)
print(model_dir)

validate_model(
    model_fit,
    save_directory = model_dir
)

aggregated_results <- aggregate_results(model_dir)
r2 <- calculate_validation_r2(aggregated_results)
print(r2)

save(model_fit, file = paste0(model_dir, "model.rda"))

print(model_dir)
plt <- plot_by_pft(
    aggregated_results,
    save_path = paste0(model_dir, "aggregate.html"),
    open = FALSE,
    image_path = NULL,
    aggregation=0
)


subsample_by_class <- function(df, by_col){

    train_labels <- df[, by_col]
    sample_perm <- permute::shuffle(nrow(df))
    sample_index <- create_stratified_sample(
        train_labels,
        permutation = sample_perm,
        samples_per_pft = 300
    )

    return(df[sample_perm,][sample_index,])
}


bin_df <- function(df, num_bins = 10){
    binned_df <- as.data.frame(df)

    for(col in colnames(df)){

        binned_df[,col] <- cut(df[,col], breaks = num_bins)
    }

    return(binned_df)

}
