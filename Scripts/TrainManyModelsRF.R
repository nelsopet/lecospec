source("Functions/lecospectR.R")

# Read in data
spec_chem_canopy <- read.csv("Output/speclib/spruce_canopy_speclib_5nm_wChem.csv")
band_names <- colnames(spec_chem_canopy[, 29:ncol(spec_chem_canopy)])

# Set seed for stable cal/val split
set.seed(1234)

# List columns names available for modeling

vars <- colnames(spec_chem_canopy[, 11:27])
model_id <- 1

lapply(1:length(vars), function(x) {
    # x=7

    className <- vars[1]

    spec_chem_canopy_df <- spec_chem_canopy[is.na(spec_chem_canopy[className]) == F, ] %>%
        group_by(Site, TreeID, GenusSpecies) %>% # tally %>% dplyr::select(n) %>% ungroup() %>% summarise(min_pix= min(n), max_pix = max(n), median_pix = median(n))
        slice_sample(n = 80, replace = F)

    # Create a test and train split
    inTrain <- caret::createDataPartition(
        y = spec_chem_canopy_df[[className]],
        p = 0.7,
        list = FALSE # ,
        # na.rm = TRUE
    )

    training <- spec_chem_canopy_df[inTrain, ] %>%
        ungroup() %>%
        dplyr::select(className, band_names)
    testing <- spec_chem_canopy_df[-inTrain, ] %>%
        ungroup() %>%
        dplyr::select(className, band_names)
    # Variable Importance RF model
    n <- 256
    rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),
        data = training, num.trees = n, importance = "impurity_corrected"
    )
    plot(sort(rf_mod$variable.importance))
    # Prediction RF model
    n <- 1000
    rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),
        data = training, num.trees = n
    )
    rf_mod_pred <- predict(rf_mod, testing)
    jpeg(paste0("Output/models/figs/", vars[x], "_cal_v_val_density.jpg"), width = 700, height = 700)
    plot(hexbin::hexbin(rf_mod_pred$predictions, testing[[className]]),
        main = paste(
            "Cal vs Val values for ", 
            className, 
            ", R2=", 
            round(R2(rf_mod_pred$predictions, testing[[className]], formula = "corr"), 2), 
            sep = ""),
        xlab = paste("Calibration fitted values for ", className, sep = ""), ylab = paste("Validation fitted values for ", className, sep = "")
    )
    dev.off()

    save(rf_mod, file = paste0(
        "./Output/models/model_",
        rlang::hash(rf_mod),
        ".rda"
    ))
})
