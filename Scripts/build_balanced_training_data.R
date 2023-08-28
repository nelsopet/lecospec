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
img_resampled_bands_5nm <- resample_df(img_bands, delta = 5, drop_existing=TRUE)
img_resampled_bands_10nm <- resample_df(img_bands, delta = 10, drop_existing=TRUE)
img_resampled_bands_25nm <- resample_df(img_bands, delta = 25, drop_existing=TRUE)
img_resampled_bands_50nm <- resample_df(img_bands, delta = 50, drop_existing=TRUE)

data <- cbind(metadata, img_resampled_bands_25nm)
data$FncGrp1 <- as.factor(data$FncGrp1)
data$site <- as.factor(data$Site)


summary(data)

if(!dir.exists("Data/v3")){
    dir.create("Data/v3")
}

create_patch_balanced_sample <- function(
    data, 
    patch_col = "UID", 
    class_col = "FncGrp1", 
    test_count = 15L, 
    train_count = 300L,# rename this to reflect function
    seed = NULL,
    verbose = FALSE){

        if(!is.null(seed)){
            set.seed(seed)
        }
        train_samples <- NULL
        test_samples <- NULL

        split_data <- list()
        patches_by_pft <- list()
        samples_per_patch <- list()
        samples_per_pft <- list()

        split_levels <- unique(as.character(data[,patch_col]))
        class_levels <- unique(as.character(data[,class_col]))

        count_per_patch <- as.list(table(as.character(data[,patch_col])))
        
        
        for(li in class_levels){
            patches_by_pft[[li]] <- list()
            samples_per_pft[[li]] <- 0
            
        }

        for(p in split_levels){
            samples_per_patch[[p]] <- 0
        }

        if(verbose){
            print("Building data for the following classes:")
            print(class_levels)
            print("Splitting evenly across the following patches")
            print(split_levels)
        }

        for( i in seq_along(split_levels)){
            # extract data for the given level
            patch_id <- split_levels[[i]]
            filtered_data <- data[data[,patch_col] == patch_id, ]
            # get the PFT for the patch
            patch_pft <- filtered_data[1,class_col]
            # store the data in the data structures for the processing
            split_data[[patch_id]] <- filtered_data
            patches_by_pft[[patch_pft]] <- append(
                patches_by_pft[[patch_pft]],
                patch_id
                )
        }

        # calculate the number of samples from each patch
        for(j in seq_along(class_levels)){
            # some
            pft <- class_levels[[j]]
            for(k in 1:(test_count)){
                for(m in seq_along(patches_by_pft[[pft]])){
                    if(samples_per_pft[[pft]] < test_count){
                        current_patch_id <- patches_by_pft[[pft]][[m]]
                        current_patch_size <- count_per_patch[[current_patch_id]]
                        if(samples_per_patch[[current_patch_id]] < current_patch_size){
                            samples_per_patch[[current_patch_id]] <- samples_per_patch[[current_patch_id]] + 1
                            samples_per_pft[[pft]] <- samples_per_pft[[pft]] + 1
                        }
                    }
                }
            }

        }

        if(verbose){
            print("Samples per patch")
            print(samples_per_patch)
            print("Samples per PFT")
            print(samples_per_pft)
        }
        

        # get the samples from the patches
        for(st in split_levels){
            
            df <- split_data[[st]]
            n_samp <- samples_per_patch[[st]]
            permutation <- permute::shuffle(nrow(df))
            if(n_samp > 0){

                test_indices <- permutation[1:n_samp]

                if(is.null(test_samples)){
                    test_samples <- df[test_indices, ]
                } else {
                    test_samples <- rbind(test_samples, df[test_indices,])
                    if(verbose){
                        print(table(test_samples$FncGrp1))
                    }

                }
            }

            num_train <- min(train_count, nrow(df) - test_count)
            train_indices <- permutation[(n_samp+1):(num_train + test_count)] %>% as.numeric()
            if(is.null(train_samples)){
                train_samples <- df[train_indices,]
            } else {
                train_samples <- rbind(train_samples, df[train_indices,])
            }
        }

        # verify that the number of samples of each class matches,
        # otherwise sample more at random from

        for(pft in class_levels){

        }

        if(sum(unlist(samples_per_patch)) != nrow(test_samples)){
            warning("the number of rows in the dataframe are not as expected")
        }

    return(list(
        selection = test_samples,
        remainder = train_samples,
        samples_per_patch = samples_per_patch,
        samples_per_class = samples_per_pft
        )
    )
}

train_test_data <- create_patch_balanced_sample(
    data,
    test_count = 15,
    train_count = NULL,
    verbose = TRUE)

split_2_train <- create_patch_balanced_sample(
    train_test_data$remainder,
    test_count = 600,
    train_count = 1000,
    verbose = TRUE
)


write.csv(train_test_data$selection, "Data/v2/test_25nm_600_bands.csv")
write.csv(split_2_train$selection, "Data/v2/train_25nm_600_bands.csv")
