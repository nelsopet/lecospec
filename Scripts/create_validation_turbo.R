library(terra)
library(sf)
library(tidyverse)
library(arrow)
library(stringr)


source("Scripts/validation_defs.R")

validation_data <- NULL

for( i in seq_along(quadrats)){

    vector_layer <- terra::vect(shapes[[i]])
    quad <- terra::rast(quadrats[[i]])

    vector_layer$CLASS_NAME <- shape_names[[i]]

    for(j in seq(nrow(vector_layer))){

        quadrat_name <- vector_layer$CLASS_NAME[[j]]
        quadrat_geometry <- vector_layer[,j]
        print(quadrat_geometry)

        band_names <- read.csv("assets/bands.csv")$x

        cropped_raster <- terra::mask(quad, quadrat_geometry)
        df <- as.data.frame(cropped_raster)
        colnames(df) <- band_names
        head(df)

        veg_indices <- get_vegetation_indices(df, NULL, cluster = NULL)

        # drop rows that are uniformly zero
      
        resampled_df <- resample_df(
            df,
            normalize = FALSE,
            max_wavelength = 995.716,
            drop_existing=TRUE)
        gc()

        resampled_df$location <- quadrat_name

        head(resampled_df)

        combined_df <- cbind(resampled_df, veg_indices)

        if(is.null(validation_data)){
            validation_data <- combined_df
        } else {
            validation_data <- rbind(validation_data, combined_df)
        }

    }

}

write.csv(validation_data, "./validation_test.csv")


# ideal validation flow -> 
# for predictions, group by Quadrat, then get the count for each PFT.  
# for validation, get the proportion for each PFT and each quadrat <- this does not change


directory <- "mle/experiments/gs/a9ee12bd-fa8f-492c-bbf5-ce5c3de99df6"
validation_filepaths <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)

compiled_validation <- NULL

validation_by_quadrat <- list()

for(k in seq_along(validation_filepaths)){
    
    loaded_template <- read.csv(validation_filepaths[[k]])
    site_index <- parse_filename_to_site_index(validation_filepaths[[k]])
    quadrat_index <- parse_filename_to_quadrat_index(validation_filepaths[[k]])
    print(site_index)
    print(quadrat_index)
    quadrat_id <- shape_names[[site_index]][[quadrat_index]]

    loaded_template$id <- quadrat_id
    loaded_template$predicted_counts <- 0
    loaded_template$prediction_prop <- 0
    #validation_by_quadrat[[quadrat_id]] <- loaded_template

    prop_by_pft <- list()
    for(r in seq(nrow(loaded_template))){
        key <- loaded_template$key[[r]]
        count <- loaded_template$validation_counts[[r]]
        prop <- loaded_template$validation_prop[[r]]

        prop_by_pft[[key]] <- list(
            prop = prop,
            count = count
        )

    }
    validation_by_quadrat[[quadrat_id]] <- prop_by_pft


}

serialized_validation <- rjson::toJSON(validation_by_quadrat)
print(validation_by_quadrat)
write(serialized_validation, "assets/validation_tree.json")

parse_filename_to_site_index <- function(name) {
    site_index <- 0
    if(grepl("site_1", name, fixed=TRUE)){

        return(1)
    }

    if(grepl("site_2", name, fixed=TRUE)){
        return(2)
    }

    if(grepl("site_3", name, fixed=TRUE)){
        return(3)
    }

    if(grepl("site_4", name, fixed = TRUE)){
        return(4)
    }

    if(grepl("site_5", name, fixed = TRUE)){
        return(5)
    }

    if(grepl("site_6", name, fixed = TRUE)){
        return(6)
    }

    if(grepl("site_7", name, fixed = TRUE)){
        return(7)
    }

    if(grepl("site_8", name, fixed = TRUE)){
        return(8)
    }

    if(grepl("site_9", name, fixed = TRUE)){
        return(9)
    }
    if(grepl("site_10", name, fixed = TRUE)){
        return(10)
    }
}

parse_filename_to_quadrat_index <- function(name, site_name) {
    index <- 0
    if(grepl("quadrat_1", name, fixed=TRUE)){
        index <- 1
    }
    if(grepl("quadrat_2", name, fixed=TRUE)){
        index <- 2
    }
    if(grepl("quadrat_3", name, fixed=TRUE)){
        index <- 3
    }
    if(grepl("quadrat_4", name, fixed=TRUE)){
        index <- 4
    }
    if(grepl("quadrat_5", name, fixed=TRUE)){
        index <- 5
    }
    if(grepl("quadrat_6", name, fixed = TRUE)){
        index <- 6
    }
    if(grepl("quadrat_7", name, fixed = TRUE)){
        index <- 7
    }
    if(grepl("quadrat_8", name, fixed = TRUE)){
        index <- 8
    }
    if(grepl("quadrat_9", name, fixed = TRUE)){
        index <- 9
    }
    if(grepl("quadrat_10", name, fixed = TRUE)){
        index <- 10
    }
    if(grepl("quadrat_11", name, fixed = TRUE)){
        index <- 11
    }
    if(grepl("quadrat_12", name, fixed = TRUE)){
        index <- 12
    }
    if(grepl("quadrat_13", name, fixed = TRUE)){
        index <- 13
    }

    return(index)
}

model <- load_model("mle/models/gs/8ffd4ac3-9428-41d0-84f6-3d03a0078ef3.rda")

turbo_validate <- function(model){

    data <- read.csv("./validation_test.csv")

    validation_tree <- rjson::fromJSON(file = "assets/validation_tree.json")

    predictions <- apply_model(data, model)
    head(predictions)
    data$predictions <- as.factor(predictions$.)
    
    by_quad <- data %>% # check this 
        group_by(location, predictions) %>%   
        summarise(n = n()) %>%
        mutate(freq = n / sum(n))


    validation_col <- list()

    for(i in seq(nrow(by_quad))){
        loc <- by_quad$location[[i]]
        key <- by_quad$predictions[[i]]
        cal_prop <- validation_tree[[loc]][[key]][["prop"]]
        validation_col <- append(validation_col, cal_prop[[1]])
       
    }

    print(nrow(by_quad))

    print(validation_col)

    by_quad$validation <- as.numeric(validation_col)

    return(by_quad)

}

t <- turbo_validate(model)
head(t)
summary(t)
plot_turbo_validation_results <- function(t_results){

    plt <- ggplot2::ggplot() +
    geom_point()
        
}

get_r2 <- function(t_results){
    model_fit <- lm(t_results$freq~t_results$validation, na.action = na.exclude)
    print(summary(model_fit))
    return(summary(model_fit)$r.squared)

}

get_r2(t)
