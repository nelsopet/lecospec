source("./Functions/lecospectR.R")
require(sf)

####################################################
#       Data loads
####################################################

source("./Scripts/validation_defs.R")

# Shapefile & check names
tm_shapes <- sf::st_read(shape_path_1)
print(tm_shapes$CLASS_NAME)

# process_tile inputs
ml_model <- load_model("mle/models/norm_noise_post.rda")
band_names <- read.csv("./assets/bands.csv")$x %>% as.vector()
print(ml_model$forest$independent.variable.names)
# load the validation data
validation_df <- read.csv(validation_data_path, na.strings=c("NA", "n/a"))

####################################################
#       Process the Quadrats 
####################################################
#since the images are small-ish, process_tile is faster
tile_results <- process_tile(
    test_path_1,
    ml_model,
    1,
    cluster = NULL,
    return_raster = TRUE,
    band_names = band_names,
    save_path = "./test_raster_save.grd",
    suppress_output = FALSE)

####################################################
#       Plots as needed
####################################################
# plot the shapefiles over the quadrats
windows();plot(tile_results)
print(tile_results)

windows();plot(tm_shapes[1])
plot(tile_results, add=TRUE)

windows();plot(tile_results)
plot(tm_shapes[1], add=TRUE)

####################################################
#       Correct names of shapefile entries
####################################################
# have to ensure the shapefile entries match the 
# validation data

# assign correct names to shafile
tm_shapes$CLASS_NAME <- bison_gulch_names

####################################################
#       Fix projection issues
####################################################

# check extents
print("Projection of Raster")
print(raster::crs(tile_results))
print("Projection of Shapefile")
print(crs(tm_shapes))

# project shapefile (faster) to match the raster data
projected_shapes <- sf::st_transform(tm_shapes, raster::crs(tile_results))

print(crs(projected_shapes))

# check extents
print("Extent of Raster")
print(raster::extent(tile_results))
print("Extent of Shapefile")
print(extent(projected_shapes))


####################################################
#       Run the validation
####################################################
validation_df <- rbind(
    read.csv(validation_data_path, na.strings=c("NA", "n/a")),
    read.csv(validation_data_path_2, na.strings=c("NA", "n/a"))
)

print(projected_shapes)

validation_aggregates <- validate_results(
    tile_results,
    projected_shapes,
    validation_df,
    rjson::fromJSON(file = "./assets/pft_adj_list.json"),
    "./assets/pft1_template.csv",
    aggregation = 1
)
print(nrow(projected_shapes))
print(length(validation_aggregates))

# sanity check
print(validation_aggregates[[3]])

for(i in seq_along(validation_aggregates)){
    write.csv(
        validation_aggregates[[i]], 
        paste0(
            "figures/BisonGulch/validation_redux_",
            i,
            ".csv"
        ))
}

# Statistical tests & write to file
chi_squared_results <- apply_chi_squared_test(
    validation_aggregates = validation_aggregates
    )
KS_results <- apply_KS_test(
    validation_aggregates = validation_aggregates
    )

save_validation()

sink(file = "./figures/Chatanika/stats.txt", append = TRUE)
print(chi_squared_results)
print(KS_results)
sink(NULL)

# plot bar graphs

for(i in seq_along(tm_shapes$CLASS_NAME)){
    if(i > 0){
        plot_prop_test <- plot_quadrat_proportions(
            validation_aggregates[[i]],
            filter_missing = TRUE)

        #windows();plot_prop_test

        ggsave(
            paste0("fg1_bar_", i, ".png"),
            device = png, 
            path = "./")
    }
}


####################################################
#       Save the results & aggregate
####################################################

save_validation(validation_aggregates, base_filename = "validation")

initial_path <- "./assets/pft1_template.csv"# use if there is to cold start
aggregation_path <- "figures/aggregator.csv"

# run the 
aggregator <- read.csv(aggregation_path)

vdf <- coalesce_results(validation_aggregates, aggregator_df = aggregator)
print(vdf)

write.csv(vdf, aggregation_path)

####################################################
#       Run the validation on ALL THE QUADS
####################################################

# load both validation data sets, so there is no more need to be concerned with which goes where, etc.
validation_df <- rbind(
    read.csv(validation_data_path, na.strings=c("NA", "n/a")),
    read.csv(validation_data_path_2, na.strings=c("NA", "n/a"))
)


for( i in seq_along(quadrats)){
    # process the tile
    tile_results <- process_tile(
        quadrats[[i]],
        ml_model,
        1,
        cluster = NULL,
        return_raster = TRUE,
        band_names = band_names,
        save_path = "./test_raster_save_128.grd",
        suppress_output = FALSE)

    # load shapefile and project to match
    shape <- sf::st_read(shapes[[i]])
    print(shape_names[[i]])
    shape$CLASS_NAME <- shape_names[[i]]
    projected_shapes <- sf::st_transform(shape, raster::crs(tile_results))

    # Validation data
    #validation_df <- read.csv(validation_paths[[i]], na.strings=c("NA", "n/a"))

    # run the validation
    validation_aggregates <- validate_results(
        tile_results,
        projected_shapes,
        validation_df,
        rjson::fromJSON(file = "./assets/pft_adj_list.json"),
        "./assets/pft1_template.csv",
        aggregation = 1
    )

    print(names(tile_results))

    # bar plots
    for(j in seq_along(validation_aggregates)){
        plot_prop_test <- plot_quadrat_proportions(
            validation_aggregates[[j]],
            filter_missing = TRUE)

            #windows();plot_prop_test

        ggsave(
            paste0(save_paths[[i]], j, "_actually_normed.png"),
            device = png)

        write.csv(
            validation_aggregates[[j]], 
            paste0(
                save_paths[[i]],
                "actually_normed_",
                j,
                ".csv"
        ))
    }
} 

validate_model()


head(validation_df)

print(raster::extent(tile_results))

plot(terra::rast("test_raster_save_64.grd"))