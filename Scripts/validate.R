source("./Functions/lecospectR.R")
require(sf)

####################################################
#       Define file locations
####################################################

# Rasters
test_path_1 <- "Data/Quadrats/BisonGulchQuads.envi"
test_path_2 <- "Data/Quadrats/ChatanikaQuads.envi"
test_path_3 <- "Data/Quadrats/TwelveMileGulchQuads1.envi"
test_path_4 <- "Data/Quadrats/TwelveMileGulchQuads2.envi"
test_path_5 <- "Data/Quadrats/EightMileQuads.envi"
test_path_6 <- "Data/Quadrats/MurphDomeQuads0_10.envi"
test_path_7 <- "Data/Quadrats/MurphDomeQuads20_50.envi"
test_path_8 <- "Data/Quadrats/MurphDomeQuads60_100.envi"

# ML models
model_path_base <- "Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"
model_path <- "mle/RandomForest_FncGrp1_1000trees_augmented.rda"
model_path_128 <- "Output/E_003_Pred_Model_RandomForest_FncGrp1_128trees.rda"
model_path_64 <- "Output/E_003_Pred_Model_RandomForest_FncGrp1_64trees.rda"
model_path_rf <- "mle/fg1_model.rda"

# Shapefiles
shape_path_1 <- "Data/Vectors/Bisoon_Quadrats_renamed_quads.shp"
shape_path_2 <- "Data/Vectors/ChatanikaQuads.shp"
shape_path_3 <- "Data/Vectors/TwelveMileQ0_10_20_30_40m.shp"
shape_path_4 <- "Data/Vectors/TwelveMileQ70_80_90_100m.shp"
shape_path_5 <- "Data/Vectors/EightMile_Quadrats_revised.shp"
shape_path_6 <- "Data/Vectors/MurphyQuads0_10m.shp"
shape_path_7 <- "Data/Vectors/MurphyQuads20_50m.shp"
shape_path_8 <- "Data/Vectors/MurphyQuads60_100m.shp"



# Validation data
validation_data_path <- "Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2019_Raw.csv"
validation_data_path_2 <- "Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2018_Raw.csv"

####################################################
#       Data loads
####################################################

# Shapefile & check names
tm_shapes <- sf::st_read(shape_path_1)
print(tm_shapes$CLASS_NAME)

# process_tile inputs
ml_model <- load_model(model_path_rf)
band_names <- read.csv("./assets/bands.csv")$x %>% as.vector()

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

md_names_1 <- c(
    "Murphydome10",
    "Murphydome0"
)

md_names_2 <- c(
    "Murphydome50",
    "Murphydome40",
    "Murphydome30",
    "Murphydome20"
)

md_names_3 <- c(
    "Murphydome100",
    "Murphydome90",
    "Murphydome80",
    "Murphydome70",
    "Murphydome60"
)

twelve_mile_names_1 <- c(
    "Twelvemile40",
    "Twelvemile30",
    "Twelvemile20",
    "Twelvemile10",
    "Twelvemile0"
)
twelve_mile_names_2 <- c(
    "Twelvemile100",
    "Twelvemile90",
    "Twelvemile80",
    "Twelvemile70"
)

# there is no Eight Mile Validation Data!
eight_mile_names <- c(
    "Eightmile10",
    "Eightmile0",
    "Eightmile60",
    "Eightmile50",
    "Eightmile40",
    "Eightmile30",
    "Eightmile20",
    "Eightmile100",
    "Eightmile90",
    "Eightmile80",
    "Eightmile70"
)

bison_gulch_names <- c(
    "Bisongulch0",
    "Bisongulch70",
    "Bisongulch80",
    "Bisongulch10",
    "Bisongulch90",
    "Bisongulch20",
    "Bisongulch50",
    "Bisongulch30",
    "Bisongulch40"
)

chatanika_names <- c(
    "Chatanika100",
    "Chatanika90",
    "Chatanika70",
    "Chatanika80",
    "Chatanika60",
    "Chatanika50",
    "Chatanika40",
    "Chatanika30",
    "Chatanika20",
    "Chatanika10",
    "Chatanika0"
)

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

validation_aggregates <- validate_results(
    tile_results,
    projected_shapes,
    validation_df,
    rjson::fromJSON(file = "./assets/pft_adj_list.json"),
    "./assets/pft1_template.csv",
    aggregation = 1
)

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

quadrats <- list(
    test_path_1,
    test_path_2,
    test_path_3,
    test_path_4,
    test_path_5,
    test_path_6,
    test_path_7,
    test_path_8
)

shapes <- list(
    shape_path_1,
    shape_path_2,
    shape_path_3,
    shape_path_4,
    shape_path_5,
    shape_path_6,
    shape_path_7,
    shape_path_8
)


shape_names <- list(
    bison_gulch_names,
    chatanika_names,
    twelve_mile_names_1,
    twelve_mile_names_2,
    eight_mile_names,
    md_names_1,
    md_names_2,
    md_names_3
)

# note: murphy dome appears in both; I recently corrected this one though :)
validation_paths <- list(
    validation_data_path,
    validation_data_path,
    validation_data_path,
    validation_data_path,
    validation_data_path_2,
    validation_data_path_2,
    validation_data_path_2,
    validation_data_path_2
)

save_paths <- list(
    "figures/BisonGulch/",
    "figures/Chatanika/",
    "figures/twelveMile1/",
    "figures/twelveMile2/",
    "figures/EightMile/",
    "figures/MurphyDome/Part1/",
    "figures/MurphyDome/Part2/",
    "figures/MurphyDome/Part3/"
)

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
        save_path = "./test_raster_save_64.grd",
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
            paste0(save_paths[[i]], j, "_redux.png"),
            device = png)

        write.csv(
            validation_aggregates[[j]], 
            paste0(
                save_paths[[i]],
                "validation_redux_",
                j,
                ".csv"
        ))
    }
} 


head(validation_df)

print(raster::extent(tile_results))
