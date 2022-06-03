source("./Functions/lecospectR.R")
require(sf)

# Get some results to work with

chatanika_path <- "E:/Lecospec/Quadrat_Shapefiles/ChatanikaQuads.envi"
eightMile_path <- "Data/Ground_Validation/EightMileQuads.envi"
model_path_base <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/mle/RandomForest_FncGrp1_1000trees_augmented.rda"

ml_model <- load_model(model_path)
bandnames <- read.csv("./bands.csv")$x %>% as.vector()

tile_results <- process_tile(
    eightMile_path,
    ml_model,
    1,
    cluster = NULL,
    return_raster = TRUE,
    band_names = bandnames,
    save_path = "./test_raster_save.grd",
    suppress_output = FALSE)


# plot the shapefiles over the quadrats
windows();plot(tile_results)
print(tile_results)

# separate the quadrats
eight_mile_shapes <- "Data/Vectors/EightMile_Quadrats_revised.shp"
shapefile_path <- "Data/Vectors/TwelveMileQ0_10_20_30_40m.shp"
bison_gulch_path <- "Data/Vectors/Bisoon_Quadrats.shp"

chatanika_shape_path <- "E:/Lecospec/Quadrat_Shapefiles/ChatanikaQuads.shp"

path <- ""
tm_shapes <- sf::st_read(chatanika_shape_path)
fg1_RAT <- read.csv("fg1Rat.csv")
levels(tile_results) <- fg1_RAT

plot(tile_results)

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
    "Eightmile60",
    "Eightmile50",
    "Eightmile40",
    "Eightmile30",
    "Eightmile20",
    "Eightmile10",
    "Eightmile0"
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

chatanika_names <- c(# make the quadrat names match the validation data
    "Chatanika100", "Chatanika90",  "Chatanika70",
    "Chatanika80",  "Chatanika60",  "Chatanika50",
    "Chatanika40",  "Chatanika30", "Chatanika20",
    "Chatanika10", "Chatanika0"
)

tm_shapes$CLASS_NAME <- chatanika_names#twelve_mile_names_1# select correct name

print(tm_shapes$CLASS_NAME)
# load the validation data
validation_data_path <- "Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2019_Raw.csv"
validation_data_path_2 <- "Data/Ground_Validation/QuadratEstimates/quadrat_cover_2018.csv"
validation_df <- read.csv(validation_data_path_2, na.strings=c("NA", "n/a"))


windows();plot(tile_results)
plot(tm_shapes, add=TRUE)
print(tile_results)


# build a validation template

# fix the shapefile names

source("./Functions/lecospectR.R")
validation_aggregates <- validate_results(
    tile_results,
    tm_shapes,
    validation_df,
    rjson::fromJSON(file = "./pft_adj_list.json"),
    "./pft1_template.csv",
    aggregation = 1
)

chi_squared_results <- apply_chi_squared_test(
    validation_aggregates = validation_aggregates
    )
KS_results <- apply_KS_test(
    validation_aggregates = validation_aggregates
    )

sink(file = "./figures/BisonGulch/FG1/Augmented/testResults.txt", append = TRUE)
print(chi_squared_results)
print(KS_results)
sink(NULL)

validation_df <- read.csv(validation_data_path, na.strings=c("NA", "n/a"))
change_aggregation(validation_df$Plant, 2, pft_conv)

#num_quadrats <- length(tm_shapes$CLASS_NAME)

for(i in seq_along(tm_shapes$CLASS_NAME)){
    plot_prop_test <- plot_quadrat_proportions(
        validation_aggregates[[i]], 
        filter_missing = TRUE)

    #windows();plot_prop_test

    ggsave(
        paste0("fg1_bar_", i, ".png"),
        device = png,
        path = "./")
}



## Making big maps

 plot_options <- define_plot_options(
            title =  "Bison Gulch Predictions",
            xLabel = "Longitude",
            yLabel = "Latitude"
        )


predictions <- raster::raster("./Output/bg_1511_fngrp1_PREDICTIONS_FINAL.grd")

levels(predictions) <- fg1_RAT
big_plot <- plot_categorical_raster(
    predictions,
    plot_options = plot_options
)
windows();big_plot
