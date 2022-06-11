source("./Functions/lecospectR.R")
require(sf)

# Get some results to work with

test_path <- "./Data/Ground_Validation/BisonGulchQuads.envi"
test_path_2 <- "E:/Lecospec/Quadrat_Shapefiles/ChatanikaQuads.envi"
test_path_3 <- "./Data/Ground_Validation/TwelveMileGulchQuads1.envi"
test_path_4 <- "./Data/Ground_Validation/TwelveMileGulchQuads2.envi"
test_path_5 <- "Data/Ground_Validation/EightMileQuads.envi"
test_path_6 <- "E:/test"

model_path_base <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/mle/RandomForest_FncGrp1_1000trees_augmented.rda"
# separate the quadrats
EightMileShapes <- "Data/Vectors/EightMile_Quadrats_revised.shp"
twelve_mile_path_1 <- "Data/Vectors/TwelveMileQ0_10_20_30_40m.shp"
twelve_mile_path_2 <- "Data/Vectors/TwelveMileQ70_80_90_100m.shp"
bison_gulch_path <- "Data/Vectors/Bisoon_Quadrats.shp"
chat_path <- "Data/Vectors/ChatanikaQuads.shp"
#"Data/Vectors/TwelveMileQ70_80_90_100m.shp"
tm_shapes <- sf::st_read(twelve_mile_path_1)

ml_model <- load_model(model_path)
bandnames <- read.csv("./bands.csv")$x %>% as.vector()

tile_results <- process_tile(
    test_path_3,
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

print(tm_shapes$CLASS_NAME)


windows();plot(tm_shapes[1])
plot(tile_results, add=TRUE)

windows();plot(tile_results)
plot(tm_shapes[1], add=TRUE)
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

murphy_names <- c(
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
    "Murphydome",
)

tm_shapes$CLASS_NAME <- twelve_mile_names_1# select correct name

# load the validation data
validation_data_path <- "Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2019_Raw.csv"
validation_data_path_2 <- "Data/Ground_Validation/QuadratEstimates/2018Raw.csv"
validation_df <- read.csv(validation_data_path, na.strings=c("NA", "n/a"))

# check extents
print("Projection of Raster")
print(raster::crs(tile_results))
print("Projection of Shapefile")
print(crs(tm_shapes))

projected_shapes <- sf::st_transform(tm_shapes, raster::crs(tile_results))

print(crs(projected_shapes))

# check extents
print("Extent of Raster")
print(raster::extent(tile_results))
print("Extent of Shapefile")
print(extent(projected_shapes))

# fix the shapefile names
#validation_df <- read.csv(validation_data_path_2, na.strings=c("NA", "n/a"))

source("./Functions/lecospectR.R")
validation_aggregates <- validate_results(
    tile_results,
    projected_shapes,
    validation_df,
    rjson::fromJSON(file="./pft_adj_list.json"),
    "./pft1_template.csv",
    aggregation = 1
)

print(validation_aggregates[[2]])

chi_squared_results <- apply_chi_squared_test(
    validation_aggregates = validation_aggregates[-1]
    )
KS_results <- apply_KS_test(
    validation_aggregates = validation_aggregates
    )

sink(file = "./figures/BisonGulch/FG1/Augmented/testResults.txt", append = TRUE)
print(chi_squared_results)
print(KS_results)
sink(NULL)




#num_quadrats <- length(tm_shapes$CLASS_NAME)

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
