source("./Functions/lecospectR.R")
require(sf)

####################################################
#       Define file locations
####################################################

# Rasters
test_path <- "E:/Quads/BisonGulchQuads.envi"
test_path_2 <- "E:/Lecospec/Quadrat_Shapefiles/ChatanikaQuads.envi"
test_path_3 <- "./Data/Ground_Validation/TwelveMileGulchQuads1.envi"
test_path_4 <- "./Data/Ground_Validation/TwelveMileGulchQuads2.envi"
test_path_5 <- "E:/Quads/EightMileQuads.envi"
test_path_6 <- "E:/Quads/MurphDomeQuads0_10.envi"
test_path_7 <- "E:/Quads/MurphDomeQuads20_50.envi"
test_path_8 <- "E:/Quads/MurphDomeQuads60_100.envi"

# ML models
model_path_base <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/mle/RandomForest_FncGrp1_1000trees_augmented.rda"

# Shapefiles
EightMileShapes <- "E:/Vectors/EightMile_Quadrats_revised.shp"
twelve_mile_path_1 <- "Data/Vectors/TwelveMileQ0_10_20_30_40m.shp"
twelve_mile_path_2 <- "Data/Vectors/TwelveMileQ70_80_90_100m.shp"
bison_gulch_path <- "Data/Vectors/Bisoon_Quadrats.shp"
chat_path <- "Data/Vectors/ChatanikaQuads.shp"
md_path_1 <- "E:/Vectors/MurphyQuads0_10m.shp"
md_path_2 <- "E:/Vectors/MurphyQuads20_50m.shp"
md_path_3 <- "E:/Vectors/MurphyQuads60_100m.shp"

# Validation data
validation_data_path <- "Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2019_Raw.csv"
validation_data_path_2 <- "Data/Ground_Validation/QuadratEstimates/2018Raw.csv"

####################################################
#       Data loads
####################################################

# Shapefile & check names
tm_shapes <- sf::st_read(md_path_3)
print(tm_shapes$CLASS_NAME)

# process_tile inputs
ml_model <- load_model(model_path)
bandnames <- read.csv("./bands.csv")$x %>% as.vector()

# load the validation data
validation_df <- read.csv(validation_data_path_2, na.strings=c("NA", "n/a"))

####################################################
#       Process the Quadrats 
####################################################
#since the images are small-ish, process_tile is faster
tile_results <- process_tile(
    test_path_8,
    ml_model,
    1,
    cluster = NULL,
    return_raster = TRUE,
    band_names = bandnames,
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
tm_shapes$CLASS_NAME <- md_names_3



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

validation_aggregates <- validate_results(
    tile_results,
    projected_shapes,
    validation_df,
    rjson::fromJSON(file="./pft_adj_list.json"),
    "./pft1_template.csv",
    aggregation = 1
)

# sanity check
print(validation_aggregates[[2]])

# Statistical tests & write to file
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
