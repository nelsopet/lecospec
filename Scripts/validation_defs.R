# Rasters
test_path_1 <- "Data/Quads/BisonGulchQuads.envi"
test_path_2 <- "Data/Quads/ChatanikaQuads.envi"
test_path_3 <- "Data/Quads/TwelveMileGulchQuads1.envi"
test_path_4 <- "Data/Quads/TwelveMileGulchQuads2.envi"
test_path_5 <- "Data/Quads/EightMileQuads.envi"
test_path_6 <- "Data/Quadrats/MurphDomeQuads0_10.envi"
test_path_7 <- "Data/Quadrats/MurphDomeQuads20_50.envi"
test_path_8 <- "Data/Quadrats/MurphDomeQuads60_100.envi"

# ML models
model_path_base <- "Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"
model_path <- "mle/RandomForest_FncGrp1_1000trees_augmented.rda"
model_path_128 <- "Output/E_003_Pred_Model_RandomForest_FncGrp1_128trees.rda"
model_path_64 <- "Output/E_003_Pred_Model_RandomForest_FncGrp1_64trees.rda"
model_path_rf <- "mle/fg1_model.rda"
dummy_model_path <- "mle/dummy.rda"
sample_weighted <- "mle/sample_weighted_model.rda"
model_normed_training_weighted <- "mle/fg1_model_normed_weighted.rda"

# Shapefiles
shape_path_1 <- "Data/Vectors/Bisoon_Quadrats_georeferenced.shp"
shape_path_2 <- "Data/Vectors/ChatanikaQuads_georeferenced.shp"
shape_path_3 <- "Data/Vectors/TwelveMileQ0_10_20_30_40m.shp"
shape_path_4 <- "Data/Vectors/TwelveMileQ70_80_90_100m.shp"
shape_path_5 <- "Data/Vectors/EightMile_Quadrats_revised.shp"
shape_path_6 <- "Data/Vectors/MurphyQuads0_10m.shp"
shape_path_7 <- "Data/Vectors/MurphyQuads20_50m.shp"
shape_path_8 <- "Data/Vectors/MurphyQuads60_100m.shp"



# Validation data
validation_data_path <- "Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2019_Raw.csv"
validation_data_path_2 <- "Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2018_Raw.csv"


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


validation_df <- rbind(
    read.csv(validation_data_path, na.strings=c("NA", "n/a")),
    read.csv(validation_data_path_2, na.strings=c("NA", "n/a"))
)

band_names <- read.csv("./assets/bands.csv")$x %>% as.vector()