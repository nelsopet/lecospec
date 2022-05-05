source("./Functions/lecospectR.R")
require(sf)

# Get some results to work with

test_path <- "./Data/Ground_Validation/BisonGulchQuads.envi"
model_path_base <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/RandomForest_FncGrp1_1000trees_augmented.rda"

ml_model <- load_model(model_path)
bandnames <- read.csv("./bands.csv")$x %>% as.vector()

tile_results <- process_tile(
    test_path, 
    ml_model, 
    1,
    cluster = NULL, 
    return_raster = TRUE, 
    names = bandnames,
    save_path = "./test_raster_save.grd", 
    suppress_output = FALSE)


# plot the shapefiles over the quadrats
windows();plot(tile_results)
print(tile_results)

print(tm_shapes[1])
plot(tm_shapes[2,1], add=TRUE)

print(raster::crs(tile_results))

# separate the quadrats
EightMileShapes <- "Data/Vectors/EightMile_Quadrats_ALL.shp"
shapefile_path <- "Data/Vectors/EightMile_Quadrats_ALL.shp"
bison_gulch_path <- "Data/Vectors/Bisoon_Quadrats.shp"
#"Data/Vectors/TwelveMileQ70_80_90_100m.shp"
path <- ""
tm_shapes <- sf::st_read(bison_gulch_path)
print(tm_shapes$CLASS_NAME)
plot(tm_shapes)
fg1_RAT <- read.csv("fg1Rat.csv")
print(levels(tile_results))
print(fg1_RAT)
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

tm_shapes$CLASS_NAME <- bison_gulch_names

tm_shapes
# load the validation data
validation_data_path <- "Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2019_Raw.csv"
validation_df <- read.csv(validation_data_path, na.strings=c("NA", "n/a"))
summary(validation_df)
head(validation_df)
print(validation_df$meters)
validation_df$meters <- validation_df$meters %>% as.numeric()


# build the species adjacency list
pft_path <- "Data/species_table_new.csv"
pft_table <- read.csv(pft_path)

pft_conv <- build_adjacency_list(pft_path)
print(str(pft_conv))

#save the converter to JSON for reuse
jsonData <- rjson::toJSON(pft_conv)
write(jsonData, "./pft_adj_list.json")

# filter to 
head(bison_gulch_quad_validation)

# plot the shapefiles over the quadrats


# build a validation template
print(pft1_template)
pft2_template <- build_validation_template(pft_table, col=4)
genus_template <- build_validation_template(pft_table, col=3)
species_template <- build_validation_template(pft_table, col=2)
print(species_template)

write.csv(pft1_template, "./pft1_template.csv")
write.csv(pft2_template, "./pft2_template.csv")
write.csv(genus_template, "./genus_template.csv")
write.csv(species_template, "./species_template.csv")
print(pft1_template)

plot(tile_results)

# fix the shapefile names
tm_shapes$CLASS_NAME <- c(
    "Twelvemile40",
    "Twelvemile30",
    "Twelvemile20",
    "Twelvemile10",
    "Twelvemile0"
)

pft1_template <- build_validation_template(pft_table)
pft_conv <- build_adjacency_list(pft_path)
print(pft_conv)

source("./Functions/lecospectR.R")
validation_aggregates <- validate_results(
    tile_results,
    tm_shapes,
    validation_df,
    rjson::fromJSON(file="./pft_adj_list.json"),
    "./pft1_template.csv",
    aggregation = 1
)

chi_squared_results <- apply_chi_squared_test(
    validation_aggregates = validation_aggregates
    )
KS_results <- apply_KS_test(
    validation_aggregates = validation_aggregates
    )

sink(file = "./figures/BisonGulch/FG1/Augmented/testResults.txt", append=TRUE)
print(chi_squared_results)
print(KS_results)
sink(NULL)

print(validation_df$Plant)
validation_df <- read.csv(validation_data_path, na.strings=c("NA", "n/a"))
change_aggregation(validation_df$Plant, 2, pft_conv)

#num_quadrats <- length(tm_shapes$CLASS_NAME)

for(i in seq_along(tm_shapes$CLASS_NAME)){

}

windows()
rasterVis::levelplot(raster::crop(tile_results, tm_shapes[1,1]))

plot_prop_test <- plot_quadrat_proportions(
    validation_aggregates[[1]], filter_missing = TRUE)

windows();plot_prop_test

ggsave("fg1_bar_1.png", device = png, path = "figures/BisonGulch/FG1/Augmented/")

plot(plot_prop_test)
print(summary(validation_df))

print(validation_result)

windows();rasterVis::levelplot(tile_results)
