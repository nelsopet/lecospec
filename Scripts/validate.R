source("./Functions/lecospectR.R")
require(sf)

# Get some results to work with

test_path <- "./Data/Ground_Validation/TwelveMileGulchQuads2.envi"
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

plot(tile_results)
print(tile_results)

print(raster::crs(tile_results))

# separate the quadrats
EightMileShapes <- "Data/Vectors/EightMile_Quadrats_ALL.shp"
shapefile_path <- "Data/Vectors/TwelveMileQ70_80_90_100m.shp"
path <- ""
tm_shapes <- sf::st_read(shapefile_path)
print(tm_shapes$CLASS_NAME)

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
tm_shapes$CLASS_NAME <- twelve_mile_names_2

bison_gulch_names <- c(
    "Eightmile0",
)

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

sink(file = "./figures/twelveMile2/FG1/testResults.txt", append=TRUE)
print(chi_squared_results)
print(KS_results)
sink(NULL)

print(validation_df$Plant)
validation_df <- read.csv(validation_data_path, na.strings=c("NA", "n/a"))
change_aggregation(validation_df$Plant, 2, pft_conv)


plot_prop_test <- plot_quadrat_proportions(
    validation_aggregates[[4]], filter_missing = TRUE)

windows();plot_prop_test

ggsave("fg1_bar_4.png", device = png, path = "figures/twelveMile2/FG1/Augmented/")

plot(plot_prop_test)
print(summary(validation_df))

print(validation_result)

