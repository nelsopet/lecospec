source("./Functions/lecospectR.R")
require(sf)

# Get some results to work with

test_path <- "./Data/Ground_Validation/TwelveMileGulchQuads1.envi"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"

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
shapefile_path <- "Data/Vectors/TwelveMileQ0_10_20_30_40m.shp"
tm_shapes <- sf::st_read(shapefile_path)
tm_shapes$CLASS_NAME <- gsub("0m", "0", tm_shapes$CLASS_NAME)
print(tm_shapes)
plot(tm_shapes,  add = TRUE)
extracted_quads <- raster::extract(tile_results, tm_shapes) %>% as.data.frame()
print(extracted_quads[[1]]$CLASS_ID)
print(tm_shapes)
tm_shapes$CLASS_ID
# get the quardrat distributions
prediction_df <- raster::rasterToPoints(tile_results) %>% as.data.frame()
summary(prediction_df)
test_dist <- get_prediction_distribution(prediction_df$z)
print(test_dist)

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

validation_result <- validate_results(
    tile_results,
    tm_shapes,
    validation_df,
    rjson::fromJSON(file="./pft_adj_list.json"),
    "./pft1_template.csv"
)

print(validation_df$Plant)
validation_df <- read.csv(validation_data_path, na.strings=c("NA", "n/a"))
change_aggregation(validation_df$Plant, 1, pft_conv)


print(summary(validation_df))

print(validation_result)

