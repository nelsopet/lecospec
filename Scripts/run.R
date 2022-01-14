source("./Functions/lecospectR.R")

test_path <- "F:/Lecospec/Ground_Validation/BisonGulchQuads.envi"
test_path_2 <- "F:/Lecospec/tiles/tile_0bYSfUorxlbPTIkA.grd"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp2_1000trees.rda"
big_test <- "F:/DataCubes/raw_1511_rd_rf_or"

eight_mile_quads <- "F:/Lecospec/Ground_Validation/EightMileQuads.envi"
twelve_mile_quads <- "F:/Lecospec/Ground_Validation/TwelveMileGulchQuads1.envi"


results_8m <- estimate_land_cover(
    eight_mile_quads, 
    output_filepath="./8mtest.grd", 
    use_external_bands=TRUE)


results_12m <- estimate_land_cover(
    twelve_mile_quads,
    output_filepath = "./12mtest.grd",
    use_external_bands = TRUE
)

big_results <- estimate_land_cover(
    big_test,
    output_filepath = "./Outputs/raw_1511_PREDICTIONS.grd")

quad_results <- estimate_land_cover(
    test_path,
    output_filepath = "./Output/bison_gulch_outputs_par.grd",
    use_external_bands = TRUE)

ml_model <- load_model(model_path)

output_key <- rjson::fromJSON(file = "./fg2key.json")
key_df <- read.csv("./levels.csv")

cl <- raster::beginCluster()#this is actually quite slow, believe it or not

tile_results <- process_tile(
    eight_mile_quads, 
    ml_model, 
    cluster = cl, 
    return_raster = TRUE, 
    save_path = "./test_raster_save.grd", 
    suppress_output = TRUE,
    use_external_bands = TRUE)
print(tile_results)

raster::endCluster()

tiles <- list.files("./", pattern = "prediction_*")
result <- merge_tiles_gdal(tiles, "./gdal_merged_predictions.grd")
