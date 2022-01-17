source("./Functions/lecospectR.R")

test_path <- "F:/Lecospec/Ground_Validation/BisonGulchQuads.envi"
test_path_2 <- "F:/Lecospec/tiles/tile_0bYSfUorxlbPTIkA.grd"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp2_1000trees.rda"
big_test <- "F:/DataCubes/raw_1511_rd_rf_or"

eight_mile_quads_bad_bands <- "F:/Lecospec/Ground_Validation/EightMileQuads.envi"
eight_mile_quads <- "F:/Lecospec/Ground_Validation/EightMileQuads.grd"
twelve_mile_quads_raw <- "F:/Lecospec/Ground_Validation/TwelveMileGulchQuads1.envi"
twelve_mile_quads <-"F:/Lecospec/Ground_Validation/TwelveMileGulchQuads1.grd"

correct_band_names(twelve_mile_quads_raw, "./bands.csv", twelve_mile_quads)


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

tile_results <- process_file(
    twelve_mile_quads, 
    ml_model, 
    cluster = cl, 
    return_raster = TRUE, 
    save_path = "./test_raster_save.grd")
print(tile_results)

raster::endCluster()

tiles <- list.files("./", pattern = "prediction_*")
result <- merge_tiles_gdal(tiles, "./gdal_merged_predictions.grd")

correct_band_names <- function(raster_filepath, band_filepath, output_filepath) {
    input_raster <- raster::brick(raster_filepath)
    band_count <- raster::nlayers(input_raster)
    bandnames <- read.csv(band_filepath)$x[1:band_count] %>% as.vector()
    names(input_raster) <- bandnames
    # If no output file path is provided, save in place
    if(is.null(output_filepath)){
        raster::writeRaster(input_raster, raster_filepath, overwrite=TRUE)
    } else {
        # otherwise save to the path provided
        raster::writeRaster(input_raster, output_filepath)
    }

}

correct_band_names(
    eight_mile_quads, 
    "./bands.csv", 
    output_filepath = "F:/Lecospec/Ground_Validation/EightMileQuads.grd")
