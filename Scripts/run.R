source("./Functions/LandCoverEstimator_RF.R")

test_path <- "F:/Lecospec/Ground_Validation/BisonGulchQuads.envi"
test_path_2 <- "F:/Lecospec/tiles/tile_0bYSfUorxlbPTIkA.grd"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp2_1000trees.rda"
big_test <- "F:/DataCubes/raw_1511_rd_rf_or"

big_results <- estimate_land_cover(big_test)

quad_results <- estimate_land_cover(test_path, use_external_bands = TRUE)

ml_model <- load_model(model_path)

print(ml_model$forest$independent.variable.names)

output_key <- rjson::fromJSON(file = "./fg2key.json")
key_df <- read.csv("./levels.csv")

cl <- raster::beginCluster()#this is actually quite slow, believe it or not

tile_results <- process_tile(test_path_2, ml_model, key = output_key, cluster = cl, return_raster = TRUE)
plot(tile_results)
summary(tile_results)

plot(raster::brick(test_path_2)[[1]])
raster::endCluster()
traceback()

print(get_required_veg_indices(ml_model))

df_convert_results <- tile_results %>% mutate(z = case_when(
    z == "Abiotic" ~ 1,
    z == "Dwarf Shrub" ~ 2,
    z == "Forb" ~ 3,
    z == "Graminoid" ~ 4,
    z == "Lichen" ~ 5,
    z == "Moss" ~ 6, 
    z == "Shrub" ~ 7,
    z == "Tree" ~ 8
), .keep = "unused")
print(colnames(df_convert_results))
raster_results <- raster::rasterFromXYZ(df_convert_results)
plot(raster_results)
r <- raster::stack(raster_results, raster_results, raster_results)
plot(r)
plotRGB(r)

raster::writeRaster(raster_results, "./test_out.tif")
