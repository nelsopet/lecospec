source("./Functions/LandCoverEstimator_RF.R")

test_path <- "F:/Lecospec/Ground_Validation/BisonGulchQuads.envi"
test_path_2 <- "F:/Lecospec/tiles/tile_0bYSfUorxlbPTIkA.grd"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp2_1000trees.rda"
big_test <- "F:/DataCubes/raw_1511_rd_rf_or"

big_results <- estimate_land_cover(big_test)

quad_results <- estimate_land_cover(test_path_2, use_external_bands = TRUE)

ml_model <- load_model(model_path)

print(ml_model$forest$independent.variable.names)

cl <- raster::beginCluster()#this is actually quite slow, believe it or not.  Makes it faster though

tile_results <- process_tile(test_path_2, ml_model)
plot(tile_results)
summary(tile_results)

plot(raster::brick(test_path_2)[[1]])
raster::endCluster()
traceback()

print(get_required_veg_indices(ml_model))