source("./Functions/lecospectR.R")

test_path <- "F:/Lecospec/Ground_Validation/BisonGulchQuads.envi"

raster(test_path) %>% plot()
test_path_2 <- "F:/Lecospec/tiles/tile_0bYSfUorxlbPTIkA.grd"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp2_1000trees.rda"
big_test <- "F:/DataCubes/raw_1511_rd_rf_or"

big_results <- estimate_land_cover(
    big_test,
    output_filepath = "./Outputs/raw_1511_PREDICTIONS.grd")

quad_results <- estimate_land_cover(
    test_path, 
    output_filepath = "./Output/bison_gulch_outputs_par.grd",
    use_external_bands = TRUE)

output <- raster::raster("./Output/bison_gulch_outputs_par.grd")
png("./run_out.png")
plot(output)
dev.off()





ml_model <- load_model(model_path)

output_key <- rjson::fromJSON(file = "./fg2key.json")
key_df <- read.csv("./levels.csv")

cl <- raster::beginCluster()#this is actually quite slow, believe it or not

tile_results <- process_tile(
    test_path_2, 
    ml_model, 
    cluster = cl, 
    return_raster = TRUE, 
    save_path = "./test_raster_save.grd", 
    suppress_output = FALSE)

print(tile_results)

raster::endCluster()

species_table <- read.csv("./Data/species_table_new.csv", sep=",", fileEncoding="utf-8")
print(species_table)


functional_group2_levels <- unique(species_table$Functionalgroup2)
print(functional_group2_levels)
