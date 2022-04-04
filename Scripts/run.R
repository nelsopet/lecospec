source("./Functions/lecospectR.R")

test_path <- "./Data/Ground_Validation/BisonGulchQuads.envi"

#raster::raster(test_path) %>% plot()

test_path_2 <- "tiles/tile_CpDHDDdNOXTuX0G4.tif"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"
big_test <- "F:/DataCubes/raw_1511_rd_rf_or"
medium_test <- "./Data/SubsetDatacube"

print(date())
quad_results <- estimate_land_cover(
    test_path, 
    output_filepath = "./Output/em_outputs_m.grd",
    use_external_bands = TRUE)
closeAllConnections()
print(date())

print(quad_results)
png("./test_results.png")
plot(quad_results)
dev.off()
png("./test_results_BG.png")
barplot(quad_results)
dev.off()
raster::dataType(quad_results)

medium_results  <- estimate_land_cover(
    medium_test,
    output_filepath = "./Outputs/subset_predictions.grd",
    use_external_bands = TRUE)
print(date())

big_results <- estimate_land_cover(
    big_test,
    output_filepath = "./Outputs/raw_1511_PREDICTIONS.grd")
print(date())

output <- raster::raster("./Output/bison_gulch_outputs_par.grd")
png("./run_out.png", width = 1024, height = 1024)
plot(output)
dev.off()

ml_model <- load_model(model_path)
print(ml_model$forest$independent.variable.names)

# load bands
bandnames <- read.csv("./bands.csv")$x %>% as.vector()

cl <- raster::beginCluster()#this is actually quite slow, believe it or not

print(date())
profvis::profvis(
    tile_results <- process_tile(
        test_path, 
        ml_model, 
        1,
        cluster = NULL, 
        return_raster = TRUE, 
        names = bandnames,
        save_path = "./test_raster_save.grd", 
        suppress_output = FALSE),
    interval = 0.01
)
print(date())

plot(tile_results)
print(tile_results)
raster::dataType(tile_results)

raster::endCluster()

species_table <- read.csv("./Data/species_table_new.csv", sep=",", fileEncoding="utf-8")
print(species_table)

functional_group2_levels <- unique(species_table$Functionalgroup2)
print(functional_group2_levels)


uf_test <- update_filename(test_path)
print(uf_test)


tiles <- c(
    "./tiles//prediction_V2tsWuMaJEzoJ2MF.grd",
    "./tiles//prediction_xhzHX4KVl89FsfOU.grd",
    "./tiles//prediction_kGVDd3pM4lcWqNRs.grd",
    "./tiles//prediction_LWlMl4y7KcDL8Hw4.grd"
)

output <- merge_tiles(tiles, "./merge_test.tif")



test <- raster::brick(test_path)
test_df <- raster::rasterToPoints(test)

has_empty_column(test_df)

summary(test_df)

print(date())
mean(raster::values(raster::brick(test_path)), na.rm = TRUE)
print(date())


## convert the BG Quads to .tif format

tif_test <- "./Data/BisonGulchQuads.tif"

raster::writeRaster(
    raster::brick(test_path),
    tif_test,
    "GTiff"
)

raster::writeFormats()

key_df <- 1:8 %>% as.data.frame() %>% print() %>% rename(., x1=.) %>% convert_pft_codes(., 1, to="string")
