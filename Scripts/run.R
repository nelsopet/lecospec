source("./Functions/lecospectR.R")

test_path <- "./Data/Ground_Validation/Imagery/BisonGulchQuads.envi"

raster::raster(test_path) %>% plot()

test_path_2 <- "F:/Lecospec/tiles/tile_W7u8XiTLq7S1hdvM.grd"
model_path_r <- "./mle/models/gs/3bdfeb25-60d1-41eb-941a-6a620ab0064c.rda"
model_path_a <- "./mle/models/gs/df4f745b-a12e-4b5c-b2dd-334cbc8f8f9e.rda"
model_path <- "./mle/models/gs/3bdfeb25-60d1-41eb-941a-6a620ab0064c.rda"


big_test <- "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_10350_rd_rf_or"
medium_test <- "./Data/SubsetDatacube"

# 
print(date())
quad_results <- estimate_land_cover(
    test_path, 
    output_filepath = "./Output/bg_fncgrp1_outputs_m.grd",
    use_external_bands = TRUE)
closeAllConnections()
print(date())


loaded_quad_results <- raster::raster("./Output/bg_outputs_m.grd")

print(loaded_quad_results)

print(quad_results)

windows();hist(quad_results)
windows();hist(loaded_quad_results)

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
    output_filepath = "./Output/md_10350_fncgrp1_PREDICTIONS.grd")
print(date())

output <- raster::raster("./Output/bison_gulch_outputs_par.grd")
png("./run_out.png", width = 1024, height = 1024)
plot(output)
dev.off()

ml_model <- load_model(model_path_a)
print(ml_model$forest$independent.variable.names)

# load bands
band_names <- read.csv("./bands.csv")$x %>% as.vector()
print(band_names)


test_raster <- raster::brick(test_path_2)
names(test_raster)

cl <- raster::beginCluster()#this is actually quite slow, believe it or not

print(date())
profvis::profvis(
    tile_results <- process_tile(
        test_path_2, 
        ml_model, 
        1,
        cluster = NULL, 
        return_raster = TRUE, 
        band_names = band_names,
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
