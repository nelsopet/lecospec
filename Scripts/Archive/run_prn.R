source("./Functions/lecospectR.R")

test_path <- "./Data/Ground_Validation/Imagery/BisonGulchQuads.envi"

raster::raster(test_path) %>% plot()


test_path_2 <- "tiles/tile_CpDHDDdNOXTuX0G4.tif"
test_path_3<-"./tiles/tile_xnkSMENlt5EnFYz2.envi"
#
model_path <- "./mle/models/gs/31024810-ee27-49e7-8247-e83ac0b738df.rda"
Bison_path ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/raw_1511_rd_rf_55pctWhiteRef_or"
Bonanza_path ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/raw_6425_rd_rf_55pctWhiteRef_or"
EightMile_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/raw_5968_rd_rf_55pctWhiteRef_or"
Chatanika_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/raw_0_rd_rf_56pctWhiteRef_or"
TwelveMile_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_0_rd_rf_55pctWhiteRef_or"

#big_test <- "F:/ORNL_DAAC_DATA_ARCHIVE/BisonGulch/BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or" 
#big_test <-"F:/ORNL_DAAC_DATA_ARCHIVE/Bonanza/raw_6425_rd_rf_or"
#big_test <- "F:/ORNL_DAAC_DATA_ARCHIVE/WickershamDome/WickershamDome_2019_08_08_19_31_51_0_rd_rf_or"
#big_test <- "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_10350_rd_rf_or"
#big_test <- "F:/ORNL_DAAC_DATA_ARCHIVE/Chatnika/Chatnika_2018_07_29_20_32_59_0_rd_rf_or"
#big_test<-"E:/ORNL_DAAC_DATA_ARCHIVE/LittleLake/LittleLake_2018_07_31_01_09_59_132_rd_rf_or"
#big_test<-"F:/ORNL_DAAC_DATA_ARCHIVE/BigTrailLake/BigTrailLake_2019_08_07_20_56_58_0_rd_rf_or"
#big_test<-"E:/ORNL_DAAC_DATA_ARCHIVE/BigTrailLake/BigTrailLake_2019_08_08_00_25_53_2000_rd_rf_or"
#big_test<-"E:/ORNL_DAAC_DATA_ARCHIVE/ClaytonLake/ClaytonLake_2018_07_27_01_22_25_4000_rd_rf_or"
#big_test<-"E:/ORNL_DAAC_DATA_ARCHIVE/EagleSummit/EagleSummit_2019_08_06_00_57_02_2546_rd_rf_or"
#big_test<-"F:/ORNL_DAAC_DATA_ARCHIVE/EightMile/EightMile_2018_07_28_22_56_17_5968_rd_rf_or"
#big_test<-"F:/ORNL_DAAC_DATA_ARCHIVE/TwelveMile/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
#big_test<-"F:/ORNL_DAAC_DATA_ARCHIVE/TwelveMile/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or"
medium_test <- "./Data/SubsetDatacube"

print(date())
quad_results <- estimate_land_cover(
  test_path, 
  output_filepath = "./Output/bg_fncgrp1_outputs_m.grd",
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
closeAllConnections()
big_results <- estimate_land_cover(
  TwelveMile_path,
  output_filepath = "./Output/dev_FullCube/tm_0_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.grd")
print(date())


output <- raster::raster("./Output/bison_gulch_outputs_par.grd")
png("./run_out.png", width = 1024, height = 1024)
plot(output)
dev.off()

ml_model <- load_model(model_path)
print(ml_model$forest$independent.variable.names)
ml_model$num.trees
# load bands
bandnames <- read.csv("./assets/bands.csv")$x %>% as.vector()
band_names <- read.csv("./assets/bands.csv")$x %>% as.vector()

cl <- raster::beginCluster()#this is actually quite slow, believe it or not

print(date())
#profvis::profvis(
  tile_results <- process_tile(
    test_path_3, 
    ml_model, 
    1,
    cluster = NULL, 
    return_raster = TRUE, 
    band_names = bandnames,
    save_path = "./test/test_raster3_save.grd", 
    suppress_output = FALSE)#,
  #interval = 0.01
#)
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
