source("./Functions/lecospectR.R")

test_path <- "./Data/Ground_Validation/Imagery/BisonGulchQuads.envi"

raster::raster(test_path) %>% plot()

#Model path but only useful for looking at model. Specify model in config.json
model_path <- "./mle/models/gs/3012f5ed-7d17-4e94-a454-24d8a65f5b4f.rda"

#Paths to images
Bison_path ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/raw_1511_rd_rf_55pctWhiteRef_or"
Bonanza_path ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/raw_6425_rd_rf_55pctWhiteRef_or"
EightMile_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/raw_5968_rd_rf_55pctWhiteRef_or"
Chatanika_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/raw_0_rd_rf_56pctWhiteRef_or"
TwelveMile_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_0_rd_rf_55pctWhiteRef_or"
TwelveMile_path2 ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_2000_rd_rf_55pctWhiteRef_or"

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

#1 tree model
#load_model("mle/models/gs/9cc7039c-c787-4ff4-8f44-b9ceaec36204.rda")

#2 tree model
load_model("mle/models/gs/3012f5ed-7d17-4e94-a454-24d8a65f5b4f.rda")$forest$levels

#set.seed(1234)
print(date())
quad_results <- estimate_land_cover(
  test_path_3, 
  output_filepath = "./test_pred_2tree_tile_34M0bC2ZrwSZkgXo_patch_seed1234.grd",
  use_external_bands = TRUE)
closeAllConnections()
print(date())

as_tiff<-function(path) {raster::raster(paste0("./",path,".grd")) %>% raster::writeRaster(paste0("./test/",path,".tif"), overwrite=TRUE)}

test_tif<-as_tiff("test_pred_2tree_tile_34M0bC2ZrwSZkgXo_patch_seed1234")

print(date())
closeAllConnections()
big_results <- estimate_land_cover(
  Bonanza_path,
  output_filepath = "./Output/dev_FullCube/bz_6425_fncgrp1_PREDICTIONS_img_balanced_2tree_ranger_NoDataFix.grd")
print(date())

closeAllConnections()
big_results <- estimate_land_cover(
  EightMile_path,
  output_filepath = "./Output/dev_FullCube/em_5968_fncgrp1_PREDICTIONS_img_balanced_2tree_ranger_NoDataFix.grd")
print(date())

closeAllConnections()
big_results <- estimate_land_cover(
  Chatanika_path,
  output_filepath = "./Output/dev_FullCube/ch_0_fncgrp1_PREDICTIONS_img_balanced_2tree_ranger_NoDataFix.grd")
print(date())

closeAllConnections()
big_results <- estimate_land_cover(
  Bison_path,
  output_filepath = "./Output/dev_FullCube/bg_1511_fncgrp1_PREDICTIONS_img_balanced_2tree_ranger_NoDataFix.grd")
print(date())