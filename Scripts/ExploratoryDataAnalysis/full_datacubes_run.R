source("./Functions/lecospectR.R")

test_path <- "./Data/Ground_Validation/Imagery/BisonGulchQuads.envi"

raster::raster(test_path) %>% plot()

#Flight lines for use in manuscript
Bison_dir="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/"
Bonanza_dir="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/"
EightMile_dir = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/"
Chatanika_dir = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/"

#Get image names for running predictions for all datacubes in a flight
Bison_imgs<-list.files(Bison_dir)[grepl("*_rd_rf_or$",list.files(Bison_dir))]
Bonanza_imgs<-list.files(Bonanza_dir)[grepl("*_rd_rf_or$",list.files(Bonanza_dir))]
EightMile_imgs<-list.files(EightMile_dir)[grepl("*_rd_rf_or$",list.files(EightMile_dir))]
Chatanika_imgs<-list.files(Chatanika_dir)[grepl("*_rd_rf_or$",list.files(Chatanika_dir))]

#Test to make sure a datacube reads in with the list of files and the path name to each site
#brick(paste(Bison_dir,Bison_imgs[2], sep="")) #PASS

#Single images selected for use for manuscript
Bison_path ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/raw_1511_rd_rf_55pctWhiteRef_or"
Bonanza_path ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/raw_6425_rd_rf_55pctWhiteRef_or"
EightMile_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/raw_5968_rd_rf_55pctWhiteRef_or"
Chatanika_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/raw_0_rd_rf_56pctWhiteRef_or"
TwelveMile_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_0_rd_rf_55pctWhiteRef_or"
TwelveMile_path2 ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_2000_rd_rf_55pctWhiteRef_or"

#Paths to original ORNL_DAAC_DATA_ARCHIVE prior to radiometric reprocessing
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


#Test estimate land cover on a single quadrat image
#set.seed(1234)
print(date())
quad_results <- estimate_land_cover(
  test_path, 
  output_filepath = "./test/test_pred_adaboost.grd",
  use_external_bands = TRUE)
closeAllConnections()
print(date())

as_tiff<-function(path) {raster::raster(paste0("./",path,".grd")) %>% raster::writeRaster(paste0("./test/",path,".tif"), overwrite=TRUE)}

test_tif<-as_tiff("test_pred_2tree_tile_34M0bC2ZrwSZkgXo_patch_seed1234")

#Make a list of model IDs to use and change
RF_best_mods<-list.dirs("./test/GridSearchResults/Ranger/", recursive = FALSE)
  #RF_best_mods
  RF_best_model_path<-"./test/GridSearchResults/Ranger/4a929f4b-73af-4bfc-8df3-a73b2af1bd4f/4a929f4b-73af-4bfc-8df3-a73b2af1bd4f/model.rda"
  load_model(RF_best_model_path)
  #[1] "./test/GridSearchResults/Ranger/3d36ae2c-9737-4601-b2d5-f5edb1a9ba00"
  #[2] "./test/GridSearchResults/Ranger/4a929f4b-73af-4bfc-8df3-a73b2af1bd4f"
  #[3] "./test/GridSearchResults/Ranger/ab38f80f-dc94-4002-a404-e18256dbc2a1"
  #[4] "./test/GridSearchResults/Ranger/f39c1f0d-041d-4651-ad62-52c1763bfaaa"
Adaboost_best_mods<-list.dirs("./test/GridSearchResults/Ranger/", recursive = FALSE)
PLS_best_mods<-list.dirs("./test/GridSearchResults/PLS/", recursive = FALSE)

#Predict full data cubes for each site in the study area and visualize it in a map
#Uncomment out a section below for each site to run a prediction on a full data cube

##Bonanza validation data cube prediction, conversion to tif, plotting as a png for ms

print(date())
closeAllConnections()
big_results <- estimate_land_cover(
  Bonanza_path,
  output_filepath = "./Output/dev_FullCube/bz_6425_RF_a73b2af1bd4f.grd")
print(date())
 bc_pred6<-raster("./Output/dev_FullCube/bz_6425_RF_a73b2af1bd4f.grd")
 writeRaster(bc_pred6,"./Output/dev_FullCube/bz_6425_RF_a73b2af1bd4f.tif", overwrite=TRUE)

  plot_options <- define_plot_options(
      title = "Bonanza Predictions",
      xLabel = "Longitude",
      yLabel = "Latitude"
)
  bz_map_1 <- plot_categorical_raster(bc_pred6, plot_options)

  windows();bz_map_1

  ggsave("./figures/bz_6425_RF_a73b2af1bd4f.png", dpi = 350, width = 12, height = 8, units = "in")

  dev.off()

##Eight Mile validation data cube prediction, conversion to tif, plotting as a png for ms
closeAllConnections()
big_results <- estimate_land_cover(
  EightMile_path,
  output_filepath = "./Output/dev_FullCube/em_5968_RF_a73b2af1bd4f.grd")
print(date())
 em_pred8<-raster("./Output/dev_FullCube/em_5968_RF_a73b2af1bd4f.grd")
  writeRaster(em_pred8,"./Output/dev_FullCube/em_5968_RF_a73b2af1bd4f.tif", overwrite=TRUE)
    plot_options <- define_plot_options(
        title = "Eight Mile Predictions",
        xLabel = "Longitude",
        yLabel = "Latitude"
    )
    em_map <- plot_categorical_raster(em_pred8, plot_options)

    windows();em_map

    ggsave("./figures/em_5968_RF_a73b2af1bd4f.png", dpi = 350, width = 12, height = 8, units = "in")

    dev.off()

##Chatanika validation data cube prediction, conversion to tif, plotting as a png for ms

closeAllConnections()
big_results <- estimate_land_cover(
  Chatanika_path,
  output_filepath = "./Output/dev_FullCube/ch_0_RF_a73b2af1bd4f.grd")
print(date())
 ch_pred8<-raster("./Output/dev_FullCube/ch_0_RF_a73b2af1bd4f.grd")
 writeRaster(ch_pred8,"./Output/dev_FullCube/ch_0_RF_a73b2af1bd4f.tif", overwrite=TRUE)

    plot_options <- define_plot_options(
        title = "Chatanika Predictions",
        xLabel = "Longitude",
        yLabel = "Latitude"
    )

    ca_map <- plot_categorical_raster(ch_pred8, plot_options)

    windows();ca_map

    ggsave("./figures/ch_0_RF_a73b2af1bd4f.png", dpi = 350, width = 12, height = 8, units = "in")

  dev.off()

##Bison Gulch validation data cube prediction, conversion to tif, plotting as a png for ms

closeAllConnections()
big_results <- estimate_land_cover(
  Bison_path,
  output_filepath = "./Output/dev_FullCube/bg_1511_RF_a73b2af1bd4f.grd")
print(date())
bg_pred10<-raster("./Output/dev_FullCube/bg_1511_RF_a73b2af1bd4f.grd")
 writeRaster(bg_pred10,"./Output/dev_FullCube/bg_1511_RF_a73b2af1bd4f.tif", overwrite=TRUE)
   plot_options <- define_plot_options(
       title = "Bison Gulch Predictions",
       xLabel = "Longitude",
       yLabel = "Latitude"
   )
   bg_map <- plot_categorical_raster(bg_pred9, plot_options)
   windows();bg_map
   ggsave("./figures/bg_1511_RF_a73b2af1bd4f.png", dpi = 350, width = 12, height = 8, units = "in")
   dev.off()
