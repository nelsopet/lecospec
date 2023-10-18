source("./Functions/lecospectR.R")

test_path <- "./Data/Ground_Validation/Imagery/BisonGulchQuads.envi"

raster::raster(test_path) %>% plot()

#Model path but only useful for looking at model. Specify model in config.json
model_path <- "./test/f02a4a5d-beff-418b-8c13-20df941457c7/model.rda"
load_model(model_path) %>% str
#load_model(model_path)$forest %>% str
load_model(model_path)$forest$independent.variable.names
tree1Info<-treeInfo(load_model(model_path), tree=1)
tree1Info$tree<-"tree1"
tree2Info<-treeInfo(load_model(model_path), tree=2)
tree2Info$tree<-"tree2"

bothtreeinfo<-rbind(tree1Info,tree2Info)

bothtreeinfo %>% group_by(tree,splitvarName, splitval) %>% tally() %>% pivot_wider(names_from = tree, values_from = n) %>% arrange(splitvarName, splitval) %>% print(n=500)
bothtreeinfo %>% group_by(tree,prediction) %>% tally() %>% pivot_wider(names_from = tree, values_from = n) %>% arrange(prediction) %>% print(n=500)

load_model(model_path) %>% treeInfo(tree=2) %>% group_by(splitvarName) %>% tally %>% print(n=80)
load_model("./mle/models/gs/3012f5ed-7d17-4e94-a454-24d8a65f5b4f.rda")$call

tree1<-as.data.frame(load_model(model_path)$forest$split.varIDs[1])
tree2<-as.data.frame(load_model(model_path)$forest$split.varIDs[2])
colnames(tree1)<-"splitVarIDs"
colnames(tree2)<-"splitVarIDs"
  branch1<-load_model(model_path)$forest$split.values[1] %>% as.data.frame()
  colnames(branch1)<-"splitVals"
  branch2<-load_model(model_path)$forest$split.values[2] %>% as.data.frame()
  colnames(branch2)<-"splitVals"

  tree1.2<-cbind(tree1,branch1)
  tree2.2<-cbind(tree2,branch2)

  tree1.2$tree<-"1"
  tree2.2$tree<-"2"

  tree3<-rbind(tree1.2,tree2.2)
  head(tree3)
  tree3 %>% group_by(splitVarIDs,tree) %>% tally() %>% print(n=142)

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

#1 tree model
#load_model("mle/models/gs/9cc7039c-c787-4ff4-8f44-b9ceaec36204.rda")

#2 tree model
load_model("mle/models/gs/3012f5ed-7d17-4e94-a454-24d8a65f5b4f.rda")$forest$levels

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

#print(date())
#closeAllConnections()
#big_results <- estimate_land_cover(
#  Bonanza_path,
#  output_filepath = "./Output/dev_FullCube/bz_6425_adaboost.grd")
#print(date())
 bc_pred6<-raster("./Output/dev_FullCube/bz_6425_adaboost.grd")
 writeRaster(bc_pred6,"./Output/dev_FullCube/bz_6425_adaboost.tif", overwrite=TRUE)

  plot_options <- define_plot_options(
      title = "Bonanza Predictions",
      xLabel = "Longitude",
      yLabel = "Latitude"
)
  bz_map_1 <- plot_categorical_raster(bc_pred6, plot_options)

  windows();bz_map_1

  ggsave("./figures/bz_6425_adaboost.png", dpi = 350, width = 12, height = 8, units = "in")

#closeAllConnections()
#big_results <- estimate_land_cover(
#  EightMile_path,
#  output_filepath = "./Output/dev_FullCube/em_5968_adaboost.grd")
#print(date())
 em_pred8<-raster("./Output/dev_FullCube/em_5968_adaboost.grd")
  writeRaster(em_pred8,"./Output/dev_FullCube/em_5968_adaboost.tif", overwrite=TRUE)
    plot_options <- define_plot_options(
        title = "Eight Mile Predictions",
        xLabel = "Longitude",
        yLabel = "Latitude"
    )
    em_map <- plot_categorical_raster(em_pred8, plot_options)

    windows();em_map

    ggsave("./figures/em_5968_adaboost.png", dpi = 350, width = 12, height = 8, units = "in")

#closeAllConnections()
#big_results <- estimate_land_cover(
#  Chatanika_path,
#  output_filepath = "./Output/dev_FullCube/ch_0_adaboost.grd")
#print(date())
 ch_pred8<-raster("./Output/dev_FullCube/ch_0_adaboost.grd")
 writeRaster(ch_pred8,"./Output/dev_FullCube/ch_0_adaboost.tif", overwrite=TRUE)

    plot_options <- define_plot_options(
        title = "Chatanika Predictions",
        xLabel = "Longitude",
        yLabel = "Latitude"
    )

    ca_map <- plot_categorical_raster(ch_pred8, plot_options)

    windows();ca_map

    ggsave("./figures/ch_0_adaboost.png", dpi = 350, width = 12, height = 8, units = "in")


##Bison Gulch validation data cube prediction, conversion to tif, plotting as a png for ms
#closeAllConnections()
#big_results <- estimate_land_cover(
#  Bison_path,
#  output_filepath = "./Output/dev_FullCube/bg_1511_fncgrp1_PREDICTIONS_adaboost.grd")
#print(date())
 bg_pred10<-raster("./Output/dev_FullCube/bg_1511_fncgrp1_PREDICTIONS_adaboost.grd")
  writeRaster(bg_pred10,"./Output/dev_FullCube/bg_1511_fncgrp1_PREDICTIONS_adaboost.tif", overwrite=TRUE)
    plot_options <- define_plot_options(
        title = "Bison Gulch Predictions",
        xLabel = "Longitude",
        yLabel = "Latitude"
    )
    bg_map <- plot_categorical_raster(bg_pred9, plot_options)

    windows();bg_map

    ggsave("./figures/bg_1511_fncgrp1_PREDICTIONS_adaboost.png", dpi = 350, width = 12, height = 8, units = "in")
