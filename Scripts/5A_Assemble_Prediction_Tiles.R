source("./Functions/Tile_Assembler.R")
require(mapview)
require(rgdal)
wickersham_path="./Output/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or" 
bison_path="./Output/B_001_BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or_Tile6_PredLayer" 
murph1_path="./Output/B_001_MurphyDome_2018_07_31_19_47_11_24967_rd_rf_or_Tile60_PredLayer"
littlelake_path="./Output/B_001_LittleLake_2018_07_31_01_09_59_132_rd_rf_or_Tile1_PredLayer"
chatanika_path ="./Output/B_001_Chatnika_2018_07_29_20_32_59_0_rd_rf_or_Tile1_PredLayer"
eagle_path = "./Output/B_001_EagleSummit_2019_08_06_00_57_02_2546_rd_rf_or_Tile40_PredLayer"
twelvemile_path_l2000 = "./Output/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or"
twelvemile_path_l0 = "./Output/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
twelvemile_path_FncGrp2 = "./Output/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"

#strsplit( my_path, 'B_001_'*'_Tile')


##Assemble species level maps
WickerTestOut<-TileAssembler(wickersham_path)
writeRaster(
  WickerTestOut,
  filename = "Output/Prediction/WickerTestOut.tif",
  overwrite = TRUE)

BisonTestOut<-TileAssembler(bison_path)
writeRaster(
  BisonTestOut,
  filename = "Output/Prediction/BisonTestOut.tif",
  overwrite = TRUE)

Murph1TestOut<-TileAssembler(murph1_path)
writeRaster(
  Murph1TestOut,
  filename = "Output/Prediction/Murph1TestOut.tif",
  overwrite = TRUE)

LittleLakeTestOut<-TileAssembler(littlelake_path)
writeRaster(
  LittleLakeTestOut,
  filename = "Output/Prediction/LittleLakeTestOut.tif",
  overwrite = TRUE)

ChatanikaTestOut<-TileAssembler(chatanika_path)
writeRaster(
  ChatanikaTestOut,
  filename = "Output/Prediction/ChatanikaTestOut.tif",
  overwrite = TRUE)

EagleTestOut<-TileAssembler(eagle_path)
writeRaster(
  EagleTestOut,
  filename = "Output/Prediction/EagleTestOut.tif",
  overwrite = TRUE)

TwelveMileTestOut_l0<-TileAssembler(twelvemile_path_l0)
writeRaster(
  TwelveMileTestOut_l0,
  filename = "Output/Prediction/TwelveMileTestOut_lo.tif",
  overwrite = TRUE)

TwelveMileTestOut_l2000<-TileAssembler(twelvemile_path_l2000)
writeRaster(
  TwelveMileTestOut_l2000,
  filename = "Output/Prediction/TwelveMileTestOut_l2000.tif",
  overwrite = TRUE)
##Assemble genera level maps
twelvemile_genera_path = "./Output/B_001_TwelveMile_2019_08_09_21_28_52_0_rd_rf_or_Tile1_PredLayer_Genera"
twelvemile_genera_path2 = "./Output/B_001_TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or_Tile1_PredLayer_Genera"


TwelveMileTestOut_FncGrp2<-TileAssembler(twelvemile_path_FncGrp2)
writeRaster(
  TwelveMileTestOut_FncGrp2,
  filename = "Output/Prediction/Genera/TwelveMileTestOut_FncGrp2.tif",
  overwrite = TRUE)
