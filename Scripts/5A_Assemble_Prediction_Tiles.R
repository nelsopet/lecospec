source("./Functions/Tile_Assembler.R")
require(mapview)
require(rgdal)
wickersham_path="./Output/B_001_WickershamDome_2019_08_08_19_31_51_0_rd_rf_or_Tile1_PredLayer" 
bison_path="./Output/B_001_BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or_Tile6_PredLayer" 
murph1_path="./Output/B_001_MurphyDome_2018_07_31_19_47_11_24967_rd_rf_or_Tile60_PredLayer"
littlelake_path="./Output/B_001_LittleLake_2018_07_31_01_09_59_132_rd_rf_or_Tile1_PredLayer"
#strsplit( my_path, 'B_001_'*'_Tile')
WickerTestOut<-TileAssembler(wickersham_path)
#writeOGR(WickerTestOut, )
BisonTestOut<-TileAssembler(bison_path)
Murph1TestOut<-TileAssembler(murph1_path)
LittleLakeTestOut<-TileAssembler(littlelake_path)

mapview(WickerTestOut)
mapview(LittleLakeTestOut)
