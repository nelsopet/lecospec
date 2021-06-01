source("./Functions/Tile_Assembler.R")
require(mapview)
wickersham_path="./Output/B_001_WickershamDome_2019_08_08_19_31_51_0_rd_rf_or_Tile1_PredLayer" 
_path="./Output/B_001_WickershamDome_2019_08_08_19_31_51_0_rd_rf_or_Tile1_PredLayer" 

#strsplit( my_path, 'B_001_'*'_Tile')
WickerTestOut<-TileAssembler(wickersham_path)

mapview(WickerTestOut)
