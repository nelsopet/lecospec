source("./Functions/lecospectR.R")
require(terra)
require(raster)
require(rgdal)

#Bison_path = "F:/ORNL_DAAC_DATA_ARCHIVE/BisonGulch/BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or"
Bison_path ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/raw_1511_rd_rf_55pctWhiteRef_or"
Bison_img = brick(Bison_path)
#plot(Bison_img)
#hsdar::bandnames(Bison_img)
Bison_seg_path = "./Data/Vectors/Bisoon_Quadrats_georeferenced.shp"
Bison_out<-ImgChopper(Bison_path, Bison_seg_path)
#str(Bison_out[[2]])
#plot(Bison_out[[2]])

#EightMile_path = "F:/EightMile/raw_2374_rd_rf_or"
#EightMile_path ="F:/ORNL_DAAC_DATA_ARCHIVE/EightMile/EightMile_2018_07_28_22_56_17_5968_rd_rf_or"
EightMile_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/raw_5968_rd_rf_55pctWhiteRef_or"
#EighMile_seg_path = "F:/EightMile/EightMile_Quadrats_ALL.shp"
EighMile_seg_path = "./Data/Vectors/EightMile_Quadrats_revised.shp"
EightMile_out<-ImgChopper(EightMile_path, EighMile_seg_path)

#TwelveMile_path = "F:/ORNL_DAAC_DATA_ARCHIVE/TwelveMile/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
TwelveMile_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_0_rd_rf_55pctWhiteRef_or"
TwelveMile_seg_path = "./Data/Vectors/TwelveMileQ0_10_20_30_40m.shp"
TwelveMile_out1<-ImgChopper(TwelveMile_path, TwelveMile_seg_path)

#TwelveMile_path2 = "F:/ORNL_DAAC_DATA_ARCHIVE/TwelveMile/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or"
TwelveMile_path2 ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_2000_rd_rf_55pctWhiteRef_or"
TwelveMile_seg_path2 = "./Data/Vectors/TwelveMileQ70_80_90_100m.shp"
TwelveMile_out2<-ImgChopper(TwelveMile_path2, TwelveMile_seg_path2)

#MurphyDome_path1 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_10350_rd_rf_or"
MurphyDome_path1 = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73118/ImagingSpectrometer/DataFolders/100158_MurphyDomeFlight1_2018_07_31_19_47_11/raw_10350_rd_rf_55pctWhiteRef_or"
#MurphyDome_path2 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_16674_rd_rf_or"
MurphyDome_path2 = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73118/ImagingSpectrometer/DataFolders/100158_MurphyDomeFlight1_2018_07_31_19_47_11/raw_16674_rd_rf_55pctWhiteRef_or"
#MurphyDome_path3 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_24967_rd_rf_or"
MurphyDome_path3 ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73118/ImagingSpectrometer/DataFolders/100158_MurphyDomeFlight1_2018_07_31_19_47_11/raw_24967_rd_rf_55pctWhiteRef_or"

MurphyDome_seg_path1 = "./Data/Vectors/MurphyQuads0_10m.shp"
MurphyDome_seg_path2 = "./Data/Vectors/MurphyQuads20_50m.shp"
MurphyDome_seg_path3 = "./Data/Vectors/MurphyQuads60_100m.shp"


MurphyDome_out1<-ImgChopper(MurphyDome_path1, MurphyDome_seg_path1)
MurphyDome_out2<-ImgChopper(MurphyDome_path2, MurphyDome_seg_path2)
MurphyDome_out3<-ImgChopper(MurphyDome_path3, MurphyDome_seg_path3)

#Chatanika_path = "F:/ORNL_DAAC_DATA_ARCHIVE/Chatnika/Chatnika_2018_07_29_20_32_59_0_rd_rf_or"
Chatanika_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/raw_0_rd_rf_56pctWhiteRef_or"
Chatanika_seg_path = "./Data/Vectors/ChatanikaQuads_georeferenced.shp"
#Remove quadrat 100m since it has not validation data due to scanline chatter
tst <- terra::vect(Chatanika_seg_path)
tst[-1,] %>% dim
terra::writeVector(tst[-1,], "./Data/Vectors/ChatanikaQuads_georeferenced_0_90m.shp")
Chatanika_seg_path_0_90m = "./Data/Vectors/ChatanikaQuads_georeferenced_0_90m.shp"
Chatanika_out<-ImgChopper(Chatanika_path,Chatanika_seg_path_0_90m)



#Bonanza_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/raw_6425_rd_rf_or"
Bonanza_path ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/raw_6425_rd_rf_55pctWhiteRef_or"
Bonanza_seg_path = "./Data/Vectors/Bonanza_N_Quads.shp"
Bonanza_out<-ImgChopper(Bonanza_path,Bonanza_seg_path)

writeRaster(Bison_out, "./Data/Ground_Validation/Imagery/BisonGulchQuads", format = "ENVI", overwrite = TRUE)
writeRaster(Chatanika_out,   "./Data/Ground_Validation/Imagery/ChatanikaQuads", format  = "ENVI", overwrite = TRUE)
writeRaster(EightMile_out,   "./Data/Ground_Validation/Imagery/EightMileQuads", format = "ENVI", overwrite = TRUE)
writeRaster(TwelveMile_out1, "./Data/Ground_Validation/Imagery/TwelveMileGulchQuads1", format = "ENVI", overwrite = TRUE)
writeRaster(TwelveMile_out2, "./Data/Ground_Validation/Imagery/TwelveMileGulchQuads2", format = "ENVI", overwrite = TRUE)
writeRaster(MurphyDome_out1, "./Data/Ground_Validation/Imagery/MurphDomeQuads0_10", format = "ENVI", overwrite = TRUE)
writeRaster(MurphyDome_out2, "./Data/Ground_Validation/Imagery/MurphDomeQuads20_50", format = "ENVI", overwrite = TRUE)
writeRaster(MurphyDome_out3, "./Data/Ground_Validation/Imagery/MurphDomeQuads60_100", format = "ENVI", overwrite = TRUE)
writeRaster(Bonanza_out, "./Data/Ground_Validation/Imagery/BonanzaQuads", format = "ENVI", overwrite = TRUE)


