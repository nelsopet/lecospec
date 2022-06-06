source("Functions/Image_Chopper.R")
require(terra)
require(raster)
require(rgdal)

Bison_path = "F:/BisonGulch/raw_1511_rd_rf_or"
Bison_img = brick(Bison_path)
#plot(Bison_img)
#hsdar::bandnames(Bison_img)
Bison_seg_path = "Data/Ground_Validation/Bison_Quadrats/Bison_Quadrats.shp"
Bison_out<-ImgChopper(Bison_path, Bison_seg_path)
#str(Bison_out[[2]])
#plot(Bison_out[[2]])
writeRaster(Bison_out[[2]], "./Data/Ground_Validation/BisonGulchQuads", format = "ENVI", overwrite = TRUE)

#EightMile_path = "F:/EightMile/raw_2374_rd_rf_or"
EightMile_path ="E:/ORNL_DAAC_DATA_ARCHIVE/EightMile/EightMile_2018_07_28_22_56_17_5968_rd_rf_or"
#EighMile_seg_path = "F:/EightMile/EightMile_Quadrats_ALL.shp"
EighMile_seg_path = "./Data/Vectors/EightMile_Quadrats_revised.shp"
EightMile_out<-ImgChopper(EightMile_path, EighMile_seg_path)
plot(EightMile_out$X397.593.nm)

TwelveMile_path = "F:/TwelveMile/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
TwelveMile_seg_path = "F:/TwelveMile/TwelveMileQ0_10_20_30_40m.shp"
TwelveMile_out1<-ImgChopper(TwelveMile_path, TwelveMile_seg_path)

TwelveMile_path2 = "F:/TwelveMile/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or"
TwelveMile_seg_path2 = "F:/TwelveMile/TwelveMileQ70_80_90_100m.shp"
TwelveMile_out2<-ImgChopper(TwelveMile_path2, TwelveMile_seg_path2)

MurphyDome_path1 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_10350_rd_rf_or"
MurphyDome_path2 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_16674_rd_rf_or"
MurphyDome_path3 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_24967_rd_rf_or"

MurphyDome_seg_path1 = "./Data/Vectors/MurphyQuads0_10m.shp"
MurphyDome_seg_path2 = "./Data/Vectors/MurphyQuads20_50m.shp"
MurphyDome_seg_path3 = "./Data/Vectors/MurphyQuads60_100m.shp"


MurphyDome_out1<-ImgChopper(MurphyDome_path1, MurphyDome_seg_path1)
MurphyDome_out2<-ImgChopper(MurphyDome_path2, MurphyDome_seg_path2)
MurphyDome_out3<-ImgChopper(MurphyDome_path3, MurphyDome_seg_path3)


#TwelveMile_list<-c(TwelveMile_out1,TwelveMile_out2)

#TwelveMile_out<-do.call(merge, TwelveMile_list)

Chatanika_path = "E:/ORNL_DAAC_DATA_ARCHIVE/Chatnika/Chatnika_2018_07_29_20_32_59_0_rd_rf_or"
Chatanika_seg_path = "M:/lecospec/lecospec/Data/Vectors/ChatanikaQuads.shp"
Chatanika_out<-ImgChopper(Chatanika_path,Chatanika_seg_path)
#plot(TwelveMile_out[[2]])

writeRaster(Chatanika_out,   "./Data/Ground_Validation/Imagery/ChatanikaQuads", format  = "ENVI", overwrite = TRUE)
writeRaster(EightMile_out,   "./Data/Ground_Validation/Imagery/EightMileQuads", format = "ENVI", overwrite = TRUE)
writeRaster(MurphyDome_out1, "./Data/Ground_Validation/Imagery/TwelveMileGulchQuads1", format = "ENVI", overwrite = TRUE)
writeRaster(TwelveMile_out2, "./Data/Ground_Validation/Imagery/TwelveMileGulchQuads2", format = "ENVI", overwrite = TRUE)
writeRaster(MurphyDome_out1, "./Data/Ground_Validation/Imagery/MurphDomeQuads0_10", format = "ENVI", overwrite = TRUE)
writeRaster(MurphyDome_out2, "./Data/Ground_Validation/Imagery/MurphDomeQuads20_50", format = "ENVI", overwrite = TRUE)
writeRaster(MurphyDome_out3, "./Data/Ground_Validation/Imagery/MurphDomeQuads60_100", format = "ENVI", overwrite = TRUE)


