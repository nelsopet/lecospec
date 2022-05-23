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

EightMile_path = "F:/EightMile/raw_2374_rd_rf_or"
EighMile_seg_path = "F:/EightMile/EightMile_Quadrats_ALL.shp"
EightMile_out<-ImgChopper(EightMile_path, EighMile_seg_path)
plot(EightMile_out$X397.593.nm)

TwelveMile_path = "F:/TwelveMile/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
TwelveMile_seg_path = "F:/TwelveMile/TwelveMileQ0_10_20_30_40m.shp"
TwelveMile_out1<-ImgChopper(TwelveMile_path, TwelveMile_seg_path)

TwelveMile_path2 = "F:/TwelveMile/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or"
TwelveMile_seg_path2 = "F:/TwelveMile/TwelveMileQ70_80_90_100m.shp"
TwelveMile_out2<-ImgChopper(TwelveMile_path2, TwelveMile_seg_path2)


#TwelveMile_list<-c(TwelveMile_out1,TwelveMile_out2)

#TwelveMile_out<-do.call(merge, TwelveMile_list)

Chatanika_path = "E:/ORNL_DAAC_DATA_ARCHIVE/Chatnika/Chatnika_2018_07_29_20_32_59_0_rd_rf_or"
Chatanika_seg_path = "M:/lecospec/lecospec/Data/Vectors/ChatanikaQuads.shp"
Chatanika_out<-ImgChopper(Chatanika_path,Chatanika_seg_path)
#plot(TwelveMile_out[[2]])
writeRaster(Chatanika_out, "./Data/Ground_Validation/ChatanikaQuads", format  = "ENVI", overwrite = TRUE)
writeRaster(EightMile_out, "./Data/Ground_Validation/EightMileQuads", format = "ENVI", overwrite = TRUE)
writeRaster(TwelveMile_out1, "./Data/Ground_Validation/TwelveMileGulchQuads1", format = "ENVI", overwrite = TRUE)
writeRaster(TwelveMile_out2, "./Data/Ground_Validation/TwelveMileGulchQuads2", format = "ENVI", overwrite = TRUE)


