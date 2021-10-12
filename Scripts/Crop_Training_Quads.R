source("Functions/Image_Chopper.R")

Bison_path = "F:/BisonGulch/raw_1511_rd_rf_or"
Bison_seg_path = "F:/BisonGulch/Bisoon_Quadrats.shp"
Bison_out<-ImgChopper(Bison_path, Bison_seg_path)
plot(Bison_out[[2]])
writeRaster(Bison_out[[2]], "./Data/Ground_Validation/BisonGulchQuads", format = "ENVI", overwrite = TRUE)

EightMile_path = "F:/EightMile/raw_2374_rd_or"
EighMile_seg_path = "F:/EightMile/EightMile_Quadrats_ALL.shp"
EightMile_out<-ImgChopper(EightMile_path, EighMile_seg_path)
plot(EightMile_out[[2]])
writeRaster(EightMile_out[[2]], "./Data/Ground_Validation/EightMileQuads", format = "ENVI", overwrite = TRUE)

TwelveMile_path = "F:/TwelveMile/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
TwelveMile_seg_path = "F:/TwelveMile/TwelveMileQ0_10_20_30_40m.shp"
TwelveMile_out<-ImgChopper(TwelveMile_path, TwelveMile_seg_path)

TwelveMile_path2 = "F:/TwelveMile/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or"
TwelveMile_seg_path2 = "F:/TwelveMile/TwelveMileQ70_80_90_100m.shp"
TwelveMile_out2<-ImgChopper(TwelveMile_path2, TwelveMile_seg_path2)

plot(TwelveMile_out[[2]])
writeRaster(TwelveMile_out[[2]], "./Data/Ground_Validation/TwelveMileGulchQuads", format = "ENVI", overwrite = TRUE)


