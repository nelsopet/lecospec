source("Functions/Image_Chopper.R")
require(terra)
require(raster)
require(rgdal)

Bison_path = "F:/ORNL_DAAC_DATA_ARCHIVE/BisonGulch/BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or"
Bison_img = brick(Bison_path)
Bison_pft_path = "./Data/Vectors/BisonGulchPFT_ROIs.shp"
Bison_pft_vec<-readOGR(dsn=Bison_pft_path)

#Bison_pft_out<-ImgChopper(Bison_path, Bison_pft_path)

lapply(1:length(Bison_pft_vec),  
       function(x) {
         tst_img <- brick(Bison_path)
         tst_quads<-Bison_pft_vec[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         metadata(tst_mask)<-as.list(Bison_pft_vec[x,]$CLASS_NAME)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/BisonPFT/BisonGulchPFTs", Bison_pft_vec[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })


EightMile_path ="F:/ORNL_DAAC_DATA_ARCHIVE/EightMile/EightMile_2018_07_28_22_56_17_5968_rd_rf_or"
EighMile_pft_path = "./Data/Vectors/EightmilePFT_ROIs.shp"
EighMile_pft_vec<-readOGR(dsn=EighMile_pft_path)
#EightMile_pft_out<-ImgChopper(EightMile_path, EighMile_pft_path)

lapply(1:length(EighMile_pft_vec),  
       function(x) {
         tst_img <- brick(EightMile_path)
         tst_quads<-EighMile_pft_vec[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/EightMilePFT/EightMilePFTs", EighMile_pft_vec[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })



#TwelveMile_path = "F:/TwelveMile/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
#TwelveMile_pft_path = "F:/TwelveMile/TwelveMileQ0_10_20_30_40m.shp"
#TwelveMile_pft_out1<-ImgChopper(TwelveMile_path, TwelveMile_pft_path)

#TwelveMile_path2 = "F:/TwelveMile/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or"
#TwelveMile_pft_path2 = "F:/TwelveMile/TwelveMileQ70_80_90_100m.shp"
#TwelveMile_pft_out2<-ImgChopper(TwelveMile_path2, TwelveMile_pft_path2)

#MurphyDome_path1 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_10350_rd_rf_or"
#MurphyDome_path2 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_16674_rd_rf_or"
#MurphyDome_path3 = "E:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_24967_rd_rf_or"

#MurphyDome_pft_path1 = "./Data/Vectors/MurphyQuads0_10m.shp"
#MurphyDome_pft_path2 = "./Data/Vectors/MurphyQuads20_50m.shp"
#MurphyDome_pft_path3 = "./Data/Vectors/MurphyQuads60_100m.shp"


#MurphyDome_pft_out1<-ImgChopper(MurphyDome_path1, MurphyDome_pft_path1)
#MurphyDome_pft_out2<-ImgChopper(MurphyDome_path2, MurphyDome_pft_path2)
#MurphyDome_pft_out3<-ImgChopper(MurphyDome_path3, MurphyDome_pft_path3)


#TwelveMile_list<-c(TwelveMile_pft_out1,TwelveMile_pft_out2)

#TwelveMile_pft_out<-do.call(merge, TwelveMile_list)

Chatanika_path = "F:/ORNL_DAAC_DATA_ARCHIVE/Chatnika/Chatnika_2018_07_29_20_32_59_0_rd_rf_or"
Chatanika_pft_path = "./Data/Vectors/ChatanikaPFT_ROIs.shp"
Chatanika_pft_vec=readOGR(dsn=Chatanika_pft_path)
#Chatanika_pft_out<-ImgChopper(Chatanika_path,Chatanika_pft_path)

lapply(1:length(Chatanika_pft_vec),  
       function(x) {
         tst_img <- brick(Chatanika_path)
         tst_quads<-Chatanika_pft_vec[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/ChatanikaPFT/ChatanikaPFTs", Chatanika_pft_vec[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })



writeRaster(Bison_pft_sep_out,       "./Data/Ground_Validation/Imagery/BisonGulchPFTs", format = "ENVI", overwrite = TRUE)
writeRaster(Chatanika_pft_out,   "./Data/Ground_Validation/Imagery/ChatanikaPFTs", format  = "ENVI", overwrite = TRUE)
writeRaster(EightMile_pft_out,   "./Data/Ground_Validation/Imagery/EightMilePFTs", format = "ENVI", overwrite = TRUE)
#writeRaster(MurphyDome_pft_out1, "./Data/Ground_Validation/Imagery/TwelveMileGulchQuads1", format = "ENVI", overwrite = TRUE)
#writeRaster(TwelveMile_pft_out2, "./Data/Ground_Validation/Imagery/TwelveMileGulchQuads2", format = "ENVI", overwrite = TRUE)
#writeRaster(MurphyDome_pft_out1, "./Data/Ground_Validation/Imagery/MurphDomeQuads0_10", format = "ENVI", overwrite = TRUE)
#writeRaster(MurphyDome_pft_out2, "./Data/Ground_Validation/Imagery/MurphDomeQuads20_50", format = "ENVI", overwrite = TRUE)
#writeRaster(MurphyDome_pft_out3, "./Data/Ground_Validation/Imagery/MurphDomeQuads60_100", format = "ENVI", overwrite = TRUE)


