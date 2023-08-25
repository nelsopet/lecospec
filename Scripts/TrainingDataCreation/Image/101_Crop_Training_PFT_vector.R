source("Functions/lecospectR.R")
require(terra)
require(raster)
require(rgdal)

#Bison_path = "F:/ORNL_DAAC_DATA_ARCHIVE/BisonGulch/BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or"
Bison_path ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/raw_1511_rd_rf_55pctWhiteRef_or"
Bison_img = brick(Bison_path)
Bison_pft_path = "./Data/Vectors/PFTs/BisonGulchPFT_ROIs.shp"
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


#EightMile_path ="F:/ORNL_DAAC_DATA_ARCHIVE/EightMile/EightMile_2018_07_28_22_56_17_5968_rd_rf_or"
EightMile_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/raw_5968_rd_rf_55pctWhiteRef_or"
EighMile_pft_path = "./Data/Vectors/PFTs/EightmilePFT_ROIs.shp"
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



#TwelveMile_path = "F:/ORNL_DAAC_DATA_ARCHIVE/TwelveMile/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
TwelveMile_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_0_rd_rf_55pctWhiteRef_or"
TwelveMile_pft_path = "./Data/Vectors/PFTs/TwelveMilePFT_ROIs_52_0.shp"
TwelveMile_pft_vec=readOGR(dsn=TwelveMile_pft_path)

TwelveMile_pft_out1<-ImgChopper(TwelveMile_path, TwelveMile_pft_path)
lapply(1:length(TwelveMile_pft_vec),  
       function(x) {
         tst_img <- brick(TwelveMile_path)
         tst_quads<-TwelveMile_pft_vec[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/TwelveMilePFT/TwelveMilePFTs", TwelveMile_pft_vec[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })

#TwelveMile_path2 = "F:/ORNL_DAAC_DATA_ARCHIVE/TwelveMile/TwelveMile_2019_08_09_21_10_22_2000_rd_rf_or"
TwelveMile_path2 ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_2000_rd_rf_55pctWhiteRef_or"
TwelveMile_pft_path2 = "./Data/Vectors/PFTs/TwelveMilePFT_ROIs_22_2000.shp"
TwelveMile_pft_path2_vec=readOGR(dsn=TwelveMile_pft_path2)

#TwelveMile_pft_out2<-ImgChopper(TwelveMile_path2, TwelveMile_pft_path2)

lapply(1:length(TwelveMile_pft_path2_vec),  
       function(x) {
         tst_img <- brick(TwelveMile_path2)
         tst_quads<-TwelveMile_pft_path2_vec[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/TwelveMilePFT/TwelveMilePFTs", TwelveMile_pft_path2_vec[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })


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

#Chatanika validation data cube raw_0_rd_rf_or.hdr
Chatanika_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/raw_0_rd_rf_56pctWhiteRef_or"
Chatanika_pft_path = "./Data/Vectors/PFTs/ChatanikaPFT_ROIs_0_hdr.shp"
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

#Chatanika validation data cube raw_2000_rd_rf_or.hdr
Chatanika_path_2000 = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/raw_2000_rd_rf_or"
Chatanika_pft_path_2000 = "./Data/Vectors/PFTs/ChatanikaPFT_ROIs_2000hdr.shp"
Chatanika_pft_vec_2000=readOGR(dsn=Chatanika_pft_path_2000)
#Chatanika_pft_out<-ImgChopper(Chatanika_path,Chatanika_pft_path)

lapply(1:length(Chatanika_pft_vec_2000),  
       function(x) {
         tst_img <- brick(Chatanika_path_2000)
         tst_quads<-Chatanika_pft_vec_2000[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/ChatanikaPFT/ChatanikaPFTs", Chatanika_pft_vec_2000[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })

#Chatanika validation data cube raw_14080_rd_rf_or.hdr
Chatanika_path_14080 = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/raw_14080_rd_rf_or"
Chatanika_pft_path_14080 = "./Data/Vectors/PFTs/ChatanikaPFT_ROIs_14080hdr.shp"
Chatanika_pft_vec_14080=readOGR(dsn=Chatanika_pft_path_14080)
#Chatanika_pft_out<-ImgChopper(Chatanika_path,Chatanika_pft_path)

lapply(1:length(Chatanika_pft_vec_14080),  
       function(x) {
         tst_img <- brick(Chatanika_path_14080)
         tst_quads<-Chatanika_pft_vec_14080[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/ChatanikaPFT/ChatanikaPFTs", Chatanika_pft_vec_14080[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })
#Bonanza_path = "F:/ORNL_DAAC_DATA_ARCHIVE/Bonanza/raw_6425_rd_rf_or"
Bonanza_path ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/raw_6425_rd_rf_55pctWhiteRef_or"
Bonanza_pft_path = "./Data/Vectors/PFTs/BonanzaPFT_ROIs.shp"
Bonanza_pft_vec=readOGR(dsn=Bonanza_pft_path)
#Bonanza_pft_out<-ImgChopper(Bonanza_path,Bonanza_pft_path)

lapply(1:length(Bonanza_pft_vec),  
       function(x) {
         tst_img <- brick(Bonanza_path)
                  #tst_img <- terra::rast(Bonanza_path)

         tst_quads<-Bonanza_pft_vec[x,]
                  #tst_quads<-Bonanza_pft_vec[1,]

         tst_quads<-sf::st_transform(sf::st_as_sf(tst_quads), crs(tst_img))
                  #tst_img<-terra::project(tst_img, crs(tst_quads))

        tst_crop <- raster::crop(tst_img, tst_quads)
        tst_mask <- raster::mask(tst_crop, tst_quads)
       # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/BonanzaPFT/BonanzaPFTs", Bonanza_pft_vec[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })

#Bonanza SOUTH image
Bonanza_SOUTH_path ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100068_2018_07_25_22_58_40/raw_0_rd_rf_or"
Bonanza_SOUTH_pft_path = "./Data/Vectors/PFTs/BonanzaPFT_ROIs_SOUTH.shp"
Bonanza_SOUTH_pft_vec=readOGR(dsn=Bonanza_SOUTH_pft_path)

lapply(1:length(Bonanza_SOUTH_pft_vec),  
       function(x) {
         tst_img <- brick(Bonanza_SOUTH_path)
                  #tst_img <- terra::rast(Bonanza_SOUTH_path)

         tst_quads<-Bonanza_SOUTH_pft_vec[x,]
                  #tst_quads<-Bonanza_SOUTH_pft_vec[1,]

         tst_quads<-sf::st_transform(sf::st_as_sf(tst_quads), crs(tst_img))
                  #tst_img<-terra::project(tst_img, crs(tst_quads))

        tst_crop <- raster::crop(tst_img, tst_quads)
        tst_mask <- raster::mask(tst_crop, tst_quads)
       # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/Imagery/BonanzaPFT/BonanzaPFTs", Bonanza_SOUTH_pft_vec[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })



