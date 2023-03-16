source("Functions/lecospectR.R")
require(terra)
require(raster)
require(rgdal)

Tarp_img_path = "F:/ORNL_DAAC_DATA_ARCHIVE/Bonanza/raw_6425_rd_rf_or"
Tarp_img = brick(Tarp_img_path)
Tarp_pft_path = "C:/Users/Nelson Lab/Desktop/lecospec_figs/BonanzaTarp.shp"
Tarp_pft_vec<-readOGR(dsn=Tarp_pft_path)
Tarp_pft_vec_sf<-sf::st_as_sf(Tarp_pft_vec)
tst_crs<-crs(tst_img)
Tarp_pft_vec_sf_proj<-sf::st_transform(Tarp_pft_vec_sf, tst_crs)
  
lapply(1:3,  
       function(x) {
         tst_img <- brick(Tarp_img_path)
         tst_quads<-Tarp_pft_vec_sf_proj[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         metadata(tst_mask)<-as.list(Tarp_pft_vec_sf_proj[x,]$CLASS_NAME)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/WhiteRefs/Tarp/", Tarp_pft_vec_sf_proj[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })

##Bison Gulch
Tarp_img_path = "F:/ORNL_DAAC_DATA_ARCHIVE/BisonGulch/BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or"
Tarp_img = brick(Tarp_img_path)
tst_crs<-crs(Tarp_img)
Tarp_pft_path = "C:/Users/Nelson Lab/Desktop/lecospec_figs/BisonGulchTarp.shp"
Tarp_pft_vec<-readOGR(dsn=Tarp_pft_path)
Tarp_pft_vec_sf<-sf::st_as_sf(Tarp_pft_vec)
Tarp_pft_vec_sf_proj<-sf::st_transform(Tarp_pft_vec_sf, tst_crs)

lapply(1:3,  
       function(x) {
         tst_img <- brick(Tarp_img_path)
         tst_quads<-Tarp_pft_vec_sf_proj[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         metadata(tst_mask)<-as.list(Tarp_pft_vec_sf_proj[x,]$CLASS_NAME)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/WhiteRefs/Tarp/", Tarp_pft_vec_sf_proj[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })



##TwelveMile

Tarp_img_path = "F:/ORNL_DAAC_DATA_ARCHIVE/TwelveMile/TwelveMile_2019_08_09_21_28_52_0_rd_rf_or"
Tarp_img = brick(Tarp_img_path)
tst_crs<-crs(Tarp_img)
Tarp_pft_path = "C:/Users/Nelson Lab/Desktop/lecospec_figs/TwelveMileTarp.shp"
Tarp_pft_vec<-readOGR(dsn=Tarp_pft_path)
Tarp_pft_vec_sf<-sf::st_as_sf(Tarp_pft_vec)
Tarp_pft_vec_sf_proj<-sf::st_transform(Tarp_pft_vec_sf, tst_crs)

lapply(1:3,  
       function(x) {
         tst_img <- brick(Tarp_img_path)
         tst_quads<-Tarp_pft_vec_sf_proj[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         metadata(tst_mask)<-as.list(Tarp_pft_vec_sf_proj[x,]$CLASS_NAME)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/WhiteRefs/Tarp/", Tarp_pft_vec_sf_proj[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })

##Chatanika

Tarp_img_path = "F:/ORNL_DAAC_DATA_ARCHIVE/Chatnika/Chatnika_2018_07_29_20_32_59_0_rd_rf_or"
Tarp_img = brick(Tarp_img_path)
tst_crs<-crs(Tarp_img)
Tarp_pft_path = "C:/Users/Nelson Lab/Desktop/lecospec_figs/ChatanikaTarp.shp"
Tarp_pft_vec<-readOGR(dsn=Tarp_pft_path)
Tarp_pft_vec_sf<-sf::st_as_sf(Tarp_pft_vec)
Tarp_pft_vec_sf_proj<-sf::st_transform(Tarp_pft_vec_sf, tst_crs)

lapply(1:3,  
       function(x) {
         tst_img <- brick(Tarp_img_path)
         tst_quads<-Tarp_pft_vec_sf_proj[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         metadata(tst_mask)<-as.list(Tarp_pft_vec_sf_proj[x,]$CLASS_NAME)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/WhiteRefs/Tarp/", Tarp_pft_vec_sf_proj[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })

##EightMile

Tarp_img_path = "F:/ORNL_DAAC_DATA_ARCHIVE/EightMile/EightMile_2018_07_28_22_56_17_5968_rd_rf_or"
Tarp_img = brick(Tarp_img_path)
tst_crs<-crs(Tarp_img)
Tarp_pft_path = "C:/Users/Nelson Lab/Desktop/lecospec_figs/EightMileTarp.shp"
Tarp_pft_vec<-readOGR(dsn=Tarp_pft_path)
Tarp_pft_vec_sf<-sf::st_as_sf(Tarp_pft_vec)
Tarp_pft_vec_sf_proj<-sf::st_transform(Tarp_pft_vec_sf, tst_crs)

lapply(1:3,  
       function(x) {
         tst_img <- brick(Tarp_img_path)
         tst_quads<-Tarp_pft_vec_sf_proj[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         metadata(tst_mask)<-as.list(Tarp_pft_vec_sf_proj[x,]$CLASS_NAME)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/WhiteRefs/Tarp/", Tarp_pft_vec_sf_proj[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })


##Murphy Dome

Tarp_img_path = "F:/ORNL_DAAC_DATA_ARCHIVE/MurphyDome/MurphyDome_2018_07_31_19_47_11_10350_rd_rf_or"
Tarp_img = brick(Tarp_img_path)
tst_crs<-crs(Tarp_img)
Tarp_pft_path = "C:/Users/Nelson Lab/Desktop/lecospec_figs/MurphyDomeTarp.shp"
Tarp_pft_vec<-readOGR(dsn=Tarp_pft_path)
Tarp_pft_vec_sf<-sf::st_as_sf(Tarp_pft_vec)
Tarp_pft_vec_sf_proj<-sf::st_transform(Tarp_pft_vec_sf, tst_crs)

lapply(1:3,  
       function(x) {
         tst_img <- brick(Tarp_img_path)
         tst_quads<-Tarp_pft_vec_sf_proj[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         metadata(tst_mask)<-as.list(Tarp_pft_vec_sf_proj[x,]$CLASS_NAME)
         # tst_out<-c(tst_crop,tst_mask)
         writeRaster(tst_mask, paste("./Data/Ground_Validation/WhiteRefs/Tarp/", Tarp_pft_vec_sf_proj[x,]$CLASS_NAME, sep=""), format = "ENVI", overwrite = TRUE)
         #return(tst_mask)
       })
