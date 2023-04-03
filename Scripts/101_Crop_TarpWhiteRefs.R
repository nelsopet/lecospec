source("Functions/lecospectR.R")
require(terra)
require(raster)
require(rgdal)

Tarp_img_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/raw_6425_rd_rf_55pctWhiteRef_or"
tst_img = brick(Tarp_img_path)
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
Tarp_img_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/raw_1511_rd_rf_55pctWhiteRef_or"
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

Tarp_img_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/raw_0_rd_rf_55pctWhiteRef_or"
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

Tarp_img_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/raw_0_rd_rf_56pctWhiteRef_or"
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

Tarp_img_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/raw_5968_rd_rf_55pctWhiteRef_or"
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

Tarp_img_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73118/ImagingSpectrometer/DataFolders/100158_MurphyDomeFlight1_2018_07_31_19_47_11/raw_10350_rd_rf_55pctWhiteRef_or"
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
