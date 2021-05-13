#Segment
require(raster)
require(spectrolab)
require(tidyverse)
require(hsdar)
require(sf)
require(mapview)
require(caTools)
require(terra)
require(tools)
require(OpenImageR)
require(tiff)
require(itcSegment)
require(rgdal)

#View images
path="Data/SubsetDatacube"
tst<-brick(path)
tst_rgb<-tst[[c(160,80,25)]]
plotRGB(tst, r=160, g=80, b=25, stretch="lin")

##Project 
projection(tst)
#[1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#tst_proj<-projectRaster(tst, crs=new_prj)
#tst_proj_crs42310<-projectRaster(tst, crs=crs())
tst_812<-as(tst$Resize..Band.225.raw_5968_rd_rf_or...812.390000.Nanometers.,"RasterLayer")

#tst_seg_26983_winSize3_Dist50   <-itcIMG(tst_812, epsg = 3467, searchWinSize = 3, DIST = 50) #Works with error
plot(tst_seg_26983_winSize3_Dist50)
#tst_seg_26983_winSize9_Dist50   <-itcIMG(tst_812, epsg = 3467, searchWinSize = 9, DIST = 50) #Works with error
#tst_seg_26983_winSize21_Dist50  <-itcIMG(tst_812, epsg = 3467, searchWinSize = 21, DIST = 50) #Works with error
tst_seg_26983_winSize51_Dist500 <-itcIMG(tst_812, epsg = 3467, searchWinSize = 51, DIST = 500) #Works with error
plot(tst_seg_26983_winSize51_Dist500)
