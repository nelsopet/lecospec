##Lets test the tiling function
install.packages("rgdal")
install.packages("raster")
install.packages("GSIF")
install.packages("tmap")
library(rgdal)
library(gdalUtils)
library(raster)
library(GSIF)
library(tmap)
library(parallel)

setwd("Original_data/Sensors/Tiles/")
# Gets spatial info for datacube
# The datacube is about 11 by 13 pixels
Datacube<-GDALinfo("Original_data/Sensors/Tiles/EightMile_TSTIMG2_envi_R1C1.dat")

# Tiles datacube into 90 cm blocks

tile.lst <- getSpatialTiles(Datacube, block.x=.000009, return.SpatialPolygons=TRUE)

tile.tbl <- getSpatialTiles(Datacube, block.x=.000009, return.SpatialPolygons=FALSE)

# Adds a unique ID to each Tile
tile.tbl$ID <- as.character(1:nrow(tile.tbl))
# head(tile.tbl)

# Reads in the first tile from the 2 tiles created 

m1 <- readGDAL("Original_data/Sensors/Sample_clips/Sub1", offset=unlist(tile.tbl[1,c("offset.y","offset.x")]),
              region.dim=unlist(tile.tbl[1,c("region.dim.y","region.dim.x")]),
              output.dim=unlist(tile.tbl[1,c("region.dim.y","region.dim.x")]),
              silent = TRUE)

m2 <- readGDAL("Original_data/Sensors/Sample_clips/Sub1", offset=unlist(tile.tbl[2,c("offset.y","offset.x")]),
               region.dim=unlist(tile.tbl[2,c("region.dim.y","region.dim.x")]),
               output.dim=unlist(tile.tbl[2,c("region.dim.y","region.dim.x")]),
               silent = TRUE)

# Converts spaitial points dataframe to a dataframe 
b1<-as.data.frame(m1)
b2<-as.data.frame(m2)

# Changes column names 
names(b1)[327:328]<-c("x","y")
names(b2)[327:328]<-c("x","y")

# Reorders column names
b1<-dplyr::select(b1,x,y,everything())
b2<-dplyr::select(b2,x,y,everything())

# Converts dataframe to a bricked raster, functions can be applied to this brick
c1<-rasterFromXYZ(as.data.frame(b1))
c2<-rasterFromXYZ(as.data.frame(b2))
         
# Creates output file as a Geotiff
out.tif = paste0("OutputsIMG/Processing/Tiles", "/T_", tile.tbl[1,"ID"], ".tif")
writeRaster(c1, out.tif,options=c('TFW=YES'))

out.tif = paste0("OutputsIMG/Processing/Tiles", "/T_", tile.tbl[2,"ID"], ".tif")
writeRaster(c2, out.tif,options=c('TFW=YES'))


# From the list of files we can build a mosaic using GDAL and save it to disk 
t.lst <- list.files("OutputsIMG/Processing/Tiles", pattern=".tif$", full.names=TRUE)

t.st2<-lapply(t.lst,brick)
finalraster<-do.call(merge,t.st2)


setwd("OutputsIMG/Processing/Tiles")

gdalbuildvrt(gdalfile = "*.tif",
             output.vrt = "testTileCombo.vrt")




brick("OutputsIMG/Processing/Test_tileCombine.tif")

