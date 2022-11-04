#Load packages
require(sf)
library(rgdal)
library(tidyverse)
library(sf)
require(terra)
require(maptiles)


#Custom function to get each KML
allKmlLayers <- function(kmlfile){
  lyr <- ogrListLayers(kmlfile)
  mykml <- list()
  for (i in 1:length(lyr)) {
    mykml[i] <- readOGR(kmlfile,lyr[i])
  }
  names(mykml) <- lyr
  return(mykml)
}


#Read in metadata of reflectanc spectra taken as a part of NASA ABoVE activities
Speclib_metadata<-read_csv("./Output/C_000_Speclib_raw_metadata.csv")
UAS_path<-"./assets/UAV_VNIR_AK_2018_2019.kml"
UAS_extents<-allKmlLayers(UAS_path)
UAS_all<-Reduce(raster::union, UAS_extents)
UAS_all_centroids<-centroids(vect(UAS_all)) 
writeVector(UAS_all_centroids, "./Output/UAV_VNIR_AK_centroids.kml") #", filetype = "ESRI Shapefile", overwrite= TRUE)
readOGR("./Output/UAV_VNIR_AK_centroids.kml") 

#Create a spatial object to plot locations where ground spectral measurements were made
SpecLib_LatLong<-Speclib_metadata %>% dplyr::select(Latitude, Longitude) %>% unique() %>% filter(Latitude!="n/a")

# Convert data frame to sf object
SpecLib_LatLong_point <- st_as_sf(x = SpecLib_LatLong, 
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


# Plot both UAV missions and SpecLib locations
bg <- get_tiles(ext(SpecLib_LatLong_point))
plotRGB(bg)
points(UAS_all_centroids, col="blue", lwd=3)
lines(SpecLib_LatLong, col="red", lwd=3)
