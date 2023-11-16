#Load packages
require(sf)
library(rgdal)
library(tidyverse)
library(sf)
require(terra)
require(maptiles)
require(stars)


#Custom function to get each KML
#allKmlLayers <- function(kmlfile){
#  lyr <- ogrListLayers(kmlfile)
#  mykml <- list()
#  for (i in 1:length(lyr)) {
#    mykml[i] <- readOGR(kmlfile,lyr[i])
#  }
#  names(mykml) <- lyr
#  return(mykml)
#}
allVectLayers <- function(vectr){
  lyr <- ogrListLayers(vectr)
  myvectr <- list()
  for (i in 1:length(lyr)) {
    myvectr[i] <- vect(vectr,lyr[i])
  }
  names(myvectr) <- lyr
  return(myvectr)
}

#Read in metadata of reflectanc spectra taken as a part of NASA ABoVE activities
Speclib_metadata<-read_csv("./Output/C_000_Speclib_raw_metadata.csv")
UAS_path<-"./assets/UAV_VNIR_AK_2018_2019.kml"
UAS_extents<-allVectLayers(UAS_path)
UAS_extents
#This kills R on VScode
UAS_all<-Reduce(terra::union, UAS_extents[1:2])
plot(UAS_all)
UAS_all_centroids<-centroids(vect(UAS_all)) 
writeVector(UAS_all_centroids, "./Output/UAV_VNIR_AK_centroids.kml") #", filetype = "ESRI Shapefile", overwrite= TRUE)
UAS_all_centroids<-readOGR("./assets/UAV_VNIR_AK_centroids.kml") 

#Create a spatial object to plot locations where ground spectral measurements were made
SpecLib_LatLong<-Speclib_metadata %>% 
  dplyr::select(Latitude, Longitude) %>% 
  unique() %>% 
  filter(Latitude!="n/a")

# Convert data frame to sf object
SpecLib_LatLong_point <- st_as_sf(x = SpecLib_LatLong, 
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

st_write(SpecLib_LatLong_point, driver = "KML", "./Output/Ground_Spetra_AK_points.kml")
SpecLib_LatLong_point<-readOGR("./Output/Ground_Spetra_AK_points.kml") 


# Plot both UAV missions and SpecLib locations
jpeg("./Output/StudyAreaGround_Airborne_Spectra_Locs.jpg")
bg <- get_tiles(ext(SpecLib_LatLong_point))
plotRGB(bg)
points(UAS_all_centroids, col="blue", lwd=10)
points(SpecLib_LatLong_point, col="red", lwd=3)
dev.off()
