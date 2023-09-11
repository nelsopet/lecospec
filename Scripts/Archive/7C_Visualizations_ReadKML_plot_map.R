library(rgdal)
library(tidyverse)
library(sf)

allKmlLayers <- function(kmlfile){
  lyr <- ogrListLayers(kmlfile)
  mykml <- list()
  for (i in 1:length(lyr)) {
    mykml[i] <- readOGR(kmlfile,lyr[i])
  }
  names(mykml) <- lyr
  return(mykml)
}
peter_vnir_uas <- allKmlLayers('D:/mmacander/Documents/19-301/spectral_wg/UAV_VNIR_AK_2018_2019.kml')

peter_vnir_uas_sf_poly <- do.call(rbind, peter_vnir_uas) %>%
  st_as_sf()

peter_vnir_uas_sf_points <- peter_vnir_uas_sf_poly %>%
  st_centroid()

mapview(peter_vnir_uas_sf_points)