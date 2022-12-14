require(Polychrome)
require(gplots)
require(mapview)
require(tidyverse)
require(rasterVis)
require(leaflet.opacity)
require(leaflegend)
require(vegan)
require(leaflet)
require(sf)
require(htmlwidgets)

source("Functions/lecospectR.R")

sf::st_read(system.file(dsn="./Data/Ground_Validation/Imagery/kml/BisonGulch.kml", package = "sf"))
list.files("./Data/Ground_Validation/Imagery/kml")

output<-raster("M://Output/bg_outputs_m.grd")
output_df<-rasterToPoints(output) %>% as.data.frame() %>% rename(z = layer)
output_df_string<-convert_pft_codes(output_df, 1, to="string")
output_df_fin<-raster::rasterFromXYZ(output_df_string, digits=4)
plot(output_df_string)
help("rast")


pal <- colorNumeric("OrRd", output_df_string$z)


maprint<-leaflet(output) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(minZoom = 0.1, maxZoom = 1000)) %>%
  # leaflet::addRasterImage(BisonGulchQuadsRGB_hires) %>%
  leaflet::addRasterImage(output, 
                          layerId = "Plant Functional Type", 
                          colors = coarse_colors$Color,
                          project = TRUE
                          #colors = coarse_pal2
  ) %>%
  leaflet::addLegend("bottomleft", 
                     colors = coarse_colors$Color, 
                     #colors = coarse_pal2, 
                     #pal = coarse_pal2,
                     #values = tst_proj, 
                     labels = coarse_colors$FNC_grp1, 
                     opacity = 1 ) %>%
  addOpacitySlider(layerId = "Plant Functional Type") #%>%

#visualize_prediction <- function(filepath, key_file, column){
#  require(leaflet)
#  color_map <- create_color_map(key_file, column)
#  labels <- create_labels(key_file, column)
#  layer <- raster::raster(filepath)
#  epsg_code <- 3857
#  layer_projected <- project_to_epsg(layer, epsg_code, categorical_raster = TRUE)
#  map <- leaflet::leaflet() %>%
#    leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
#                      options = providerTileOptions(minZoom = 8, maxZoom = 100)) %>%
#    leaflet::addRasterImage(layer, layerId = "layer", colors = color_map) %>%
#    leaflet::addLegend("bottomleft", colors = color_map(labels), labels = labels, opacity = 1) %>%
#    leaflet.opacity::addOpacitySlider(layerId = "layer")
#  return(map)
#}
