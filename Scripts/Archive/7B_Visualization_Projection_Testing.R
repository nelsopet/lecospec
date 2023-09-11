new_proj = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
new_proj_crs= crs(new_proj)

tst_proj=projectRaster(BisonGulchQuads_FncGrp2_out, crs = new_proj, method = 'ngb')

maprint<-leaflet(tst_proj) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(minZoom = 0.1, maxZoom = 1000)) %>%
  # leaflet::addRasterImage(BisonGulchQuadsRGB_hires) %>%
  leaflet::addRasterImage(tst_proj, 
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
#addMarkers(y)
#mapshot(maprint, file="Output/Prediction/Genera/BisonGulchQuads2_FncGrp2_map.jpeg")
saveWidget(maprint, file="Output/Prediction/V2/FncGrp2/BisonGulchQuads.envi/BisonGulchQuads2_FncGrp2_map.html")
