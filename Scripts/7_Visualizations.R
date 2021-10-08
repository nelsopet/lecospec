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

#list.files("./Output/Prediction/")

source("./Functions/PFT_mapper.R")

SpecLib_derivs<-read.csv("./Output/D_002_SpecLib_Derivs.csv")

#Fnc Grp 2 colors
coarse_colors = createPalette(length(unique(SpecLib_derivs$Functional_group2)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(SpecLib_derivs$Functional_group2)) %>%
  mutate(ColorNum = seq(1:length(unique(SpecLib_derivs$Functional_group2))));

coarse_color_list<-SpecLib_derivs %>% dplyr::select(Functional_group2) %>% inner_join(coarse_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)







##Plot data cubes predicted at the species level.
unique(SpecLib_derivs$Species_name)
species_colors = createPalette(length(unique(SpecLib_derivs$Species_name)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(SpecLib_derivs$Species_name)) %>%
  mutate(ColorNum = seq(1:length(unique(SpecLib_derivs$Species_name))));
species_color_list<-SpecLib_derivs %>% dplyr::select(Species_name) %>% inner_join(species_colors, by=c("Species_name"="FNC_grp1"), keep=FALSE)

##Reclass class raster based on histogram of most common targets and creat an "other"
##class to reduce the number of elements the legend that are trace or not present


##Basic species and genus maps
AK_sp_map<- function(map) {leaflet() %>%
    leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
             options = providerTileOptions(minZoom = 8, maxZoom = 100)) %>%
    leaflet::addRasterImage(map, layerId = "layer", colors = species_colors$Color) %>%
    leaflet::addLegend("bottomleft", colors = species_colors$Color, labels = species_colors$FNC_grp1,opacity = 1) %>%
    addOpacitySlider(layerId = "layer")
    #setView(zoom = 10)
    }


AK_genus_map<- function(map) {leaflet() %>%
    leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
             options = providerTileOptions(minZoom = 8, maxZoom = 100)) %>%
    leaflet::addRasterImage(map, layerId = "layer", colors = genera_colors$Color) %>%
    leaflet::addLegend("bottomleft", colors = genera_colors$Color, labels = genera_colors$FNC_grp1, opacity = 0) %>%
    addOpacitySlider(layerId = "layer")}

AK_coarse_map<- function(map) {leaflet(map) %>%
    leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                      options = providerTileOptions(minZoom = 8, maxZoom = 100)) %>%
    leaflet::addRasterImage(map, layerId = "layer", colors = coarse_colors$Color) %>%
    leaflet::addLegend("bottomleft", colors = coarse_colors$Color, labels = coarse_colors$FNC_grp1, opacity = 0) %>%
    addOpacitySlider(layerId = "layer")}


#Read in TIF and predicted output
TwelveMileRGB_hires<-raster("F:/TwelveMile/_12mile_tif_Area0.dat")
TwelveMileTestOut_FncGrp2<-raster("Output/Prediction/Genera/TwelveMileTestOut_FncGrp2.tif")
TwelveMileQuads<-read_sf("F:/TwelveMile/TwelveMileQ0_40_70m.shp")
Mile12Quads<-read_csv("F:/TwelveMile/TwelveMileQ0_40_70m.csv")

maprint<-AK_coarse_map(TwelveMileTestOut_FncGrp2)
maprint<-leaflet(TwelveMileTestOut_FncGrp2) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(minZoom = 3, maxZoom = 100)) %>%
 # leaflet::addRasterImage(TwelveMileRGB_hires) %>%
  leaflet::addRasterImage(TwelveMileTestOut_FncGrp2, layerId = "layer", colors = coarse_colors$Color) %>%
  leaflet::addLegend("bottomleft", colors = coarse_colors$Color, labels = coarse_colors$FNC_grp1) %>% #, opacity = 0) %>%
  addOpacitySlider(layerId = "layer") %>%
  addMarkers(y)
#mapshot(maprint, file="Output/Prediction/Genera/TwelveMile2_FncGrp2_map.jpeg")
saveWidget(maprint, file="Output/Prediction/Genera/TwelveMile2_FncGrp2_map.html")

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)

WickerTestOut<-raster("Output/Prediction/Species/WickerTestOut.tif")

Wicker_map<-AK_sp_map(WickerTestOut)
mapshot(Wicker_map,file="Output/Prediction/WickerTestOut.jpeg")

LittleLakeTestOut<-raster("Output/Prediction/Species/LittleLakeTestOut.tif")
LittleLake_map<-AK_sp_map(LittleLakeTestOut)
mapshot(LittleLake_map,file="Output/Prediction/LittleLakeTestOut.jpeg")


BisonTestOut<-raster("Output/Prediction/Species/BisonTestOut.tif")
Bison_map<-AK_sp_map(BisonTestOut)
mapshot(Bison_map,file="Output/Prediction/BisonTestOut.jpeg")


Murph1TestOut<-raster("Output/Prediction/Species/Murph1TestOut.tif")
Murph1TestOut_map<-AK_sp_map(Murph1TestOut)
saveWidget(Murph1TestOut_map, file="Output/Prediction/Species/Murph1TestOut_map.html")
mapshot(Bison_map,file="Output/Prediction/Murph1TestOut.jpeg")

ChatanikaTestOut<-raster("Output/Prediction/Species/ChatanikaTestOut.tif")
ChatanikaTestOut_map<-AK_sp_map(ChatanikaTestOut)
mapshot(ChatanikaTestOut_map,file="Output/Prediction/ChatanikaTestOut.jpeg")

EagleTestOut<-raster("Output/Prediction/Species/EagleTestOut.tif")
EagleTestOut_map<-AK_sp_map(EagleTestOut)
mapshot(EagleTestOut_map,file="Output/Prediction/EagleTestOut.jpeg")

TwelveMileTestOut<-raster("Output/Prediction/TwelveMileTestOut.tif")
TwelveMile_map<-AK_sp_map(TwelveMileTestOut)

#Read in vector layer
TwelveMile_quads<-sf::read_sf("/Users/peternelson 1/Documents/Schoodic/Grants/Field Museum/data/12mile_shapefiles/12_mile_shapefile_1.shp")
TwelveMile_quads_2<-sf::read_sf("/Users/peternelson 1/Documents/Schoodic/Grants/Field Museum/data/12mile_shapefiles/12_mile_shapefile_2.shp")

leaflet::addPolygons(data =TwelveMile_quads_2$geometry)

saveWidget(TwelveMile_map, file="Output/Prediction/Species/TwelveMile_map.html")
jpeg("Output/Prediction/Species/TwelveMile_map_hist.jpeg")
raster::hist(TwelveMileTestOut$layer, breaks = c(0:108),labels = species_colors$FNC_grp1)
dev.off()
#mapshot(TwelveMile_map,file="Output/Prediction/TwelveMileTestOut.jpeg")


#pdf("./Output/Prediction/WickerTestOut_ggplot.pdf") #, width= 10, height =20)
#PFT_Mapper("./Output/Prediction/WickerTestOut.tif")
#dev.off()
#
#pdf("./Output/Prediction/LittleLakeTestOut_ggplot.pdf") #, width= 10, height =20)
#PFT_Mapper("./Output/Prediction/LittleLakeTestOut.tif")
#dev.off()
#
#pdf("./Output/Prediction/BisonTestOut_ggplot.pdf") #, width= 10, height =20)
#PFT_Mapper("./Output/Prediction/BisonTestOut.tif")
#dev.off()
#
#pdf("./Output/Prediction/Murph1TestOut_ggplot.pdf") #, width= 10, height =20)
#PFT_Mapper("./Output/Prediction/Murph1TestOut.tif")
#dev.off()
#
#pdf("./Output/Prediction/ChatanikaTestOut_ggplot.pdf") #, width= 10, height =20)
#PFT_Mapper("./Output/Prediction/ChatanikaTestOut.tif")
#dev.off()
#
#pdf("./Output/Prediction/EagleTestOut_ggplot.pdf") #, width= 10, height =20)
#PFT_Mapper("./Output/Prediction/EagleTestOut.tif")
#dev.off()
#
#pdf("./Output/Prediction/TwelveMileTestOut_ggplot.pdf") #, width= 10, height =20)
#PFT_Mapper("./Output/Prediction/TwelveMileTestOut.tif")
#dev.off()

####################
##Plot data cubes predicted at the genera level.
unique(SpecLib_derivs$Functional_group1)
genera_colors = createPalette(length(unique(SpecLib_derivs$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(SpecLib_derivs$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(SpecLib_derivs$Functional_group1))));

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
genera_color_list<-SpecLib_derivs %>% dplyr::select(Functional_group1) %>% inner_join(genera_colors, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)

#TwelveMile_Genera_map<-mapview(TwelveMileTestOut_Genera, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=genera_colors$Color)
#mapshot(TwelveMile_Genera_map,file="Output/Prediction/Genera/TwelveMileTestOut_Genera.jpeg")

#pdf("./Output/Prediction/Genera/TwelveMileTestOut_Genera_ggplot.pdf") #, width= 10, height =20)
#Genera_Mapper("./Output/Prediction/Genera/TwelveMileTestOut_Genera.tif")
#dev.off()
TwelveMileTestOut2<-raster("Output/Prediction/Genera/TwelveMileTestOut2_Genera.tif")
#list.files("Output/Prediction/Genera/")
TwelveMile2_Genera_map<-AK_genus_map(TwelveMileTestOut2)
saveWidget(TwelveMile2_Genera_map, file="Output/Prediction/Genera/TwelveMile2_Genera_map.html")

jpeg("Output/Prediction/Genera/TwelveMile2_Genera_map.jpeg")

maprint<-leaflet(TwelveMileTestOut2) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(minZoom = 3, maxZoom = 100)) %>%
  leaflet::addRasterImage(TwelveMileTestOut2, layerId = "layer", colors = genera_colors$Color) %>%
  leaflet::addLegend("bottomleft", colors = genera_colors$Color, labels = genera_colors$FNC_grp1) %>% #, opacity = 0) %>%
  addOpacitySlider(layerId = "layer")
saveWidget(maprint, file="Output/Prediction/Genera/TwelveMile2_Genera_map.html")
mapshot(maprint, file="Output/Prediction/Genera/TwelveMile2_Genera_map.jpeg")


jpeg("Output/Prediction/Genera/TwelveMile2_Genera_map_hist.jpeg")
hist(TwelveMileTestOut2, maxpixels = 1E10, breaks = c(0:36),labels = genera_colors$FNC_grp1)
dev.off()
mapshot(TwelveMile2_Genera_map,file= "Output/Prediction/Genera/TwelveMileTestOut2_Genera.jpeg")

#pdf("./Output/Prediction/Genera/TwelveMileTestOut2_Genera_ggplot.pdf") #, width= 10, height =20)
#Genera_Mapper("./Output/Prediction/Genera/TwelveMileTestOut2_Genera.tif")
#dev.off()



