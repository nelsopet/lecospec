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

list.files("./Output/Prediction/")

source("./Functions/PFT_mapper.R")

SpecLib_derivs<-read.csv("./Output/D_002_SpecLib_Derivs.csv")


##Plot data cubes predicted at the species level.
unique(SpecLib_derivs$Species_name)
species_colors = createPalette(length(unique(SpecLib_derivs$Species_name)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(SpecLib_derivs$Species_name)) %>%
  mutate(ColorNum = seq(1:length(unique(SpecLib_derivs$Species_name))));

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

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
species_color_list<-SpecLib_derivs %>% dplyr::select(Species_name) %>% inner_join(species_colors, by=c("Species_name"="FNC_grp1"), keep=FALSE)

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




##Cluster of all functional groups
#Veg<-Cleaned_Speclib %>%  select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)
Veg<-SpecLib_derivs %>% dplyr::select(-ScanID:-Functional_group2_Freq, -X397.593_5nm:-X897.593_5nm)

#Veg_env %>% group_by(Species_name, Functional_group1) %>% tally %>% View()

#Veg_vnir_names<-colnames(Veg) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)

#Veg_vnir<-Veg[1:651]

#Veg_env<-Cleaned_Speclib %>%  select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)
Veg_env<-SpecLib_derivs %>%  dplyr::select(ScanID:Functional_group2_Freq)

#fnc_grp1_colors = createPalette(length(unique(Veg_env$Functional_group2)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
#  as.data.frame() %>%
#  dplyr::rename(Color = ".") %>%
#  mutate(FNC_grp1 = unique(Veg_env$Functional_group2)) %>%
#  mutate(ColorNum = seq(1:length(unique(Veg_env$Functional_group2))));
fnc_grp1_colors = createPalette(length(unique(Veg_env$Code_name)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Veg_env$Code_name)) %>%
  mutate(ColorNum = seq(1:length(unique(Veg_env$Code_name))));

fnc_grp2_colors = createPalette(length(unique(Veg_env$Functional_group2)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Veg_env$Functional_group2)) %>%
  mutate(ColorNum = seq(1:length(unique(Veg_env$Functional_group2))));

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
fnc_grp1_color_list<-Veg_env %>% dplyr::select(Code_name) %>% inner_join(fnc_grp1_colors, by=c("Code_name"="FNC_grp1"), keep=FALSE)
fnc_grp2_color_list<-Veg_env %>% dplyr::select(Functional_group2) %>% inner_join(fnc_grp2_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)





Veg<-Veg %>% replace(is.na(.),0)
dist(Veg) %>% hist()
#hclust(as.matrix(Veg)) %>% as.dendrogram()

pdf("Output/F_Heatmap_Veg_Derivs.pdf", height = 12, width = 20)
#heatmap(as.matrix(Veg), Colv = NULL, RowSideColors = fnc_grp1_color_list$Color)
#heatmap(as.matrix(Veg), Colv = NULL, RowSideColors = fnc_grp2_color_list$Color)

#heatmap.2(as.matrix(decostand(Veg, "normalize", MARGIN = 2)), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp1_color_list$Color)
#Looks best but needs to rescale color ramp
heatmap.2(as.matrix(decostand(Veg, "standardize", MARGIN = 2), breaks = c(seq(-2, 2, by = 0.1)), dendrogram="both", trace="none", Colv = TRUE, RowSideColors = fnc_grp2_color_list$Color))

#heatmap.2(as.matrix(decostand(Veg, "normalilze", MARGIN = 2)),  breaks = c(-1,-0.5,seq(-0.01, -0.25, by = -0.01), 0, seq(0.01, 0.25, by = 0.01), 0.5,1),dendrogram="both", trace="none", Colv = TRUE, RowSideColors = fnc_grp2_color_list$Color)
#heatmap.2(as.matrix(Veg),scale="row", dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp2_color_list$Color)

legend(x="topright", legend=unique(fnc_grp2_color_list$Functional_group2), fill=unique(fnc_grp2_color_list$Color))
dev.off()

##Separate clusters for each high level functional group
Lichen<-Cleaned_Speclib %>% filter(Functional_group2=="Lichen") %>%  select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)

Lichen_env %>% group_by(Species_name, Functional_group1) %>% tally %>% View()

Lichen_vnir_names<-colnames(Lichen) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)

Lichen_vnir<-Lichen[1:651]

Lichen_env<-Cleaned_Speclib %>% filter(Functional_group2=="Lichen") %>%  select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)

fnc_grp1_colors = createPalette(length(unique(Lichen_env$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
as.data.frame() %>%
dplyr::rename(Color = ".") %>%
mutate(FNC_grp1 = unique(Lichen_env$Functional_group1)) %>%
mutate(ColorNum = seq(1:length(unique(Lichen_env$Functional_group1))));



fnc_grp1_color_list<-Lichen_env %>% select(Functional_group1) %>% inner_join(fnc_grp1_colors, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)

Lichen<-Lichen %>% replace(is.na(.),0)
dist(Lichen) %>% hist()
#hclust(as.matrix(Lichen)) %>% as.dendrogram()

pdf("Output/F_Heatmap_lichens.pdf", height = 12, width = 20)
#heatmap(as.matrix(Lichen), Colv = NULL, RowSideColors = fnc_grp1_color_list$Color)
heatmap.2(as.matrix(Lichen[,Lichen_vnir_names]), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp1_color_list$Color)

legend(x="topright", legend=unique(fnc_grp1_color_list$Functional_group1), fill=unique(fnc_grp1_color_list$Color))
dev.off()

################# Lichen yellow
unique(Cleaned_Speclib$Functional_group1)
Lichen_Yellow<-Cleaned_Speclib %>% filter(Functional_group1=="Lichen_Fruticose_Yellow") %>%  select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)

Lichen_Yellow_vnir_names<-colnames(Lichen_Yellow) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)

Lichen_Yellow_vnir<-Lichen_Yellow[1:651]

Lichen_Yellow_env<-Cleaned_Speclib %>% filter(Functional_group1=="Lichen_Fruticose_Yellow") %>%  select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)

fnc_grp1_colors = createPalette(length(unique(Lichen_Yellow_env$Species_name)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Lichen_Yellow_env$Species_name)) %>%
  mutate(ColorNum = seq(1:length(unique(Lichen_Yellow_env$Species_name))));



fnc_grp1_color_list<-Lichen_Yellow_env %>% select(Species_name) %>% inner_join(fnc_grp1_colors, by=c("Species_name"="FNC_grp1"), keep=FALSE)

Lichen_Yellow<-Lichen_Yellow %>% replace(is.na(.),0)
dist(Lichen_Yellow) %>% hist()
#hclust(as.matrix(Lichen_Yellow)) %>% as.dendrogram()
dev.off()

pdf("Output/F_Heatmap_Lichen_Yellow.pdf", height = 12, width = 20)
#heatmap(as.matrix(Lichen_Yellow), Colv = NULL, RowSideColors = fnc_grp1_color_list$Color)
heatmap.2(as.matrix(Lichen_Yellow[,Lichen_Yellow_vnir_names]), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp1_color_list$Color)
legend(x="topright", legend=unique(fnc_grp1_color_list$Species_name), fill=unique(fnc_grp1_color_list$Color))
dev.off()

################# Bryophytes
unique(Cleaned_Speclib$Functional_group2)
Moss<-Cleaned_Speclib %>% filter(Functional_group2=="Moss") %>%  select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)

Moss_vnir_names<-colnames(Moss) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)

Moss_vnir<-Moss[1:651]

Moss_env<-Cleaned_Speclib %>% filter(Functional_group2=="Moss") %>%  select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)

fnc_grp1_colors = createPalette(length(unique(Moss_env$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Moss_env$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(Moss_env$Functional_group1))));



fnc_grp1_color_list<-Moss_env %>% select(Functional_group1) %>% inner_join(fnc_grp1_colors, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)

Moss<-Moss %>% replace(is.na(.),0)
dist(Moss) %>% hist()
#hclust(as.matrix(Moss)) %>% as.dendrogram()
dev.off()

pdf("Output/F_Heatmap_Moss.pdf", height = 12, width = 20)
#heatmap(as.matrix(Moss), Colv = NULL, RowSideColors = fnc_grp1_color_list$Color)
heatmap.2(as.matrix(Moss[,Moss_vnir_names]), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp1_color_list$Color)

legend(x="topright", legend=unique(fnc_grp1_color_list$Functional_group1), fill=unique(fnc_grp1_color_list$Color))
dev.off()

################# Shrub
unique(Cleaned_Speclib$Functional_group2)
Shrub<-Cleaned_Speclib %>% filter(Functional_group2=="Shrub") %>%  dplyr::select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)
Shrub_derivs<- SpecLib_derivs %>% filter(Functional_group2=="Shrub") %>%  dplyr::select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)


Shrub_vnir_names<-colnames(Shrub) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)
Shrub_deriv_names<-colnames(Shrub_derivs) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)

Shrub_vnir<-Shrub[1:651]

Shrub_env<-Cleaned_Speclib %>% filter(Functional_group2=="Shrub") %>%  dplyr::select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)
Shrub_deriv_env<-SpecLib_derivs %>% filter(Functional_group2=="Shrub") %>%  dplyr::select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)

fnc_grp1_colors = createPalette(length(unique(Shrub_env$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Shrub_env$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(Shrub_env$Functional_group1))));



fnc_grp1_color_list<-Shrub_env %>% dplyr::select(Functional_group1) %>% inner_join(fnc_grp1_colors, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)

Shrub<-Shrub %>% replace(is.na(.),0)
Shrub_derivs<-Shrub_derivs %>% replace(is.na(.),0)

dist(Shrub) %>% hist()
#hclust(as.matrix(Shrub)) %>% as.dendrogram()
dev.off()

pdf("Output/F_Heatmap_Shrub.pdf", height = 12, width = 20)
#heatmap(as.matrix(Shrub), Colv = NULL, RowSideColors = fnc_grp1_color_list$Color)
heatmap.2(as.matrix(Shrub[,Shrub_vnir_names]), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp1_color_list$Color)
#heatmap.2(as.matrix(Shrub_derivs[,Shrub_deriv_names]), dendrogram="row", trace="none", Colv = FALSE)
#heatmap.2(as.matrix(Shrub_derivs[,Shrub_deriv_names]), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp2_color_list$Color)

legend(x="topright", legend=unique(fnc_grp1_color_list$Functional_group1), fill=unique(fnc_grp1_color_list$Color))
dev.off()


################# Shrub Salix
unique(Cleaned_Speclib$Functional_group1)
Shrub<-Cleaned_Speclib %>% filter(Functional_group1=="Shrub_Salix") %>%  select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)

Shrub_vnir_names<-colnames(Shrub) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)

Shrub_vnir<-Shrub[1:651]

Shrub_env<-Cleaned_Speclib %>% filter(Functional_group1=="Shrub_Salix") %>%  select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)

fnc_grp1_colors = createPalette(length(unique(Shrub_env$Species_name)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Shrub_env$Species_name)) %>%
  mutate(ColorNum = seq(1:length(unique(Shrub_env$Species_name))));



fnc_grp1_color_list<-Shrub_env %>% select(Species_name) %>% inner_join(fnc_grp1_colors, by=c("Species_name"="FNC_grp1"), keep=FALSE)

Shrub<-Shrub %>% replace(is.na(.),0)
dist(Shrub) %>% hist()
#hclust(as.matrix(Shrub)) %>% as.dendrogram()
dev.off()

pdf("Output/F_Heatmap_Shrub_Salix.pdf", height = 12, width = 20)
#heatmap(as.matrix(Shrub), Colv = NULL, RowSideColors = fnc_grp1_color_list$Color)
heatmap.2(as.matrix(Shrub[,Shrub_vnir_names]), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp1_color_list$Color)
legend(x="topright", legend=unique(fnc_grp1_color_list$Species_name), fill=unique(fnc_grp1_color_list$Color))
dev.off()
