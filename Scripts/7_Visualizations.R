require(Polychrome)
require(gplots)
require(mapview)
require(tidyverse)
require(rasterVis)

list.files("./Output/Prediction/")

source("./Functions/PFT_mapper.R")

SpecLib_derivs<-read.csv("./Output/D_002_SpecLib_Derivs.csv")

##Plot data cubes predicted at the species level.
unique(SpecLib_derivs$Classes)
species_colors = createPalette(length(unique(SpecLib_derivs$Classes)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(SpecLib_derivs$Classes)) %>%
  mutate(ColorNum = seq(1:length(unique(SpecLib_derivs$Classes))));

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
species_color_list<-SpecLib_derivs %>% dplyr::select(Classes) %>% inner_join(species_colors, by=c("Classes"="FNC_grp1"), keep=FALSE)

Wicker_map<-mapview(WickerTestOut, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=species_colors$Color)
mapshot(Wicker_map,file="Output/Prediction/WickerTestOut.jpeg")

LittleLake_map<-mapview(LittleLakeTestOut, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=species_colors$Color)
mapshot(LittleLake_map,file="Output/Prediction/LittleLakeTestOut.jpeg")

Bison_map<-mapview(BisonTestOut, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=species_colors$Color)
mapshot(Bison_map,file="Output/Prediction/BisonTestOut.jpeg")

Murph1TestOut_map<-mapview(Murph1TestOut, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=species_colors$Color)
mapshot(Bison_map,file="Output/Prediction/Murph1TestOut.jpeg")

ChatanikaTestOut_map<-mapview(ChatanikaTestOut, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=species_colors$Color)
mapshot(ChatanikaTestOut_map,file="Output/Prediction/ChatanikaTestOut.jpeg")

EagleTestOut_map<-mapview(EagleTestOut, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=species_colors$Color)
mapshot(EagleTestOut_map,file="Output/Prediction/EagleTestOut.jpeg")

TwelveMile_map<-mapview(TwelveMileTestOut, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=species_colors$Color)
mapshot(TwelveMile_map,file="Output/Prediction/TwelveMileTestOut.jpeg")


pdf("./Output/Prediction/WickerTestOut_ggplot.pdf") #, width= 10, height =20)
PFT_Mapper("./Output/Prediction/WickerTestOut.tif")
dev.off()

pdf("./Output/Prediction/LittleLakeTestOut_ggplot.pdf") #, width= 10, height =20)
PFT_Mapper("./Output/Prediction/LittleLakeTestOut.tif")
dev.off()

pdf("./Output/Prediction/BisonTestOut_ggplot.pdf") #, width= 10, height =20)
PFT_Mapper("./Output/Prediction/BisonTestOut.tif")
dev.off()

pdf("./Output/Prediction/Murph1TestOut_ggplot.pdf") #, width= 10, height =20)
PFT_Mapper("./Output/Prediction/Murph1TestOut.tif")
dev.off()

pdf("./Output/Prediction/ChatanikaTestOut_ggplot.pdf") #, width= 10, height =20)
PFT_Mapper("./Output/Prediction/ChatanikaTestOut.tif")
dev.off()

pdf("./Output/Prediction/EagleTestOut_ggplot.pdf") #, width= 10, height =20)
PFT_Mapper("./Output/Prediction/EagleTestOut.tif")
dev.off()

pdf("./Output/Prediction/TwelveMileTestOut_ggplot.pdf") #, width= 10, height =20)
PFT_Mapper("./Output/Prediction/TwelveMileTestOut.tif")
dev.off()

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

TwelveMile_Genera_map<-mapview(TwelveMileTestOut_Genera, map.types = 'Esri.WorldImagery',na.color="NA", col.regions=genera_colors$Color)
mapshot(TwelveMile_Genera_map,file="Output/Prediction/Genera/TwelveMileTestOut_Genera.jpeg")

pdf("./Output/Prediction/Genera/TwelveMileTestOut_Genera_ggplot.pdf") #, width= 10, height =20)
PFT_Mapper("./Output/Prediction/Genera/TwelveMileTestOut_Genera.tif")
dev.off()






##Cluster of all functional groups
#Veg<-Cleaned_Speclib %>%  select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)
Veg<-SpecLib_derivs %>% dplyr::select(-Classes)

#Veg_env %>% group_by(Species_name, Functional_group1) %>% tally %>% View()

#Veg_vnir_names<-colnames(Veg) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)

#Veg_vnir<-Veg[1:651]

#Veg_env<-Cleaned_Speclib %>%  select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)
Veg_env<-SpecLib_derivs %>%  dplyr::select(Classes)

#fnc_grp1_colors = createPalette(length(unique(Veg_env$Functional_group2)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
#  as.data.frame() %>%
#  dplyr::rename(Color = ".") %>%
#  mutate(FNC_grp1 = unique(Veg_env$Functional_group2)) %>%
#  mutate(ColorNum = seq(1:length(unique(Veg_env$Functional_group2))));
fnc_grp1_colors = createPalette(length(unique(Veg_env$Classes)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Veg_env$Classes)) %>%
  mutate(ColorNum = seq(1:length(unique(Veg_env$Classes))));



#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
fnc_grp1_color_list<-Veg_env %>% dplyr::select(Classes) %>% inner_join(fnc_grp1_colors, by=c("Classes"="FNC_grp1"), keep=FALSE)

Veg<-Veg %>% replace(is.na(.),0)
dist(Veg) %>% hist()
#hclust(as.matrix(Veg)) %>% as.dendrogram()

pdf("Output/F_Heatmap_Veg_Derivs.pdf", height = 12, width = 20)
#heatmap(as.matrix(Veg), Colv = NULL, RowSideColors = fnc_grp1_color_list$Color)
heatmap.2(as.matrix(Veg), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp1_color_list$Color)

legend(x="topright", legend=unique(fnc_grp1_color_list$Classes), fill=unique(fnc_grp1_color_list$Color))
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
Shrub<-Cleaned_Speclib %>% filter(Functional_group2=="Shrub") %>%  select(  -ScanID,-Code_name,-Functional_group1,-Functional_group2,-Area,-Species_name_Freq,-Functional_group1_Freq,-Functional_group2_Freq,-Species_name)

Shrub_vnir_names<-colnames(Shrub) #%>% as.numeric() %>% as.data.frame() %>% filter(.<1000)

Shrub_vnir<-Shrub[1:651]

Shrub_env<-Cleaned_Speclib %>% filter(Functional_group2=="Shrub") %>%  select(  ScanID,Code_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,Species_name)

fnc_grp1_colors = createPalette(length(unique(Shrub_env$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Shrub_env$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(Shrub_env$Functional_group1))));



fnc_grp1_color_list<-Shrub_env %>% select(Functional_group1) %>% inner_join(fnc_grp1_colors, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)

Shrub<-Shrub %>% replace(is.na(.),0)
dist(Shrub) %>% hist()
#hclust(as.matrix(Shrub)) %>% as.dendrogram()
dev.off()

pdf("Output/F_Heatmap_Shrub.pdf", height = 12, width = 20)
#heatmap(as.matrix(Shrub), Colv = NULL, RowSideColors = fnc_grp1_color_list$Color)
heatmap.2(as.matrix(Shrub[,Shrub_vnir_names]), dendrogram="row", trace="none", Colv = FALSE, RowSideColors = fnc_grp1_color_list$Color)

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
