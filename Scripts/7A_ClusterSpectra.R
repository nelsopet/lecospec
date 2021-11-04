


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
