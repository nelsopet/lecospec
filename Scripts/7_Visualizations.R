require(Polychrome)
require(gplots)
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
