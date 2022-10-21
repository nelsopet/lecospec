require(tidyverse)
require(factoextra)
#Define groups based on spectra
#Need to read in Cleaned_SpecLib ....
tst<-Cleaned_Speclib %>% 
#  dplyr::select(Species_name, everything()) %>% #colnames()
dplyr::select(-ScanID:-Functional_group2_Freq)

##Plot data cubes predicted at the genera level.
unique(Cleaned_Speclib$Functional_group1)
genera_colors = createPalette(length(unique(Cleaned_Speclib$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Cleaned_Speclib$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(Cleaned_Speclib$Functional_group1))));

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
genera_color_list<-Cleaned_Speclib %>% dplyr::select(Functional_group1) %>% inner_join(genera_colors, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)

  
#PCA
spectra.pca = prcomp(tst, scale =TRUE, center=TRUE)
#tst_plots<-ordiplot(spectra.pca, display = "sites")
#points(tst_plots, "sites")
#text(tst_plots, "species", col="blue", cex=0.9)
pca3d(spectra.pca,col=genera_color_list$Color,biplot=TRUE)

biplot(spectra.pca, choices = c(2,3))
jpeg("Output/PCA_species.jpg", height = 500, width = 1000)
#ordiplot(spectra.pca, label = 'Functional_group2')

autoplot(spectra.pca, data = Cleaned_Speclib, colour = 'Functional_group1', shape = 'Functional_group2') +
scale_shape_manual(values = 1:8) +
  scale_color_manual(values=genera_colors$Color)
  
 # %>%
#ordiellipse(spectra.pca,groups = 'Functional_group2')
#autoplot(kmeans(tst,10), data = tst)

#ordiplot(spectra.pca, display = "sites")
dev.off()

#K means
TenGrp<-kmeans(tst, centers = 20)
plot(TenGrp)
fviz_cluster(TenGrp, data=tst)
fviz_nbclust(tst, kmeans, method = "silhouette")
