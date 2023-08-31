#Load packages
source("./Functions/lecospectR.R")

#Read in Ground spectra and add Source
Cleaned_Speclib_tall_Fnc_grp1<-read.csv("./Data/C_001_SC3_Cleaned_SpectralLib_tall_Fnc_grp1.csv")
Cleaned_Speclib_tall_Fnc_grp1<-Cleaned_Speclib_tall_Fnc_grp1 %>% dplyr::mutate(Source = "Ground")

#Read in Image spectra
PFT_IMG_SPEC_clean <- read.csv("./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv")
head(PFT_IMG_SPEC_clean)

PFT_IMG_SPEC_clean %>% group_by(Site,FncGrp1) %>% tally() %>% pivot_wider(names_from = FncGrp1, values_from = n) %>% print(n=100)
#Make a list of variables to drop
names_drop<-c("PFT.1",
              "PFT.2",
              "PFT.3", 
              "UID.1",
              "UID.2", 
              "UID.3", 
              "X", 
              "X.1", 
              "ScanNum.1", 
              "ScanNum.2", 
              "ScanNum.3")
              
#Make a list of variables to keep
cols_to_keep <- setdiff(colnames(PFT_IMG_SPEC_clean), names_drop)

#Summarize image spectra by PFT and band to make interquartile range columns for each band
PFT_IMG_SPEC_clean_tall<-
PFT_IMG_SPEC_clean %>% 
  dplyr::select(cols_to_keep) %>%
  dplyr::select(UID, 
                sample_name, 
                ScanNum, 
                FncGrp1,
                Site, 
                everything()
  ) %>% 
  dplyr::rename (Functional_group1 = FncGrp1) %>% #colnames()
  #group_by(Functional_group1) %>% 
  dplyr::select(Functional_group1, PFT) %>% 
  unique() %>% 
  ungroup() %>% 
  group_by(Functional_group1) %>%
  tally() %>% 
  dplyr::rename(species_count = n) %>%
inner_join(PFT_IMG_SPEC_clean, by=c("Functional_group1"="FncGrp1")) %>% 
  #columnwise_robust_scale(ignore_cols = names_ignore) %>%
  #standardize_df(ignore_cols = names_ignore) %>%

  group_by(Functional_group1) %>% 
  dplyr::mutate(sample_size = n()) %>% 
  dplyr::mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {sample_size} {"pixels,"} {species_count} {"PFTs"})')) %>%
  #Use line below for unsmoothed spectra
  #pivot_longer(cols = `X397.593`:`X999.42`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  #Uncomment line below for smoothed spectra
  pivot_longer(cols = `X398`:`X998`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X","",Wavelength),
  Wavelength = as.numeric(Wavelength)) %>% #colnames()
  #mutate(Reflectance = round(Reflectance*100,2)) %>%
  #group_by(Functional_group1,Wavelength) %>% 
  group_by(Functional_group1_wN, Functional_group1,Wavelength) %>%  
  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(#Wavelength = as.numeric(Wavelength),
         Source = "Image") %>%
  as.data.frame() 

###Clean up image based PFT spectra to merge with ground based spectra
merge_ignore2 = c("UID","FncGrp1")#, "PFT")
#Further reduce image spectra columns to only have those necessary for PCA and merging with ground based spectra with the same columns
PFT_IMG_SPEC_clean_merge<-
  PFT_IMG_SPEC_clean %>%
  dplyr::select(UID, FncGrp1, X398:X998) %>% #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
  #columnwise_min_max_scale(ignore_cols = merge_ignore2) %>% #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
  dplyr::rename(Functional_group1 = FncGrp1) %>%
  mutate(Source = "Image",
         
         Area = (str_split(UID, "PFT") %>% as.data.frame() %>% t %>% as.data.frame() %>% dplyr::rename(Site = V1) %>% dplyr::select(Site))) %>%
  dplyr::select(UID,Source, Functional_group1, Area, everything())  %>%
  as.data.frame()


#Create a list of colors for plotting fnc grp 1
fncgrp1_colors = createPalette(length(unique(PFT_IMG_SPEC_clean$FncGrp1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(PFT_IMG_SPEC_clean$FncGrp1)) %>%
  mutate(ColorNum = seq(1:length(unique(PFT_IMG_SPEC_clean$FncGrp1))));

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
fncgrp1_color_list<-PFT_IMG_SPEC_clean %>% dplyr::select(FncGrp1) %>% inner_join(fncgrp1_colors, by=c("FncGrp1"="FNC_grp1"), keep=FALSE)


#Make a matrix of only reflectance values for PCA
img_mat<-PFT_IMG_SPEC_clean_merge %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

#Build PCA with and without sqrt transform
img_pca<-princomp(img_mat) #, center=FALSE, scale=FALSE)
#img_pca_pr<-prcomp(img_mat[,25:500])#, center=FALSE, scale=FALSE)

#How many values in 
seq(1:length(unique(PFT_IMG_SPEC_clean_merge$Functional_group1))) %>% max()
cols<-palette.colors(n=8)
windows()
jpeg("Output/PCA_AllImageSpectra.jpg")

plot(scores(img_pca)[,1:2], col=fncgrp1_color_list$ColorNum)#, pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))))
#biplot(tst_pca)
title(main="PCA reflectance of PFT image spectra only (no veg indices)")
legend(x = -7, y =5, legend=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), lty=1, col=unique(fncgrp1_color_list$ColorNum), cex=1)
#legend(x = -200, y =-700, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.5)
#legend(x = 300, y =100, legend=unique(PFT_IMG_SPEC_clean_merge$Area), pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))), cex=0.8)
dev.off()

#######Ground
grd_mat<-Cleaned_Speclib_merge %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() #%>% hist()

grd_mat_indices<-get_vegetation_indices(grd_mat, ml_model = "mle/gs/models/b105c68f-c0df-45e6-97b5-f9e8c299752b")
grd_mat_indices[is.na(grd_mat_indices)]<-0.00000001

#Replace any NAs or Zeros with very small value
grd_mat<-grd_mat+0.00000001
grd_mat[is.na(grd_mat)]<-0.00000001
#tst_na<-tst_mat[is.nan(tst_mat)==TRUE]

#Build PCA with and without sqrt transform
grd_pca<-princomp(grd_mat) #, center=FALSE, scale=FALSE)
grd_pca_pr<-prcomp(grd_mat[,25:500])#, center=FALSE, scale=FALSE)

#How many values in 
seq(1:length(unique(Cleaned_Speclib_merge$Functional_group1)))
cols<-palette.colors(n=9)
windows()
screeplot(grd_pca_pr)
windows()
plot(scores(grd_pca_pr)[,1:2], col=cols, pch=c(1:length(unique(Cleaned_Speclib_merge$Area))))
#biplot(tst_pca)
title(main="PCA of reflectance of PFT ground spectra only (no veg indices)")
#legend(x = -6, y =10, legend=unique(PFT_SPEC$Functional_group1), lty=1, col=c(1:9), cex=0.5)
#legend(x = -6, y =3, legend=unique(PFT_SPEC$Source), pch=c(1:2), cex=0.5)
legend(x = -300, y =800, legend=unique(Cleaned_Speclib_merge$Functional_group1), lty=1, col=cols, cex=0.5)
#legend(x = -200, y =-700, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.5)
legend(x = -800, y =800, legend=unique(Cleaned_Speclib_merge$Area), pch=c(1:length(unique(Cleaned_Speclib_merge$Area))), cex=0.8)


windows()
boxplot(scores(grd_pca_pr)[,2]~Cleaned_Speclib_merge$Functional_group1)
title(main="PCA axis 2 of reflectance of PFT ground spectra only (no veg indices)")

grd_pca_pr<-princomp(grd_mat_indices)#, center=FALSE, scale=FALSE)
hist(grd_mat)


predction_img_to_grd<-predict(img_pca_pr, grd_mat)
predction_grd_to_img<-predict(grd_pca_pr, img_mat)

tst<-c(predction_img_to_grd,scores(img_pca)[,1:2])
str(predction_img_to_grd)
plot(predction_img_to_grd)
str(predction_grd_to_img)
str(predction_img_to_grd)
biplot(img_pca_pr)


###########Merged ground and image

spectra_mat<-rbind(img_mat, grd_mat)

# Replace any NAs or Zeros with very small value
spectra_mat[spectra_mat == 0] <- 0.00000001
spectra_mat[is.na(spectra_mat)] <- 0.00000001

#Remove columns not usable in PCA
tst_mat<-spectra_mat

windows()
hist(tst_mat)
#Replace any NAs or Zeros with very small value
tst_mat<-tst_mat+0.00000001
#tst_mat[is.na(tst_mat)]<-0.00000001
#tst_na<-tst_mat[is.nan(tst_mat)==TRUE]

#Build PCA with and without sqrt transform
tst_pca<-princomp(tst_mat) #, center=FALSE, scale=FALSE)
tst_pca_pr<-prcomp(tst_mat[,40:450])#, center=FALSE, scale=FALSE)


#How many values in 
seq(1:length(unique(Speclib_merged$Functional_group1)))
cols<-palette.colors(n=9)
windows()
screeplot(tst_pca_pr)
windows()
plot(scores(tst_pca_pr)[,1:2], col=cols, pch=c(1:2))
#biplot(tst_pca)
title(main="PCA reflectance merging image and ground spectra libraries (no veg indices)")
#legend(x = -6, y =10, legend=unique(PFT_SPEC$Functional_group1), lty=1, col=c(1:9), cex=0.5)
#legend(x = -6, y =3, legend=unique(PFT_SPEC$Source), pch=c(1:2), cex=0.5)
legend(x = 200, y =-400, legend=unique(Speclib_merged$Functional_group1), lty=1, col=c(1:9), cex=0.8)
legend(x = 0, y =-500, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.8)
#legend(x = 200, y =-100, legend=unique(Speclib_merged$Area), pch=c(1:length(unique(Speclib_merged$Area))), cex=0.8)

tst_mat_clust<-hclust(dist(tst_mat))
tst_mat_clust_dend<-as.dendrogram(tst_mat_clust)
plot(tst_mat_clust, group = as.factor(PFT_SPEC$Source))

jpeg("./Output/PCA_ALL_SPECTRA_boxplot_PC2.jpeg", width = 1200, height =400)
windows()
boxplot(scores(tst_pca_pr)[,3]~Speclib_merged$Functional_group1) # nolint
title(main="PCA axis 2 of reflectance merging image and ground spectra libraries (no veg indices)")
dev.off()
#Multivariate analysis of PFT groups 
spectra_PFT_adonis<-adonis2(spectra_mat~as.factor(Speclib_merged$Source)*as.factor(Speclib_merged$Functional_group1), method="euclidean", permutations=100)
spectra_PFT_adonis


## Bind both ground and image spectra summaries (quantiles) together
PFT_SPEC_GROUND_IMAGE <- bind_rows(Cleaned_Speclib_tall_Fnc_grp1, PFT_IMG_SPEC_clean_tall)

PFT_SPEC_GROUND_IMAGE$Source_Color<-ifelse(PFT_SPEC_GROUND_IMAGE$Source == "Ground", "green", "blue")
#

######## Functional group 1 spectral profiles

jpeg("Output/Fnc_grp1_spectral_profiles_PFT_IMG_SPECTRA_ALL_corrected.jpg", height = 10000, width = 10000, res = 350)
ggplot((PFT_SPEC_GROUND_IMAGE %>%
  dplyr::filter(Functional_group1 != "Forb")),
aes(Wavelength, Median_Reflectance, group = Functional_group1),
scales = "fixed"
) +
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance, alpha = 0.3)) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2))+
  labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 1242 scans"), y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    # legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
    legend.position = "none",
    title = element_text(size = 25),
    strip.text = element_text(size = 25),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 90)
  ) +
  # geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+
      geom_line(aes(Wavelength, Median_Reflectance, colour = Source_Color), size = 2) +

  annotate("rect", xmin = 492.4 - (66 / 2), xmax = 492.4 + (66 / 2), ymin = 0, ymax = 1, alpha = .7, color = color[2], fill = color[2]) +
  # Band3 559.8 36, fill =
  annotate("rect", xmin = 559.8 - (36 / 2), xmax = 559.8 + (36 / 2), ymin = 0, ymax = 1, alpha = .7, color = color[3], fill = color[3]) +
  # Band4 664.6 31, fill =
  annotate("rect", xmin = 664.6 - (31 / 2), xmax = 664.6 + (31 / 2), ymin = 0, ymax = 1, alpha = .7, color = color[4], fill = color[4]) +
  # Band5 704.1 15, fill =
  annotate("rect", xmin = 704.1 - (15 / 2), xmax = 704.1 + (15 / 2), ymin = 0, ymax = 1, alpha = .7, color = color[5], fill = color[5]) +
  # Band6<-740.5 15, fill =
  annotate("rect", xmin = 740.5 - (15 / 2), xmax = 740.5 + (15 / 2), ymin = 0, ymax = 1, alpha = .7, color = color[6], fill = color[6]) +
  # Band7<-782.8 20
  annotate("rect", xmin = 782.8 - (20 / 2), xmax = 782.8 + (20 / 2), ymin = 0, ymax = 1, alpha = .2) +
  # Band8<- 864 21
  annotate("rect", xmin = 864 - (21 / 2), xmax = 864 + (21 / 2), ymin = 0, ymax = 1, alpha = .2) +
  # Band9<-945.1 20
  annotate("rect", xmin = 945.1 - (20 / 2), xmax = 945.1 + (20 / 2), ymin = 0, ymax = 1, alpha = .2) +
  # Band10<-1373.5 31
  #annotate("rect", xmin = 1373.5 - (31 / 2), xmax = 1373.5 + (31 / 2), ymin = 0, ymax = 100, alpha = .2) +
  ## Band11<-1613.7 91
  #annotate("rect", xmin = 1613.7 - (91 / 2), xmax = 1613.7 + (91 / 2), ymin = 0, ymax = 100, alpha = .2) +
  ## Band12<-2202.4 175
  #annotate("rect", xmin = 2202.4 - (175 / 2), xmax = 2202.4 + (175), ymin = 0, ymax = 100, alpha = .2) +
  #scale_color_grey() +

  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 4)
# facet_wrap(reorder(~Functional_group1_wN, Source_Color), scales = "fixed", ncol = 4)

dev.off()

# PFT_IMG_SPEC_clean_merge$UID<-c(PFT_IMG_SPEC_clean_merge$UID)

Cleaned_Speclib_merge %>%
  dplyr::select(-UID, -Source, -Functional_group1, `X425`:`X998`) %>%
  as.matrix() %>%
  hist()


PFT_IMG_SPEC_clean_merge %>%
  dplyr::select(-UID, -Source, -Functional_group1, `X425`:`X998`) %>%
  as.matrix() %>%
  hist()

PFT_SPEC <- bind_rows(Cleaned_Speclib_merge, PFT_IMG_SPEC_clean_merge)

PFT_SPEC %>%
  dplyr::mutate(Area = case_when(
    Area == "EightMile" ~ "Eight Mile",
    Area == "Murphy" ~ "Murphy Dome",
    Area == "Big Trail" ~ "Fairbanks Area Big Trail Lake",
    Area == "12mile" ~ "Twelve Mile",
    Area == "Chatanika" ~ "Caribou Poker",
    TRUE ~ Area
  )) %>%
  group_by(Area, Functional_group1, Source) %>%
  tally() %>%
  pivot_wider(names_from = Functional_group1, values_from = n) %>%
  arrange(Area, Source) %>%
  write.csv("./Output/TableScansPFTSiteAllSpectra.csv")


PFT_SPEC_global_rescale <- PFT_SPEC %>%
  global_min_max_scale(ignore_cols = c("UID", "Source", "Functional_group1", "Area"))



PFT_SPEC_TALL <- PFT_SPEC_global_rescale %>%
  dplyr::select(UID, Source, Functional_group1, everything()) %>%
  pivot_longer(cols = `X420`:`X998`, names_to = "Wavelength", values_to = "Reflectance") # %>%
# dplyr::filter(is.na(Reflectance))
# dplyr::filter(Reflectance==0)



# Remove columns not usable in PCA
tst_mat <- PFT_SPEC_global_rescale %>%
  dplyr::select(-UID, -Source, -Functional_group1, `X425`:`X998`) %>%
  as.matrix()

hist(tst_mat)
# Replace any NAs or Zeros with very small value
tst_mat[tst_mat == 0] <- 0.00000001
tst_mat[is.na(tst_mat)] <- 0.00000001
tst_na <- tst_mat[is.nan(tst_mat) == TRUE]

# Build PCA with and without sqrt transform
# tst_pca<-princomp(tst_mat) #, center=FALSE, scale=FALSE)
tst_pca_pr <- prcomp(log10(tst_mat)) # , center=FALSE, scale=FALSE)

summary(tst_pca)

# How many values in
seq(1:length(unique(PFT_SPEC$Functional_group1)))
cols <- palette.colors(n = 9)
screeplot(tst_pca_pr)
plot(scores(tst_pca_pr)[, 1:2], col = cols, pch = c(1:2))
title(main = "PCA reflectance global minmax rescale \n after merging image and ground spectra libraries")

# legend(x = -6, y =10, legend=unique(PFT_SPEC$Functional_group1), lty=1, col=c(1:9), cex=0.5)
# legend(x = -6, y =3, legend=unique(PFT_SPEC$Source), pch=c(1:2), cex=0.5)
legend(x = 4, y = 0, legend = unique(PFT_SPEC$Functional_group1), lty = 1, col = c(1:9), cex = 0.5)
legend(x = 2, y = -5, legend = unique(PFT_SPEC$Source), pch = c(1:2), cex = 0.5)


# Cluster plots based on their woody veg
# QUESTION: Should we use scale() to center the data
tst_mat_clust <- hclust(dist(tst_mat))
tst_mat_clust_dend <- as.dendrogram(tst_mat_clust)
plot(tst_mat_clust, group = as.factor(PFT_SPEC$Source))

jpeg("./Output/PCA_ALL_SPECTRA_boxplot_PC2.jpeg", width = 1200, height = 400)
boxplot(scores(tst_pca_pr)[, 2] ~ PFT_SPEC_global_rescale$Functional_group1)
title(main = "PCA axis 2 of reflectance global minmax rescale after \n merging image and ground spectra libraries")
dev.off()

fnc_grp1_colors <- createPalette(length(unique(PFT_SPEC$Functional_group1)), c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(Functional_group1 = unique(PFT_SPEC$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(PFT_SPEC$Functional_group1))))
fnc_grp1_color_list <- PFT_SPEC %>%
  dplyr::select(Functional_group1) %>%
  inner_join(fnc_grp1_colors, by = "Functional_group1", keep = FALSE)

jpeg("./Output/HEATMAP_ALL_SPECTRA_boxplot.jpeg", width = 6000, height = 4000)

heatmap(log10(tst_mat),
  dendrogram = "row",
  trace = "none",
  Colv = FALSE,
  RowSideColors = fnc_grp1_color_list$Color
)

legend(x = "bottomright", legend = unique(fnc_grp1_color_list$Functional_group1), fill = unique(fnc_grp1_color_list$Color), cex = 2)
dev.off()



############## Vegetation Indices

## Read in vegetation indices of image spectra
Cleaned_Speclib_derivs <- read.csv("./Data/D_002_SpecLib_Derivs.csv")

Cleaned_Speclib_Derivs_merge <-
  Cleaned_Speclib_derivs %>%
  dplyr::select(ScanID, Functional_group1, Boochs:Vogelmann4) %>% # colnames()
  # global_min_max_scale(ignore_cols = merge_ignore1) %>% #colnames() #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
  dplyr::rename(UID = ScanID) %>%
  mutate(Source = "Ground") %>%
  # mutate(UID=c(Source, ScanID,Functional_group1))
  # dplyr::select(UID,Source, Functional_group1, X398:X998) %>%
  as.data.frame()

VI_DF < read.csv("./Data/D_002_Image_SpecLib_Derivs.csv")

hist(Cleaned_Speclib_Derivs_merge %>%
  dplyr::select(-UID, -Source, -Functional_group1) %>%
  columnwise_min_max_scale() %>% # str()
  as.matrix())

ground_PFT_derivs_mat <- Cleaned_Speclib_Derivs_merge %>%
  dplyr::select(-UID, -Source, -Functional_group1) %>%
  columnwise_min_max_scale() %>% # #str()
  as.matrix()

hist(ground_PFT_derivs_mat)

rownames(ground_PFT_derivs_mat) <- Cleaned_Speclib_Derivs_merge$UID

heatmap.2(columnwise_min_max_scale(ground_PFT_derivs_mat) %>% as.matrix(),
  dendrogram = "row",
  trace = "none",
  Colv = FALSE,
  RowSideColors = fnc_grp1_color_list$Color
)
legend(x = "topright", legend = unique(fnc_grp1_color_list$Functional_group1), fill = unique(fnc_grp1_color_list$Color), cex = 0.7)

hist(as.matrix(VI_DF %>%
  dplyr::select(-1:-5)) %>% columnwise_min_max_scale() %>% as.matrix())




## Cast vegetation index VI to matrix and plot using heatmap and PCA
image_PFT_derivs_mat <- as.matrix(VI_DF %>%
  dplyr::select(-1:-5) %>%
  columnwise_min_max_scale())

hist(image_PFT_derivs_mat)

rownames(image_PFT_derivs_mat) <- VI_DF_rescale$UID

heatmap.2(columnwise_min_max_scale(image_PFT_derivs_mat) %>% as.matrix(),
  dendrogram = "row",
  trace = "none",
  Colv = FALSE,
  RowSideColors = fnc_grp1_color_list$Color
)
legend(x = "topright", legend = unique(fnc_grp1_color_list$Functional_group1), fill = unique(fnc_grp1_color_list$Color), cex = 0.7)

dim(ground_PFT_derivs_mat)
dim(image_PFT_derivs_mat)

range(ground_PFT_derivs_mat, na.rm = TRUE)
range(image_PFT_derivs_mat, na.rm = TRUE)

hist(dist(ground_PFT_derivs_mat))
hist(dist(image_PFT_derivs_mat))

PFT_derivs_mat <- rbind(ground_PFT_derivs_mat, image_PFT_derivs_mat)

hist(dist(PFT_derivs_mat))
dim(PFT_derivs_mat)

all_deriv_grp <- c(VI_DF_rescale$Functional_group1, Cleaned_Speclib_Derivs_merge$Functional_group1) %>% as.data.frame()
colnames(all_deriv_grp) <- "Functional_group1"

all_derivs_grp1_colors <- createPalette(length(unique(all_deriv_grp$Functional_group1)), c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(Functional_group1 = unique(all_deriv_grp$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(all_deriv_grp$Functional_group1))))
all_derivs_grp1_color_list <- all_deriv_grp %>%
  dplyr::select(Functional_group1) %>%
  inner_join(all_derivs_grp1_colors, by = "Functional_group1", keep = FALSE)


# PFT_derivs_mat_rescale<-PFT_derivs_mat %>%
#  columnwise_min_max_scale() %>%
#  as.matrix()

PFT_derivs_mat[PFT_derivs_mat <= 0.000001] <- 0.00000001
PFT_derivs_mat[is.nan(PFT_derivs_mat)] <- 0.00000001
PFT_derivs_mat[is.na(PFT_derivs_mat)] <- 0.00000001

PFT_derivs_mat_rescale <- PFT_derivs_mat

hist(dist(PFT_derivs_mat_rescale))


dim(PFT_derivs_mat)
heatmap.2(PFT_derivs_mat_rescale,
  dendrogram = "row",
  trace = "none",
  Colv = FALSE,
  RowSideColors = all_fnc_grp1_color_list$Color
)
legend(x = "topright", legend = unique(all_fnc_grp1_color_list$Functional_group1), fill = unique(all_fnc_grp1_color_list$Color), cex = 0.7)

dev.off()

derivs_pca_pr <- prcomp(PFT_derivs_mat_rescale) # , center=FALSE, scale=FALSE)

cols <- palette.colors(n = 8)
screeplot(derivs_pca_pr)
plot(scores(derivs_pca_pr)[, 2:3], col = all_derivs_grp1_color_list$Color) # , pch=c(1:2))
title(main = "PCA of min max rescaled vegetations indices from ground and image")


jpeg("./Output/PCA_Veg_Indices_boxplot_PC`.jpeg", width = 1200, height = 400)
boxplot(scores(derivs_pca_pr)[, ] ~ c(VI_DF_rescale$Functional_group1, Cleaned_Speclib_Derivs_merge$Functional_group1))
dev.off()


PFT_df <- bind_rows(Cleaned_Speclib_merge, PFT_IMG_SPEC_clean_merge)

all_fnc_grp1_colors <- createPalette(length(unique(PFT_df$Functional_group1)), c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(Functional_group1 = unique(PFT_df$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(PFT_df$Functional_group1))))
all_fnc_grp1_color_list <- PFT_df %>%
  dplyr::select(Functional_group1) %>%
  inner_join(all_fnc_grp1_colors, by = "Functional_group1", keep = FALSE)


area_colors <- createPalette(length(unique(PFT_df$Area)), "#ff0000") %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(Area = unique(PFT_df$Area)) %>%
  mutate(ColorNum = seq(1:length(unique(PFT_df$Area))))
area_color_list <- PFT_df %>%
  dplyr::select(Area) %>%
  inner_join(area_colors, by = "Area", keep = FALSE)

source_colors <- createPalette(length(unique(PFT_df$Source)), "#ff0000") %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(Source = unique(PFT_df$Source)) %>%
  mutate(ColorNum = seq(1:length(unique(PFT_df$Source))))
source_color_list <- PFT_df %>%
  dplyr::select(Source) %>%
  inner_join(source_colors, by = "Source", keep = FALSE)

ground_mat <- Cleaned_Speclib_merge %>%
  dplyr::select(-UID, -Source, -Functional_group1, -Area) %>%
  # columnwise_min_max_scale() %>% #str()
  as.matrix()
rownames(ground_mat) <- Cleaned_Speclib_merge$UID

image_mat <- as.matrix(PFT_IMG_SPEC_clean_merge %>%
  dplyr::select(-1:-4))

rownames(image_mat) <- PFT_IMG_SPEC_clean_merge$UID

PFT_mat <- rbind(ground_mat, image_mat)

PFT_mat[PFT_mat <= 0.000001] <- 0.00000001

heatmap.2(PFT_mat,
  dendrogram = "row",
  trace = "none",
  Colv = FALSE,
  RowSideColors = all_fnc_grp1_color_list$Color
)

dev.off()

spectra_pca_pr <- prcomp(PFT_mat) # , center=FALSE, scale=FALSE)

cols <- palette.colors(n = 8)
screeplot(spectra_pca_pr)
plot(scores(spectra_pca_pr)[, 1:2], col = source_color_list$Color) # , pch=c(1:2))
title(main = "PCA of spectra from images")
