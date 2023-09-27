#Load packages
source("./Functions/lecospectR.R")
require(Polychrome)
require(vegan)
require(glue)
#Read in Ground spectra 
Cleaned_Speclib<-read.csv("./Output/C_001_SC3_Cleaned_SpectralLib.csv")

#Prepare ground spectral library for merging with image library
Cleaned_Speclib<- Cleaned_Speclib %>% dplyr::rename(Site = Area, UID = ScanID,)
Cleaned_Speclib_meta_names<-c(colnames(Cleaned_Speclib[,1:12]),colnames(Cleaned_Speclib[,2164:2187]))
dim(Cleaned_Speclib)
#Summarize ground spectra by PFT and band to make interquartile range columns for each band
Cleaned_Speclib_tall_Fnc_grp1<- 
Cleaned_Speclib %>% 
    #dplyr::select(cols_to_keep) %>%
    #dplyr::select(UID, 
    #              #sample_name, 
    #              #ScanNum, 
    #              Functional_group1,
    #              Site
    #) %>% 
    #dplyr::rename (Functional_group1 = FncGrp1) %>% #colnames()
    #group_by(Functional_group1) %>% 
    dplyr::select(Functional_group1, Species_name) %>% 
    unique() %>% 
    ungroup() %>% 
    group_by(Functional_group1) %>%
    tally() %>% 
    dplyr::rename(species_count = n) %>%
  inner_join(Cleaned_Speclib, by=c("Functional_group1")) %>% #colnames()
    group_by(Functional_group1) %>% 
    dplyr::mutate(sample_size = n()) %>% 
    dplyr::mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {sample_size} {"scans,"} {species_count} {"species"})')) %>%
pivot_longer(cols = `X350`:`X2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%    
  mutate(Wavelength = gsub("X","",Wavelength)) %>%
  group_by(Functional_group1, Functional_group1_wN, Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength),
         Source = "Ground")  %>%
  as.data.frame() #%>%


write.csv(Cleaned_Speclib_tall_Fnc_grp1, "./Data/C_001_SC3_Cleaned_SpectralLib_tall_Fnc_grp1.csv")

#####
#Read in Image spectra
PFT_IMG_SPEC_clean <- read.csv("./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv")
dim(PFT_IMG_SPEC_clean)
#Make a table of pixels by PFT by site 
#PFT_IMG_SPEC_clean %>% group_by(Site,FncGrp1) %>% tally() %>% pivot_wider(names_from = FncGrp1, values_from = n) %>% print(n=100)
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
  #Filter out unneeded columns
  dplyr::select(cols_to_keep) %>%
  #Rearrange columns
  dplyr::select(UID, 
                #sample_name, 
                #ScanNum, 
                FncGrp1,
                Site, 
                everything()
  ) %>% 
  #Rename columns and coun the number of unique species/functional group 1 names
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
  mutate(Reflectance = round(Reflectance*100,2)) %>%
  #group_by(Functional_group1,Wavelength) %>% 
  group_by(Functional_group1, Functional_group1_wN, Wavelength) %>%  
  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength),
         Source = "Image") %>%
  as.data.frame() 

write.csv(PFT_IMG_SPEC_clean_tall, "./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean_tall.csv")

## Bind both ground and image spectra summaries (quantiles) together
PFT_SPEC_GROUND_IMAGE <- bind_rows(Cleaned_Speclib_tall_Fnc_grp1 %>% 
                                  dplyr::filter(Wavelength<1000) %>%
                                  dplyr::filter(Wavelength>399), 
                                  PFT_IMG_SPEC_clean_tall)
dim(Cleaned_Speclib_tall_Fnc_grp1)
#[1] 19359    11
dim(PFT_IMG_SPEC_clean_tall)
#[1] 4808   11

PFT_SPEC_GROUND_IMAGE$Source_Color<-ifelse(PFT_SPEC_GROUND_IMAGE$Source == "Ground", "green", "blue")

dim(PFT_SPEC_GROUND_IMAGE)
#[1] 24167    12

######## Functional group 1 spectral profiles
# Sentinel-2 colors
color <- grDevices::hcl.colors(6, palette = "Spectral", rev = TRUE)
color[1]<-"e4f6f8"
jpeg("Output/Fnc_grp1_spectral_profiles_PFT_GRD_IMG_SPECTRA_ALL.jpg", height = 10000, width = 10000, res = 350)
ggplot((PFT_SPEC_GROUND_IMAGE %>%
  dplyr::filter(Functional_group1 != "Forb") %>%
  dplyr::filter(Wavelength<1000) %>%
  dplyr::filter(Wavelength>399)) 
  ,
aes(Wavelength, Median_Reflectance, group = Functional_group1),
scales = "fixed"
) +
  # geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+
      

  annotate("rect", xmin = 492.4 - (66 / 2), xmax = 492.4 + (66 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[2], fill = color[2]) +
  # Band3 559.8 36, fill =
  annotate("rect", xmin = 559.8 - (36 / 2), xmax = 559.8 + (36 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[3], fill = color[3]) +
  # Band4 664.6 31, fill =
  annotate("rect", xmin = 664.6 - (31 / 2), xmax = 664.6 + (31 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[4], fill = color[4]) +
  # Band5 704.1 15, fill =
  annotate("rect", xmin = 704.1 - (15 / 2), xmax = 704.1 + (15 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[5], fill = color[5]) +
  # Band6<-740.5 15, fill =
  annotate("rect", xmin = 740.5 - (15 / 2), xmax = 740.5 + (15 / 2), ymin = 0, ymax = 100, alpha = .7, color = color[6], fill = color[6]) +
  # Band7<-782.8 20
  annotate("rect", xmin = 782.8 - (20 / 2), xmax = 782.8 + (20 / 2), ymin = 0, ymax = 100, alpha = .2) +
  # Band8<- 864 21
  annotate("rect", xmin = 864 - (21 / 2), xmax = 864 + (21 / 2), ymin = 0, ymax = 100, alpha = .2) +
  # Band9<-945.1 20
  annotate("rect", xmin = 945.1 - (20 / 2), xmax = 945.1 + (20 / 2), ymin = 0, ymax = 100, alpha = .2) +
  # Band10<-1373.5 31
  #annotate("rect", xmin = 1373.5 - (31 / 2), xmax = 1373.5 + (31 / 2), ymin = 0, ymax = 100, alpha = .2) +
  ## Band11<-1613.7 91
  #annotate("rect", xmin = 1613.7 - (91 / 2), xmax = 1613.7 + (91 / 2), ymin = 0, ymax = 100, alpha = .2) +
  ## Band12<-2202.4 175
  #annotate("rect", xmin = 2202.4 - (175 / 2), xmax = 2202.4 + (175), ymin = 0, ymax = 100, alpha = .2) +
  #scale_color_grey() +
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
  ) + geom_line(aes(Wavelength, Median_Reflectance, colour = Source), size = 2) + 
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 4) + 
  facet_wrap(~reorder(Functional_group1_wN, Source))

dev.off()

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
PFT_IMG_SPEC_clean_merge$Area<-PFT_IMG_SPEC_clean_merge$Area$Site
PFT_IMG_SPEC_clean_merge_meta<- PFT_IMG_SPEC_clean_merge %>% dplyr::select(UID,Source, Functional_group1, Area)
#Resample image spectra at coarser bandpass
PFT_IMG_SPEC_clean_merge_5nm<-resample_df(PFT_IMG_SPEC_clean_merge, min_wavelength = 398, max_wavelength = 998, delta = 5, drop_existing = TRUE)
  PFT_IMG_SPEC_clean_merge_5nm<-cbind(PFT_IMG_SPEC_clean_merge_meta,PFT_IMG_SPEC_clean_merge_5nm)
PFT_IMG_SPEC_clean_merge_10nm<-resample_df(PFT_IMG_SPEC_clean_merge, min_wavelength = 398, max_wavelength = 998, delta = 10, drop_existing = TRUE)
  PFT_IMG_SPEC_clean_merge_10nm<-cbind(PFT_IMG_SPEC_clean_merge_meta,PFT_IMG_SPEC_clean_merge_10nm)
PFT_IMG_SPEC_clean_merge_20nm<-resample_df(PFT_IMG_SPEC_clean_merge, min_wavelength = 398, max_wavelength = 998, delta = 20, drop_existing = TRUE)
  PFT_IMG_SPEC_clean_merge_20nm<-cbind(PFT_IMG_SPEC_clean_merge_meta,PFT_IMG_SPEC_clean_merge_20nm)
PFT_IMG_SPEC_clean_merge_50nm<-resample_df(PFT_IMG_SPEC_clean_merge, min_wavelength = 398, max_wavelength = 998, delta = 50, drop_existing = TRUE)
  PFT_IMG_SPEC_clean_merge_50nm<-cbind(PFT_IMG_SPEC_clean_merge_meta,PFT_IMG_SPEC_clean_merge_50nm)
PFT_IMG_SPEC_clean_merge_100nm<-resample_df(PFT_IMG_SPEC_clean_merge, min_wavelength = 398, max_wavelength = 998, delta = 100, drop_existing = TRUE)
  PFT_IMG_SPEC_clean_merge_100nm<-cbind(PFT_IMG_SPEC_clean_merge_meta,PFT_IMG_SPEC_clean_merge_100nm)

#Further reduce ground spectra columns to only have those necessary for PCA and merging with image based spectra with the same columns
 Cleaned_Speclib_merge <-
  Cleaned_Speclib %>%
  #dplyr::select(UID, Functional_group1, A, X398:X998) %>% #colnames()
  #columnwise_min_max_scale(ignore_cols = merge_ignore1) %>% #colnames() #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
  #dplyr::rename(UID=ScanID) %>%
  mutate(Source = "Ground") %>%
  #mutate(UID=c(Source, ScanID,Functional_group1))
  dplyr::select(UID,Site, Source, Functional_group1, X398:X998) %>%
  dplyr::rename(Area = Site) %>%
  dplyr::mutate(Area = case_when(
    Area == "EightMile" ~ "Eight Mile",
    Area == "Murphy" ~ "Murphy Dome",
    Area == "Big Trail" ~ "Fairbanks Area Big Trail Lake",
    Area == "12mile" ~ "Twelve Mile",
    Area == "Chatanika" ~ "Caribou Poker",
    TRUE ~ Area
  )) %>%
  as.data.frame()

Cleaned_Speclib_merge_meta<-Cleaned_Speclib_merge %>% dplyr::select(UID,Area, Source, Functional_group1)

#Resample ground spectra at coarser bandpass
Cleaned_Speclib_merge_5nm<-resample_df(Cleaned_Speclib_merge, min_wavelength = 398, max_wavelength = 998, delta = 5, drop_existing = TRUE)
  Cleaned_Speclib_merge_5nm<-cbind(Cleaned_Speclib_merge_meta,Cleaned_Speclib_merge_5nm)
Cleaned_Speclib_merge_10nm<-resample_df(Cleaned_Speclib_merge, min_wavelength = 398, max_wavelength = 998, delta = 10, drop_existing = TRUE)
  Cleaned_Speclib_merge_10nm<-cbind(Cleaned_Speclib_merge_meta,Cleaned_Speclib_merge_10nm)
Cleaned_Speclib_merge_20nm<-resample_df(Cleaned_Speclib_merge, min_wavelength = 398, max_wavelength = 998, delta = 20, drop_existing = TRUE)
  Cleaned_Speclib_merge_20nm<-cbind(Cleaned_Speclib_merge_meta,Cleaned_Speclib_merge_20nm)
Cleaned_Speclib_merge_50nm<-resample_df(Cleaned_Speclib_merge, min_wavelength = 398, max_wavelength = 998, delta = 50, drop_existing = TRUE)
  Cleaned_Speclib_merge_50nm<-cbind(Cleaned_Speclib_merge_meta,Cleaned_Speclib_merge_50nm)
Cleaned_Speclib_merge_100nm<-resample_df(Cleaned_Speclib_merge, min_wavelength = 398, max_wavelength = 998, delta = 100, drop_existing = TRUE)
  Cleaned_Speclib_merge_100nm<-cbind(Cleaned_Speclib_merge_meta,Cleaned_Speclib_merge_100nm)

#Merge the ground and image spectra
dim(PFT_IMG_SPEC_clean_merge)
dim(Cleaned_Speclib_merge)
PFT_SPEC<-rbind(PFT_IMG_SPEC_clean_merge,Cleaned_Speclib_merge) 

dim(PFT_SPEC)

PFT_SPEC %>% group_by(Area, Source, Functional_group1) %>% tally() %>% ungroup() %>%
  pivot_wider(names_from = Functional_group1, values_from = n) %>% print(n=100)
PFT_SPEC %>% dplyr::filter(is.na(Source)==TRUE) %>% head

#Make a table of image spectra by site
PFT_IMG_by_site<-
PFT_SPEC %>% 
  dplyr::filter(Source =="Image") %>% 
  group_by(Area, Functional_group1) %>% tally() %>% ungroup() %>%
  pivot_wider(names_from = Area, values_from = n) %>% print(n=100)
write.csv(PFT_IMG_by_site, "./figures/PFT_IMG_by_site.csv")

#Resample PFT_SPEC to have 125 samples per PFT, balanced by site, source and PFT for PCA
PFT_SPEC_bal<-PFT_SPEC %>% group_by(Area, Source, Functional_group1)  %>% slice_sample(n=125)
PFT_SPEC_bal_mat_img<-PFT_SPEC_bal %>% ungroup() %>% dplyr::filter(Source == "Image") %>% dplyr::select(X400:X998) %>% as.matrix() 
PFT_SPEC_bal_mat_img<-PFT_SPEC_bal_mat_img*100
PFT_SPEC_bal_mat_grd<-PFT_SPEC_bal %>% ungroup() %>% dplyr::filter(Source == "Ground") %>% dplyr::select(X400:X998) %>% as.matrix() 
PFT_SPEC_bal_mat<-rbind(PFT_SPEC_bal_mat_img,PFT_SPEC_bal_mat_grd)
PFT_SPEC_bal<-PFT_SPEC_bal %>% dplyr::select(Source, Area, Functional_group1) %>% cbind(as.data.frame(PFT_SPEC_bal_mat))

#Why are there 3922 row with NA metadata after slicing?

PFT_SPEC_bal %>% head#dplyr::filter(is.na(Source)==TRUE) %>% head

PFT_SPEC_bal %>%
  group_by(Source,Functional_group1) %>% tally() %>% ungroup() %>%
  pivot_wider(names_from = Functional_group1, values_from = n) %>% print(n=100)

#Create a list of colors for plotting fnc grp 1 with the image data
fncgrp1_colors = createPalette(length(unique(PFT_IMG_SPEC_clean$FncGrp1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(PFT_IMG_SPEC_clean$FncGrp1)) %>%
  mutate(ColorNum = seq(1:length(unique(PFT_IMG_SPEC_clean$FncGrp1))))

fncgrp1_colors_grd = createPalette(length(unique(Cleaned_Speclib_merge$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(Cleaned_Speclib_merge$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(Cleaned_Speclib_merge$Functional_group1))));

fncgrp1_colors_all = createPalette(length(unique(PFT_SPEC$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(PFT_SPEC$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(PFT_SPEC$Functional_group1))));

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
fncgrp1_color_list<-PFT_IMG_SPEC_clean %>% dplyr::select(FncGrp1) %>% inner_join(fncgrp1_colors, by=c("FncGrp1"="FNC_grp1"), keep=FALSE)
fncgrp1_color_list_grd<-Cleaned_Speclib_merge %>% dplyr::select(Functional_group1) %>% inner_join(fncgrp1_colors, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)
fncgrp1_color_list_all<-PFT_SPEC %>% dplyr::select(Functional_group1) %>% inner_join(fncgrp1_colors_all, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)

##Make colors for balanced dataset
fncgrp1_bal_colors = createPalette(length(unique(PFT_SPEC_bal$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(PFT_SPEC_bal$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(PFT_SPEC_bal$Functional_group1))))

#fnc_grp1_color_list<-Veg_env %>% select(Functional_group2) %>% inner_join(fnc_grp1_colors, by=c("Functional_group2"="FNC_grp1"), keep=FALSE)
fncgrp1_bal_color_list<-PFT_SPEC_bal %>% ungroup() %>% dplyr::select(Functional_group1) %>% inner_join(fncgrp1_bal_colors, by=c("Functional_group1"="FNC_grp1"), keep=FALSE)


#Make a matrix of only image reflectance values for PCA
img_mat<-(PFT_SPEC_bal %>% dplyr::filter(Source == "Image")) %>%
ungroup() %>%
dplyr::select(-Source, -Functional_group1, -Area) %>% as.matrix() 

img_mat<-PFT_IMG_SPEC_clean_merge %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

img_mat_5nm<-PFT_IMG_SPEC_clean_merge_5nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

img_mat_10nm<-PFT_IMG_SPEC_clean_merge_10nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

img_mat_20nm<-PFT_IMG_SPEC_clean_merge_20nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

img_mat_50nm<-PFT_IMG_SPEC_clean_merge_50nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix()

img_mat_100nm<-PFT_IMG_SPEC_clean_merge_100nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix()

#Build PCA with and without sqrt transform
img_pca<-princomp(img_mat) #, center=FALSE, scale=FALSE)
#img_pca_pr<-prcomp(img_mat[,25:500])#, center=FALSE, scale=FALSE)

#PCA figures with image spectra
seq(1:length(unique(PFT_IMG_SPEC_clean_merge$Functional_group1))) %>% max()
cols<-palette.colors(n=8)
windows()
jpeg("Output/PCA_AllImageSpectra_1nm_balanced.jpg")
plot(scores(img_pca)[,1:2], col=fncgrp1_bal_color_list$Color)#, pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))))
#plot(scores(img_pca)[,1:2], col=fncgrp1_color_list$Color)#, pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))))

#biplot(tst_pca)
title(main="PCA reflectance of PFT image spectra only (no veg indices)")
legend(x = -7, y =5, legend=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), lty=1, col=unique(fncgrp1_color_list$ColorNum), cex=1)
#legend(x = -200, y =-700, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.5)
#legend(x = 300, y =100, legend=unique(PFT_IMG_SPEC_clean_merge$Area), pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))), cex=0.8)
dev.off()

#######Ground spectra PCA
grd_mat_1nm<-Cleaned_Speclib_merge %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() #%>% hist()

grd_mat_5nm<-Cleaned_Speclib_merge_5nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

grd_mat_10nm<-Cleaned_Speclib_merge_10nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

grd_mat_20nm<-Cleaned_Speclib_merge_20nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

grd_mat_50nm<-Cleaned_Speclib_merge_50nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix()

grd_mat_100nm<-Cleaned_Speclib_merge_100nm %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix()

#Replace any NAs or Zeros with very small value
#grd_mat<-grd_mat+0.00000001
#grd_mat[is.na(grd_mat)]<-0.00000001
#tst_na<-tst_mat[is.nan(tst_mat)==TRUE]

#Build PCA with and without sqrt transform
grd_pca<-princomp(grd_mat_1nm) #, center=FALSE, scale=FALSE)
#grd_pca_pr<-prcomp(grd_mat[,25:500])#, center=FALSE, scale=FALSE)

#PCA figure for ground spectra
seq(1:length(unique(Cleaned_Speclib_merge$Functional_group1)))
cols<-palette.colors(n=9)
windows()
screeplot(grd_pca)
windows()
jpeg("Output/PCA_AllGroundSpectra_1nm.jpg")
plot(scores(grd_pca)[,1:2], col=fncgrp1_color_list_grd$Color)#, pch=c(1:length(unique(Cleaned_Speclib_merge$Area))))
#biplot(tst_pca)
title(main="PCA of reflectance of PFT ground spectra only (no veg indices)")
#legend(x = -6, y =10, legend=unique(PFT_SPEC$Functional_group1), lty=1, col=c(1:9), cex=0.5)
#legend(x = -6, y =3, legend=unique(PFT_SPEC$Source), pch=c(1:2), cex=0.5)
legend(x = -600, y =800, legend=unique(Cleaned_Speclib_merge$Functional_group1), lty=1, col=unique(fncgrp1_color_list_grd$Color), cex=0.5)
#legend(x = -200, y =-700, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.5)
#legend(x = -600, y =800, legend=unique(Cleaned_Speclib_merge$Area), pch=c(1:length(unique(Cleaned_Speclib_merge$Area))), cex=0.8)
dev.off()

###########Merged ground and image

spectra_mat_1nm<-rbind(img_mat*100, grd_mat_1nm)
spectra_mat_5nm<-rbind(img_mat_5nm*100, grd_mat_5nm)
spectra_mat_10nm<-rbind(img_mat_10nm*100, grd_mat_10nm)
spectra_mat_20nm<-rbind(img_mat_20nm*100, grd_mat_20nm)
spectra_mat_50nm<-rbind(img_mat_50nm*100, grd_mat_50nm)

#Make a list of colors for plotting ground vs image 
spectra_mat_source_color<-c(rep("red",length(PFT_IMG_SPEC_clean_merge_meta$Source)),rep("blue",length(Cleaned_Speclib_merge_meta$Source)))
spectra_mat_source_shape<-c(rep(1,length(PFT_IMG_SPEC_clean_merge_meta$Source)),rep(2,length(Cleaned_Speclib_merge_meta$Source)))

# Replace any NAs or Zeros with very small value
spectra_mat_5nm[spectra_mat_5nm <= 0] <- 0.00000001
spectra_mat_5nm[is.na(spectra_mat_5nm)] <- 0.00000001
spectra_mat_50nm[spectra_mat_50nm == 0] <- 0.00000001
spectra_mat_50nm[is.na(spectra_mat_50nm)] <- 0.00000001

#Remove columns not usable in PCA
#tst_mat<-spectra_mat_50nm
tst_mat<-PFT_SPEC_bal_mat

#Replace any NAs or Zeros with very small value
#tst_mat<-tst_mat+0.00000001
tst_mat[tst_mat<0]<-0.00000001
#tst_mat[is.na(tst_mat)]<-0.00000001
#tst_na<-tst_mat[is.nan(tst_mat)==TRUE]
#windows()
dim(tst_mat)

#Build PCA with and without sqrt transform
tst_pca<-princomp(sqrt(tst_mat)) #, center=FALSE, scale=FALSE)
tst_pca_df<-scores(tst_pca)[,1:6]
summary(tst_pca)
head(tst_pca_df)
#tst_pca_pr<-prcomp(tst_mat[,40:450])#, center=FALSE, scale=FALSE)
windows()
length(scores(tst_pca)[,1])
unique(PFT_SPEC_bal$Functional_group1)
boxplot(scores(tst_pca)[,1] ~ PFT_SPEC_bal$Functional_group1+PFT_SPEC_bal$Source)# col=fncgrp1_color_list_all$Color, pch=spectra_mat_source_shape)
anova(aov(scores(tst_pca)[,1] ~ PFT_SPEC_bal$Functional_group1+PFT_SPEC_bal$Source))# col=fncgrp1_color_list_all$Color, pch=spectra_mat_source_shape)


#How many values in 
cols<-palette.colors(n=9)
#windows()
screeplot(tst_pca)
#Factor to plot PFT by source
PFT_by_Source<-PFT_SPEC_bal %>% ungroup() %>% mutate(PFT_by_Source = paste(Source,Functional_group1)) %>% dplyr::select(PFT_by_Source) #%>% unique()
PFT_by_Source[1]
windows()
jpeg("Output/PCA_AllSpectra_1nm_balanced_axis12_sqrt.jpg")
plot(scores(tst_pca)[,c(1,2)], col=fncgrp1_bal_color_list$Color, pch=c(1,2))
#ordihull(scores(tst_pca)[,c(1,2)], groups = as.factor(PFT_by_Source$PFT_by_Source))
#biplot(tst_pca)
title(main="PCA reflectance merging image and ground spectra libraries (no veg indices)")
#legend(x = -6, y =10, legend=unique(PFT_SPEC$Functional_group1), lty=1, col=c(1:9), cex=0.5)
#legend(x = -6, y =3, legend=unique(PFT_SPEC$Source), pch=c(1:2), cex=0.5)
#1nm wide
legend(x = -500, y = 1000, legend=unique(PFT_SPEC_bal$Functional_group1), pch = 1, lty=1, col=unique(fncgrp1_bal_color_list$Color), cex=0.8)
legend(x = -500, y = 600, legend=unique(PFT_SPEC_bal$Source), pch=c(1:2), cex=0.8)

##50nm wide
#legend(x = 45, y = 150, legend=unique(Speclib_merged$Functional_group1), pch = 1, lty=1, col=unique(fncgrp1_color_list_all$Color), cex=0.8)
#legend(x = 50, y =60, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.8)
#legend(x = 200, y =-100, legend=unique(Speclib_merged$Area), pch=c(1:length(unique(Speclib_merged$Area))), cex=0.8)
dev.off()

#Make clusters with the spectra or PCA axes
tst_mat_clust<-hclust(dist(tst_pca_df))
tst_mat_clust_dend<-as.dendrogram(tst_mat_clust)
windows()
plot(tst_mat_clust, col = unique(fncgrp1_bal_color_list$Color))

#Boxplot of PCA axis scores
jpeg("./Output/PCA_ALL_SPECTRA_boxplot_PC2.jpeg", width = 1200, height =400)
windows()
#Base R boxplot
#boxplot(scores(tst_pca)[,2]~PFT_SPEC_bal$Functional_group1+PFT_SPEC_bal$Source, col = c("red","blue"), cex.axis = 0.75,las =2) # nolint
#title(main="PCA axis 2 of reflectance merging image and ground spectra libraries (no veg indices)")

dev.off()
  ##GGPlot version of PCA boxplot
  PCA_df<-cbind(scores(tst_pca), PFT_SPEC_bal)
  PCA_box<-ggplot(PCA_df %>% group_by(Functional_group1, Source))
  windows()
  PCA_box+geom_boxplot(aes(Comp.1,Functional_group1, color = Source))

#Multivariate analysis of PFT groups 
#spectra_PFT_adonis<-adonis2(spectra_mat~as.factor(Speclib_merged$Source)*as.factor(Speclib_merged$Functional_group1), method="euclidean", permutations=100)
#spectra_PFT_adonis

#PFT_SPEC %>%
#  dplyr::mutate(Area = case_when(
#    Area == "EightMile" ~ "Eight Mile",
#    Area == "Murphy" ~ "Murphy Dome",
#    Area == "Big Trail" ~ "Fairbanks Area Big Trail Lake",
#    Area == "12mile" ~ "Twelve Mile",
#    Area == "Chatanika" ~ "Caribou Poker",
#    TRUE ~ Area
#  )) %>%
#  group_by(Area, Functional_group1, Source) %>%
#  tally() %>%
#  pivot_wider(names_from = Functional_group1, values_from = n) %>%
#  arrange(Area, Source) %>%
#  write.csv("./Output/TableScansPFTSiteAllSpectra.csv")

