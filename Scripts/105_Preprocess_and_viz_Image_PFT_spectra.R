require(Polychrome)
require(glue)
require(vegan)
source("Functions/lecospectR.R")
#Image spectra
PFT_IMG_SPEC_clean <- read.csv("./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv")
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
cols_to_keep <- setdiff(colnames(PFT_IMG_SPEC_clean), names_drop)

##Make a site variable
RawUID<-PFT_IMG_SPEC_clean %>% 
  dplyr::select(UID) %>% as.data.frame() #%>%
  #imgs_names<-

SiteNames<-str_split(RawUID[,1], "PFT") %>% as.data.frame() %>% t %>% as.data.frame() %>%dplyr::rename(Site = V1) %>% dplyr::select(Site)
rownames(SiteNames)<-NULL


site_colors = createPalette(length(unique(SiteNames$Site)), "#ff0000") %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(Site = unique(SiteNames$Site)) %>%
  mutate(ColorNum = seq(1:length(unique(SiteNames$Site))));

site_color_list<-SiteNames %>% dplyr::select(Site) %>% inner_join(site_colors, by="Site", keep=FALSE)

names_ignore<-c("X" 
,"UID"
,"sample_name" 
,"ScanNum"
,"Functional_group1"
,"species_count", 
"PFT")

PFT_IMG_SPEC_clean_tall<-
PFT_IMG_SPEC_clean %>% 
  dplyr::select(cols_to_keep) %>%
  dplyr::select(UID, 
                sample_name, 
                ScanNum, 
                FncGrp1, 
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

write.csv(PFT_IMG_SPEC_clean_tall, "./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean_tall.csv")

jpeg("Output/Fnc_grp1_spectral_profiles_ImageSpectra.jpg", height = 10000, width = 9000, res = 350)
ggplot(PFT_IMG_SPEC_clean_tall, aes(Wavelength, Median_Reflectance, group = Functional_group1), scales = "fixed")+
  labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 2561 scans"), y="Reflectance")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  #geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+  
    geom_line(aes(Wavelength, Median_Reflectance,color = Site),size = 2)+  

  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3) 
dev.off()

###Clean up image based PFT spectra to merge with ground based spectra
merge_ignore2 = c("UID","FncGrp1")#, "PFT")
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

PFT_IMG_SPEC_clean_merge_summary<-
PFT_IMG_SPEC_clean_merge %>% 
  group_by(Area,Functional_group1, Source) %>% 
  tally() %>% pivot_wider(names_from = Functional_group1, values_from = n) 

write.csv(PFT_IMG_SPEC_clean_merge_summary,"figures/PFT_IMG_SPEC_summary.csv")
#PCA of ground spectra only
image_PFT_spectra_mat<-PFT_IMG_SPEC_clean_merge %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

#Replace any NAs or Zeros with very small value
image_PFT_spectra_mat[image_PFT_spectra_mat==0]<-0.00000001
image_PFT_spectra_mat[is.na(image_PFT_spectra_mat)]<-0.00000001

#Multivariate analysis of PFT groups 
image_PFT_adonis<-adonis2(image_PFT_spectra_mat~as.factor(PFT_IMG_SPEC_clean_merge$Functional_group1), method="euclidean", permutations=500)
image_PFT_adonis

#Build PCA with and without sqrt transform
#image_pca<-princomp(sqrt(image_PFT_spectra_mat)) #, center=FALSE, scale=FALSE)
image_pca_pr<-prcomp(image_PFT_spectra_mat) #, center=FALSE, scale=FALSE)
plot(scores(image_pca_pr)[,1:2], col=unique(fnc_grp1_color_list$Color))#, pch=c(1:2))
plot(scores(image_pca_pr)[,1:2], col=site_color_list$Color, pch = PFT_IMG_SPEC_clean_merge$Functional_group1)#, pch=c(1:2))
legend('bottomright', legend=unique(site_color_list$Site), lty=1, col=unique(site_color_list$Color), cex=0.5)
legend('bottomleft', legend=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), lty=1, col=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), cex=0.5)

jpeg("./Output/PCA_Image_boxplot_PC1.jpeg", width = 1200, height =400)
boxplot((scores(image_pca_pr)[,1])*-1~PFT_IMG_SPEC_clean_merge$Functional_group1)
#legend('bottomright', legend=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), lty=1, col=fnc_grp1_color_list$Color, cex=0.5)
dev.off()
cols<-palette.colors(n=8)
screeplot(image_pca_pr)
#For princomp
#plot(image_pca$scores[,1:2], col=cols)#, pch=c(1:2))
#For prcomp
title(main = "PCA of min max rescaled PFT spectra from images")
#legend(x = -2, y =-1, legend=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), lty=1, col=c(1:9), cex=0.5)
#legend(x = -2, y =-1, legend=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), lty=1, col=c(1:9), cex=0.5)

##Read in vegetation indices of image spectrq
VI_DF<-read.csv("./Data/D_002_Image_SpecLib_Derivs.csv")

# Removes metadata before function can be applied

#ignore_derivs_image<-DEFINE

image_derivs_rescale_tall<-VI_DF %>%
  group_by(Functional_group1, Species_name) %>% 
  dplyr::select(Functional_group1, Species_name) %>% 
  unique() %>% 
  ungroup() %>% 
  group_by(Functional_group1) %>%
  tally() %>% 
  dplyr::rename(species_count = n) %>%
  inner_join(VI_DF, by="Functional_group1", keep=FALSE) %>% #colnames()
  columnwise_min_max_scale(ignore_cols = ignore_derivs_image) %>% #head() %>% View()#colnames() #as.matrix() %>% hist()
  group_by(Functional_group1) %>% 
  dplyr::mutate(sample_size = n()) %>% 
  dplyr::mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {sample_size} {"scans,"} {species_count} {"species"})')) %>%
  #mutate(Functional_group1_wN = Functional_group1) %>% 
  ungroup() %>% # colnames() %>% as.matrix() %>% View()
  dplyr::select(#X
    UID
    #,Area
    #,Code_name
    ,Species_name
    ,Functional_group1
    #,Functional_group2
    #,Species_name_Freq
    #,Functional_group1_Freq
    #,Functional_group2_Freq
    ,Functional_group1_wN
    ,sample_size
    ,species_count,
    everything()) %>%
  pivot_longer(cols = Boochs:Vogelmann4,  names_to  = "Veg_Index", values_to = "Value") %>%
  # mutate(Wavelength = gsub("X","",Wavelength)) %>% #dplyr::select(Wavelength) %>% unique() %>% as.data.frame() %>% View()
  group_by(Functional_group1_wN, Functional_group1,Veg_Index) %>%  
  dplyr::summarise(Median_Value = median(Value),
                   Max_Value = max(Value, na.rm = TRUE),
                   Min_Value = min(Value, na.rm = TRUE),
                   Pct_87_5_Value = quantile(Value, probs = 0.875, na.rm = TRUE),
                   Pct_12_5_Value = quantile(Value, probs = 0.125, na.rm = TRUE),
                   Upper_Value = quantile(Value, probs = 0.95, na.rm = TRUE),
                   Lower_Value = quantile(Value, probs = 0.05, na.rm = TRUE)
  ) %>%
  #mutate(Wavelength = as.numeric(Wavelength),
  #       Source = "Ground")  %>%
  as.data.frame() #%>% #str()
#dplyr::filter(Wavelength>397 & Wavelength<1000)

VI_DF_rescale<-columnwise_min_max_scale(VI_DF, ignore_cols = colnames(VI_DF[,1:5]))

fnc_grp1_colors = createPalette(length(unique(VI_DF_rescale$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(Functional_group1 = unique(VI_DF_rescale$Functional_group1)) %>%
  mutate(ColorNum = seq(1:length(unique(VI_DF_rescale$Functional_group1))));

fnc_grp1_color_list<-VI_DF_rescale %>% dplyr::select(Functional_group1) %>% inner_join(fnc_grp1_colors, by="Functional_group1", keep=FALSE)

as.matrix(VI_DF_rescale %>% 
            dplyr::select(-1:-5)) %>% hist()
heatmap(as.matrix(VI_DF_rescale %>% 
                    dplyr::select(-1:-5)), 
        dendrogram="row", 
        trace="none", 
        Colv = FALSE,
        RowSideColors = fnc_grp1_color_list$Color)

legend(x='bottomright', legend=unique(fnc_grp1_color_list$Functional_group1), fill=unique(fnc_grp1_color_list$Color), cex=0.7)


#PCA of ground spectra only
image_PFT_derivs_mat<-as.matrix(VI_DF_rescale %>% 
                                  dplyr::select(-1:-5))
as.matrix(VI_DF_rescale %>% 
            dplyr::select(-1:-5)) %>% hist()
#Replace any NAs or Zeros with very small value
image_PFT_derivs_mat[image_PFT_derivs_mat==0]<-0.00000001
image_PFT_derivs_mat[is.na(image_PFT_derivs_mat)]<-0.00000001

#Build PCA with and without sqrt transform
#image_pca<-princomp(sqrt(image_PFT_derivs_mat)) #, center=FALSE, scale=FALSE)
image_derivs_pca_pr<-prcomp(image_PFT_derivs_mat) #, center=FALSE, scale=FALSE)

cols<-palette.colors(n=8)
screeplot(image_derivs_pca_pr)
plot(scores(image_derivs_pca_pr)[,1:2], col=fnc_grp1_color_list$Color)#, pch=c(1:2))
title(main = "PCA of min max rescaled veg indices from image spectra")
legend(x='bottomright', legend=unique(fnc_grp1_color_list$Functional_group1), fill=unique(fnc_grp1_color_list$Color), cex=0.7)


jpeg("./Output/PCA_Image_Veg_Indices_boxplot_PC1.jpeg", width = 1200, height =400)
boxplot(scores(image_derivs_pca_pr)[,1]~VI_DF_rescale$Functional_group1)
dev.off()
