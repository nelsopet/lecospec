#Load packages
source("./Functions/lecospectR.R")
require(vegan)

#Read in spectral libraries from both ground and image tagged with the same labels
#Ground spectra
Cleaned_Speclib <- read.csv("./Output/C_001_SC3_Cleaned_SpectralLib.csv")

colnames(Cleaned_Speclib) %>% as.data.frame() %>% View()
unique(Cleaned_Speclib$Functional_group2)


#List of column names to ignore 
#ignore<-colnames(Cleaned_Speclib[,1:40])
ignore<-c(colnames(Cleaned_Speclib[,1:11]), "species_count")

#Rescale ground spectra to min max  
Cleaned_Speclib_tall_Fnc_grp1<-
    Cleaned_Speclib %>% 
    #Comment line below if data should be used before rescaling
    #global_min_max_scale(ignore_cols = ignore) %>%  
    group_by(Functional_group1, Species_name) %>% 
    dplyr::select(Functional_group1, Species_name) %>% 
    unique() %>% 
    ungroup() %>% 
    group_by(Functional_group1) %>%
    tally() %>% 
    dplyr::rename(species_count = n) %>%
  inner_join(Cleaned_Speclib, by="Functional_group1") %>% #colnames()
  global_min_max_scale(ignore_cols = ignore) %>% #head() %>% View()#colnames() #as.matrix() %>% hist()
  group_by(Functional_group1) %>% 
  dplyr::mutate(sample_size = n()) %>% 
  dplyr::mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {sample_size} {"scans,"} {species_count} {"species"})')) %>%
  #mutate(Functional_group1_wN = Functional_group1) %>% 
  ungroup() %>% #colnames() %>% View()
  dplyr::select(X
                ,ScanID
                ,Area
                ,Code_name
                ,Species_name
                ,Functional_group1
                ,Functional_group2
                ,Species_name_Freq
                ,Functional_group1_Freq
                ,Functional_group2_Freq
                ,Functional_group1_wN
                ,sample_size
                ,species_count,
                everything()) %>%
  pivot_longer(cols = `X350`:`X2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X","",Wavelength)) %>% #dplyr::select(Wavelength) %>% unique() %>% as.data.frame() %>% View()
  group_by(Functional_group1_wN, Functional_group1,Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05)) %>%
  mutate(Wavelength = as.numeric(Wavelength),
         Source = "Ground")  %>%
  as.data.frame() %>% #str()
 dplyr::filter(Wavelength>397 & Wavelength<1000)

unique(Cleaned_Speclib_tall_Fnc_grp1$Wavelength) %>% as.data.frame() %>% View()

jpeg("Output/Fnc_grp1_spectral_profiles_minmax.jpg", height = 10000, width = 9000, res = 350)
ggplot(Cleaned_Speclib_tall_Fnc_grp1, aes(Wavelength, Median_Reflectance, group = Functional_group1), scales = "fixed")+
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance, alpha = 0.3))+
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2))+
  labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 1242 scans"), y="Reflectance")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3) 
dev.off()






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

names_ignore<-c("UID",
                "sample_name", 
                "ScanNum",
                "PFT",
                "Functional_group1",
                "X",
                "species_count")

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
  global_min_max_scale(ignore_cols = names_ignore) %>%
  group_by(Functional_group1) %>% 
  dplyr::mutate(sample_size = n()) %>% 
  dplyr::mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {sample_size} {"pixels,"} {species_count} {"PFTs"})')) %>%
  #Use line below for unsmoothed spectra
  #pivot_longer(cols = `X397.593`:`X999.42`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  #Uncomment line below for smoothed spectra
  pivot_longer(cols = `X398`:`X998`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X","",Wavelength)) %>% #colnames()
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
  mutate(Wavelength = as.numeric(Wavelength),
         Source = "Image") %>%
  as.data.frame() 



##Bind both ground and image spectra summaries (quantiles) together
PFT_SPEC_GROUND_IMAGE <- bind_rows(Cleaned_Speclib_tall_Fnc_grp1, PFT_IMG_SPEC_clean_tall)

##Bind all raw spectral libraries from ground and image.
merge_ignore1<-c("ScanID", "Functional_group1") #, "Species_name")

Cleaned_Speclib_merge <-
Cleaned_Speclib %>%
  dplyr::select(ScanID, Functional_group1, X398:X998) %>% #colnames()
  global_min_max_scale(ignore_cols = merge_ignore1) %>% #colnames() #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
  dplyr::rename(UID=ScanID) %>%
  mutate(Source = "Ground") %>%
  #mutate(UID=c(Source, ScanID,Functional_group1))
  dplyr::select(UID,Source, Functional_group1, X398:X998) %>%
  as.data.frame()


#PCA of ground spectra only
ground_PFT_spectra_mat<-Cleaned_Speclib_merge %>% 
  dplyr::select(-UID,-Source, -Functional_group1) %>% 
  as.matrix() 

#Replace any NAs or Zeros with very small value
ground_PFT_spectra_mat[ground_PFT_spectra_mat==0]<-0.00000001
ground_PFT_spectra_mat[is.na(ground_PFT_spectra_mat)]<-0.00000001

#Build PCA with and without sqrt transform
ground_pca<-princomp(sqrt(ground_PFT_spectra_mat)) #, center=FALSE, scale=FALSE)
#ground_pca_pr<-prcomp(sqrt(ground_PFT_spectra_mat)) #, center=FALSE, scale=FALSE)

cols<-palette.colors(n=9)
screeplot(ground_pca)
plot(ground_pca$scores[,1:2], col=cols, pch=c(1:2))
title(main = "PCA of min max rescaled ground spectra")
legend("bottomleft", legend=unique(Cleaned_Speclib_merge$Functional_group1), lty=1, col=c(1:9), cex=0.5)

cols<-palette.colors(n=8)
screeplot(image_pca)
plot(image_pca$scores[,1:2], col=cols)#, pch=c(1:2))


###Clean up image based PFT spectra to merge with ground based spectra
merge_ignore2 = c("UID","FncGrp1")#, "PFT")
PFT_IMG_SPEC_clean_merge<-
  PFT_IMG_SPEC_clean %>%
  dplyr::select(UID, FncGrp1, X398:X998) %>% #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
  global_min_max_scale(ignore_cols = merge_ignore2) %>% #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
  dplyr::rename(Functional_group1 = FncGrp1) %>%
  mutate(Source = "Image") %>%
    dplyr::select(UID,Source, Functional_group1, everything())  %>%
    as.data.frame()


  PFT_IMG_SPEC_clean_merge %>%
    dplyr::select(UID,Source, Functional_group1, everything()) %>%
    pivot_longer(cols = `X398`:`X998`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  #dplyr::filter(is.na(Reflectance))
  dplyr::filter(Reflectance==0)
  

  #PCA of ground spectra only
  image_PFT_spectra_mat<-PFT_IMG_SPEC_clean_merge %>% 
    dplyr::select(-UID,-Source, -Functional_group1) %>% 
    as.matrix() 
  
  #Replace any NAs or Zeros with very small value
  image_PFT_spectra_mat[image_PFT_spectra_mat==0]<-0.00000001
  image_PFT_spectra_mat[is.na(image_PFT_spectra_mat)]<-0.00000001
  
  #Build PCA with and without sqrt transform
  image_pca<-princomp(sqrt(image_PFT_spectra_mat)) #, center=FALSE, scale=FALSE)
  #image_pca_pr<-prcomp(sqrt(image_PFT_spectra_mat)) #, center=FALSE, scale=FALSE)
  
  cols<-palette.colors(n=8)
  screeplot(image_pca)
  plot(image_pca$scores[,1:2], col=cols)#, pch=c(1:2))
  title(main = "PCA of min max rescaled PFT spectra from images")
  #legend(x = -2, y =-1, legend=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), lty=1, col=c(1:9), cex=0.5)
  #legend(x = -2, y =-1, legend=unique(PFT_IMG_SPEC_clean_merge$Functional_group1), lty=1, col=c(1:9), cex=0.5)
  

  
#PFT_IMG_SPEC_clean_merge$UID<-c(PFT_IMG_SPEC_clean_merge$UID)

PFT_SPEC<-bind_rows(Cleaned_Speclib_merge, PFT_IMG_SPEC_clean_merge) 

PFT_SPEC_TALL<-PFT_SPEC %>% dplyr::select(UID,Source, Functional_group1, everything()) %>%
  pivot_longer(cols = `X398`:`X998`,  names_to  = "Wavelength", values_to = "Reflectance") #%>%
#dplyr::filter(is.na(Reflectance))
#dplyr::filter(Reflectance==0)


rownames(tst_mat)<-PFT_SPEC$UID
length(PFT_SPEC$Source)
#res.man <- MANOVA(tst_mat ~ as.factor(PFT_SPEC$Source))

#Remove columns not usable in PCA
tst_mat<-PFT_SPEC %>% 
  dplyr::select(-UID,-Source, -Functional_group1) %>%
  as.matrix()

#Replace any NAs or Zeros with very small value
tst_mat[tst_mat==0]<-0.00000001
tst_mat[is.na(tst_mat)]<-0.00000001
tst_na<-tst_mat[is.na(tst_mat)==TRUE]

#Build PCA with and without sqrt transform
tst_pca<-princomp(sqrt(tst_mat)) #, center=FALSE, scale=FALSE)
#tst_pca_pr<-prcomp(sqrt(tst_mat)) #, center=FALSE, scale=FALSE)

summary(tst_pca)

#How many values in 
seq(1:length(unique(PFT_SPEC$Functional_group1)))
cols<-palette.colors(n=9)
screeplot(tst_pca)
plot(tst_pca$scores[,1:2], col=cols, pch=c(1:2))

#legend(x = -6, y =10, legend=unique(PFT_SPEC$Functional_group1), lty=1, col=c(1:9), cex=0.5)
#legend(x = -6, y =3, legend=unique(PFT_SPEC$Source), pch=c(1:2), cex=0.5)
legend(x = 4, y =0, legend=unique(PFT_SPEC$Functional_group1), lty=1, col=c(1:9), cex=0.5)
legend(x = 2, y =-5, legend=unique(PFT_SPEC$Source), pch=c(1:2), cex=0.5)


    #Cluster plots based on their woody veg
    #QUESTION: Should we use scale() to center the data
    tst_mat_clust<-hclust(dist(tst_mat))
    tst_mat_clust_dend<-as.dendrogram(tst_mat_clust)
    plot(tst_mat_clust, group = as.factor(PFT_SPEC$Source))

    
    ######## Functional group 1 spectral profiles
jpeg("Output/Fnc_grp1_spectral_profiles_PFT_IMG_SPECTRA_ALL.jpg", height = 10000, width = 9000, res = 350)
ggplot(PFT_SPEC_GROUND_IMAGE, aes(Wavelength, Median_Reflectance,group = Functional_group1), scales = "fixed")+
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance, alpha = 0.3))+
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2))+
  labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 1242 scans"), y="Reflectance")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  #geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+
  geom_line(aes(Wavelength, Median_Reflectance,color = Source),size = 2)+
  
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3) 
#facet_wrap(vars(Functional_group1), scales = "fixed", ncol = 3) 

dev.off()

