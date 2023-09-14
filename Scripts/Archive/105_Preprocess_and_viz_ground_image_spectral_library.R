#Load packages
source("./Functions/lecospectR.R")
require(vegan)
require(glue)
require(Polychrome)
require(PERMANOVA)
#Read in spectral libraries from both ground and image tagged with the same labels
#Ground spectra
#Cleaned_Speclib <- read.csv("./Output/C_001_SC3_Cleaned_SpectralLib.csv") 
#Cleaned_Speclib <- read.csv("./Output/C_001_SC3_Cleaned_SpectralLib_CommonSp.csv") 
Cleaned_Speclib <- read.csv("./Output/C_001_SC3_Cleaned_SpectralLib_CenAkCommonSp.csv") 

#Cleaned_Speclib_derivs<-read.csv("./Output/D_002_SpecLib_Derivs.csv")

#List of column names to ignore 
#ignore<-colnames(Cleaned_Speclib[,1:40])
ground_cols_keep<-Cleaned_Speclib %>% dplyr::select(X:X2500) %>%
  colnames() 
  
  
ignore<-c(colnames(Cleaned_Speclib[,1:11]), "species_count")
ignore_derivs<-c(colnames(Cleaned_Speclib_derivs[,1:31]), "species_count")

#Rescale ground spectra to min max  
Cleaned_Speclib_tall_Fnc_grp1<-
    Cleaned_Speclib %>% 
    #Comment line below if data should be used before rescaling
    group_by(Functional_group1, Species_name) %>% 
    dplyr::select(Functional_group1, Species_name) %>% 
    unique() %>% 
    ungroup() %>% 
    group_by(Functional_group1) %>%
    tally() %>% 
    dplyr::rename(species_count = n) %>%
  inner_join(Cleaned_Speclib %>% dplyr::select(all_of(ground_cols_keep)), by="Functional_group1") %>% #colnames() %>% as.data.frame() %>% View()
  #columnwise_robust_scale(ignore_cols = ignore) %>% #head() %>% View()#colnames() #as.matrix() %>% hist()
  #standardize_df(ignore_cols = ignore) %>% #head() %>% View()#colnames() #as.matrix() %>% hist()
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
                #,Species_name_Freq
                #,Functional_group1_Freq
                #,Functional_group2_Freq
                ,Functional_group1_wN
                ,sample_size
                ,species_count,
                everything()) %>%
  pivot_longer(cols = `X350`:`X2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X","",Wavelength),
         Reflectance = Reflectance/100) %>% #dplyr::select(Wavelength) %>% unique() %>% as.data.frame() %>% View()
  group_by(Functional_group1_wN, Functional_group1,Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength))  %>%
  as.data.frame() %>%
 dplyr::filter(Wavelength>397 & Wavelength<1000)

  
#write.csv(Cleaned_Speclib_tall_Fnc_grp1, "./Data/C_001_SC3_Cleaned_SpectralLib_tall_Fnc_grp1.csv")
write.csv(Cleaned_Speclib_tall_Fnc_grp1, "./Data/C_001_SC3_Cleaned_SpectralLib_CenAkCommonSp_tall_Fnc_grp1.csv")

jpeg("Output/Fnc_grp1_spectral_profiles_AllSpectra.jpg", height = 10000, width = 9000, res = 350)
ggplot(Cleaned_Speclib_tall_Fnc_grp1, aes(Wavelength, Median_Reflectance, group = Functional_group1), scales = "fixed")+
  labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 2561 scans"), y="Reflectance")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+  
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3) 
dev.off()



