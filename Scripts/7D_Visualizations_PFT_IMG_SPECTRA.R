PFT_IMG_SPEC_clean <- read.csv("./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean_unsmoothed.csv")

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


PFT_IMG_SPEC_clean<-
PFT_IMG_SPEC_clean %>% 
dplyr::select(cols_to_keep) %>%
  dplyr::select(UID, 
                sample_name, 
                ScanNum, 
                FncGrp1, 
                 everything()
                ) %>% 
  dplyr::rename (Functional_group1 = FncGrp1) %>%
  #Use line below for unsmoothed spectra
  pivot_longer(cols = `X397.593`:`X999.42`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  #Uncomment line below for smoothed spectra
  #pivot_longer(cols = `X450`:`X850`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X","",Wavelength)) %>%
  #mutate(Reflectance = round(Reflectance*100,2)) %>%
  group_by(Functional_group1,Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() 

######## Functional group 1 spectral profiles
jpeg("Output/Fnc_grp1_spectral_profiles_PFT_IMG_SPECTRA_unsmoothed.jpg", height = 10000, width = 9000, res = 350)
ggplot(PFT_IMG_SPEC_clean, aes(Wavelength, Median_Reflectance,group = Functional_group1), scales = "fixed")+
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
  #facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3) 
  facet_wrap(vars(Functional_group1), scales = "fixed", ncol = 3) 

dev.off()

