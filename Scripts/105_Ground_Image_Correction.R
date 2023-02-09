source("./Functions/lecospectR.R")
require(glue)
Cleaned_Speclib <- read.csv("./Output/C_001_SC3_Cleaned_SpectralLib.csv") 
Cleaned_Speclib_tall_Fnc_grp1<-read.csv("./Data/C_001_SC3_Cleaned_SpectralLib_tall_Fnc_grp1.csv")
Cleaned_Speclib_tall_Fnc_grp1<-Cleaned_Speclib_tall_Fnc_grp1 %>% dplyr::mutate(Source = "Ground")
PFT_IMG_SPEC_clean_tall<-read.csv("./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean_tall.csv")


##Bind both ground and image spectra summaries (quantiles) together
PFT_SPEC_GROUND_IMAGE <- bind_rows(Cleaned_Speclib_tall_Fnc_grp1, PFT_IMG_SPEC_clean_tall)
#
PFT_SPEC_GROUND_IMAGE %>% dplyr::filter(Source == "Image") %>% dplyr::filter(Wavelength == 450) %>% dplyr::select(Median_Reflectance) %>% unique()
#Plot median refl for one PFT from two different sources

PFT_IMG_GROUND_CORR<-PFT_SPEC_GROUND_IMAGE %>% 
tidyr::pivot_wider(id_cols = c("Functional_group1","Wavelength"), names_from ="Source" , values_from = c("Median_Reflectance","Pct_87_5_Reflectance","Pct_12_5_Reflectance"))
PFT_IMG_GROUND_CORR<-PFT_IMG_GROUND_CORR %>% group_by(Functional_group1) %>%
mutate(Median_Diff = Median_Reflectance_Ground-Median_Reflectance_Image,
Pct_87_5_Diff = Pct_87_5_Reflectance_Ground-Pct_87_5_Reflectance_Image,
Pct_12_5_Diff = Pct_12_5_Reflectance_Ground-Pct_12_5_Reflectance_Image) %>%
dplyr::select(Functional_group1, Wavelength, Median_Diff, Pct_12_5_Diff, Pct_87_5_Diff)

hist(PFT_IMG_GROUND_CORR$Median_Diff)
plot(PFT_IMG_GROUND_CORR$Wavelength, PFT_IMG_GROUND_CORR$Median_Diff)

write.csv(PFT_IMG_GROUND_CORR, "./assets/PFT_Image_Ground_Median_CorrByBandPFT.csv")

jpeg("Output/Fnc_grp1_spectral_profiles_Diff_Ground_Vs_Image.jpg", height = 10000, width = 10000, res = 350)
ggplot((PFT_IMG_GROUND_CORR %>% dplyr::filter(Functional_group1 != "Forb") ), aes(Wavelength, Median_Diff, group = Functional_group1), scales = "fixed")+
  #geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Diff, ymax = Pct_87_5_Diff, alpha = 0.3))+
  #geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2))+
  #labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 1242 scans"), y="Reflectance")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  #geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+
  geom_line(aes(Wavelength, Median_Diff),size = 2)+
  geom_line(aes(Wavelength, Pct_12_5_Diff, col = "red"))+
  geom_line(aes(Wavelength, Pct_87_5_Diff, col = "blue"))+

  facet_wrap(vars(Functional_group1), scales = "fixed", ncol = 4) 
dev.off()

##################

ground_cols_keep<-Cleaned_Speclib %>% dplyr::select(X:X2500) %>%
  colnames() 
ignore<-c(colnames(Cleaned_Speclib[,1:11]), "species_count")
ignore_derivs<-c(colnames(Cleaned_Speclib_derivs[,1:31]), "species_count")

P1<- Cleaned_Speclib %>% 
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
   mutate(Wavelength = as.numeric(Wavelength))            
               P2<-
                P1 %>% 
                    dplyr::filter(Functional_group1 != "Forb") %>% 
                    #dplyr::filter(Source == "Ground") %>%
                    dplyr::left_join(PFT_IMG_GROUND_CORR, by=c("Functional_group1", "Wavelength")) %>% 
                    group_by(Functional_group1, Wavelength) %>% 
                    dplyr::mutate(
                    Reflectance = Reflectance-Median_Diff) %>% #,
                    #Pct_87_5_Reflectance = Pct_87_5_Reflectance - Pct_87_5_Diff,
                    #Pct_12_5_Reflectance = Pct_12_5_Reflectance - Pct_12_5_Diff) %>% #colnames()
                    dplyr::select(-Median_Diff,-Pct_12_5_Diff, -Pct_87_5_Diff) %>% #dim
                    bind_rows(P1)# %>% 
                    #dplyr::filter(Source == "Image"))
                        P2 %>% 
                        group_by(Functional_group1,Wavelength) %>%  
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

