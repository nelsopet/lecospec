# Visualizations from Tundra reflectance manuscript JGR Biogeosciences Nelson et al. 2022

Cleaned_Speclib <- read.csv("./Output/C_001_SC3_Cleaned_SpectralLib.csv")

# ------------------------------------------ Step 3: Create Plots showing spectral profiles ------------------------------------------------- #

# Creates a vector with the name of all the categories of interest
# names_of_classes<-c(as.character(unique(Cleaned_Speclib[,"Functional_group1"])))
#
## Creates an empty list
# FunctionalGroupDf<-list()
#
# for(i in 1:length(names_of_classes)){
#
#  # Subset a functional group
#  FunctionalGroupDf[[i]]<-subset(Cleaned_Speclib,Functional_group1 == names_of_classes[i])
#
#
#  # change the dtaframe to a long dataframe
#  FunctionalGroupDf[[i]]<-gather(FunctionalGroupDf[[i]] ,Wavelength,Reflectance,-1:-9)
#
#
#  # Make column name Wavelength numeric
#  FunctionalGroupDf[[i]]$Wavelength    <-as.numeric(FunctionalGroupDf[[i]]$Wavelength)
#
#  # Plot the output
#  FunctionalGroupDf[[i]]<-FunctionalGroupDf[[i]]%>%
#    group_by(Functional_group1, Wavelength) %>%
#    dplyr::summarise(Median_Reflectance = median(Reflectance),
#                     Max_Reflectance = max(Reflectance),
#                     Min_Reflectance = min(Reflectance),
#                     Upper_Reflectance = quantile(Reflectance, probs = 0.95),
#                     Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
#    as.data.frame()
# }
#
# for(i in 1:length(FunctionalGroupDf)){
#
#  # Creates a ggplot for each functional group with all species
#  ggplot(FunctionalGroupDf[[i]],aes(Wavelength,Median_Reflectance))+geom_line(aes(color = Species_name))+
#    labs(color="Species")+
#    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#          legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=12))+
#    labs(title = paste(names_of_classes[[i]]," Spectral Signatures", sep = ""))
#
#  # Saves the plots
#  ggsave(paste("Output/","C_003_SC3","_",names_of_classes[[i]],".jpg",sep =""))
#
# }


set.seed(123)
## Make a balanced sample
FncGrp2_scan_cnt <- Cleaned_Speclib %>%
  group_by(Functional_group2) %>%
  tally()
# FncGrp2_scan_cnt<-FncGrp2_scan_cnt %>% dplyr::filter(n<)
# range(FncGrp2_scan_cnt$n) #[1]  29 328

# Cleaned_Speclib_resamp29<-Cleaned_Speclib %>%
#  #anti_join(FncGrp1_scan_cnt, by="Functional_group1") %>%
#  group_by(Functional_group2) %>%
#  sample_n(26)
#
# Cleaned_Speclib_resamp29 %>% group_by(Functional_group2) %>% tally()
# write.csv(Cleaned_Speclib_resamp29, "Output/C_001_SC3_Cleaned_SpectralLib29.csv")


## Make a balanced sample
FncGrp1_scan_cnt <- Cleaned_Speclib %>%
  group_by(Functional_group1) %>%
  tally()
FncGrp1_scan_cnt <- FncGrp1_scan_cnt %>% dplyr::filter(n < 7)
range(FncGrp1_scan_cnt$n)
Cleaned_Speclib_resamp4 <- Cleaned_Speclib %>%
  anti_join(FncGrp1_scan_cnt, by = "Functional_group1") %>%
  group_by(Functional_group1) %>%
  sample_n(6)

Cleaned_Speclib_resamp4 %>%
  group_by(Functional_group1) %>%
  tally()
write.csv(Cleaned_Speclib_resamp4, "Output/C_001_SC3_Cleaned_SpectralLib4.csv")


##Tidy version of making the same as above
Cleaned_Speclib_tall_sp<-Cleaned_Speclib %>% 
  pivot_longer(cols = `X350`:`X2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X","",Wavelength)) %>%
  group_by(Functional_group1,Functional_group2,Species_name,Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() # %>%
# group_by(Species_name, Wavelength)

Cleaned_Speclib_tall_Fnc_grp2 <- Cleaned_Speclib %>%
  group_by(Functional_group2) %>%
  mutate(
    Functional_group2 = ifelse(Functional_group2 == "Abiotic", "Non-vegetated surface", Functional_group2),
    Functional_group2_wN = glue('{Functional_group2}  {"(n="} {n()})') %>% as.factor(),
    Functional_group2_wN = factor(Functional_group2_wN,
      levels = c(
        "Lichen  (n= 328)",
        "Moss  (n= 86)",
        "Graminoid  (n= 128)",
        "Forb  (n= 158)",
        "Dwarf Shrub  (n= 130)",
        "Shrub  (n= 326)",
        "Tree  (n= 29)",
        "Non-vegetated surface  (n= 57)"
      )
    )
  ) %>%
  ungroup() %>%
  pivot_longer(cols = `X350`:`X2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Reflectance = round(Reflectance*100,2)) %>%
  group_by(Functional_group2_wN, Functional_group2,Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  dplyr::filter(Wavelength >= 450) %>%
  as.data.frame() # %>%
# group_by(Species_name, Wavelength)


Cleaned_Speclib_tall_Fnc_grp1<-
  Cleaned_Speclib %>% 
  group_by(Functional_group1, Species_name) %>% 
  dplyr::select(Functional_group1, Species_name) %>% 
  unique() %>% 
  ungroup() %>% 
  group_by(Functional_group1) %>%
  tally() %>% 
  dplyr::rename(species_count = n) %>%
  inner_join(Cleaned_Speclib, by="Functional_group1") %>% 
  group_by(Functional_group1) %>% 
  dplyr::mutate(sample_size = n()) %>% 
  dplyr::mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {sample_size} {"scans,"} {species_count} {"species"})')) %>%
  #mutate(Functional_group1_wN = Functional_group1) %>% 
  ungroup() %>%
  pivot_longer(cols = `X350`:`X2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X","",Wavelength)) %>%
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
  dplyr::filter(Wavelength>419 & Wavelength<850)
#group_by(Species_name, Wavelength)





Lichen_noCrusts_Cleaned_Speclib_all<- Cleaned_Speclib %>% 
  #dplyr::filter(Functional_group1 != "Lichen_Crustose_Dark"|Functional_group1 !="Lichen_Crustose_Light") %>%
  dplyr::filter(Functional_group2 =="Lichen") %>%
  group_by(Species_name) %>%
  mutate(Species_wN = glue('{Species_wN} {"(n="} {n()})')) %>%
  ungroup() %>%
  pivot_longer(cols = `350`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  group_by(Functional_group1_wN, Functional_group1, Wavelength) %>%
  dplyr::summarise(
    Median_Reflectance = median(Reflectance),
    Max_Reflectance = max(Reflectance),
    Min_Reflectance = min(Reflectance),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05)
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() # %>%

# Lichen_Fruticose_Light_Cleaned_Speclib_tall <- Cleaned_Speclib_tall %>% dplyr::filter(Functional_group1 == "Lichen_Fruticose_Light")

Moss_Cleaned_Speclib_tall <- Cleaned_Speclib %>%
  dplyr::filter(Functional_group2 == "Moss") %>%
  group_by(Functional_group1) %>%
  mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {n()})')) %>%
  ungroup() %>%
  pivot_longer(cols = `350`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  group_by(Functional_group1_wN, Functional_group1, Wavelength) %>%
  dplyr::summarise(
    Median_Reflectance = median(Reflectance),
    Max_Reflectance = max(Reflectance),
    Min_Reflectance = min(Reflectance),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05)
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() # %>%


Forb_Cleaned_Speclib_tall <-
  Cleaned_Speclib %>%
  dplyr::filter(Functional_group2 == "Forb" | Functional_group2 == "Graminoid") %>%
  group_by(Functional_group1) %>%
  mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {n()})')) %>%
  ungroup() %>%
  pivot_longer(cols = `350`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  group_by(Functional_group1_wN, Functional_group1, Wavelength) %>%
  dplyr::summarise(
    Median_Reflectance = median(Reflectance),
    Max_Reflectance = max(Reflectance),
    Min_Reflectance = min(Reflectance),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05)
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() # %>%

Shrub_Cleaned_Speclib_tall <-
  Cleaned_Speclib %>%
  dplyr::filter(Functional_group2 == "Shrub") %>%
  group_by(Functional_group1) %>%
  mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {n()})')) %>%
  ungroup() %>%
  pivot_longer(cols = `350`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  group_by(Functional_group1_wN, Functional_group1, Wavelength) %>%
  dplyr::summarise(
    Median_Reflectance = median(Reflectance),
    Max_Reflectance = max(Reflectance),
    Min_Reflectance = min(Reflectance),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05)
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() # %>%

Tree_Cleaned_Speclib_tall <-
  Cleaned_Speclib %>%
  dplyr::filter(Functional_group2 == "Tree") %>%
  group_by(Functional_group1) %>%
  mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {n()})')) %>%
  ungroup() %>%
  pivot_longer(cols = `350`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  group_by(Functional_group1_wN, Functional_group1, Wavelength) %>%
  dplyr::summarise(
    Median_Reflectance = median(Reflectance),
    Max_Reflectance = max(Reflectance),
    Min_Reflectance = min(Reflectance),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05)
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() # %>%

Abiotic_Cleaned_Speclib_tall <-
  Cleaned_Speclib %>%
  dplyr::filter(Functional_group2 == "Abiotic") %>%
  group_by(Functional_group1) %>%
  mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {n()})')) %>%
  ungroup() %>%
  pivot_longer(cols = `350`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  group_by(Functional_group1_wN, Functional_group1, Wavelength) %>%
  dplyr::summarise(
    Median_Reflectance = median(Reflectance),
    Max_Reflectance = max(Reflectance),
    Min_Reflectance = min(Reflectance),
    Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
    Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
    Upper_Reflectance = quantile(Reflectance, probs = 0.95),
    Lower_Reflectance = quantile(Reflectance, probs = 0.05)
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() # %>%

######## PFT species spectral profiles
# Sentinel-2 colors
color <- grDevices::hcl.colors(6, palette = "Spectral", rev = TRUE)


jpeg("Output/Lichen_species_no_crusts_spectral_profiles.jpg", height = 5000, width = 7000, res = 350)
ggplot(Lichen_noCrusts_Cleaned_Speclib_all, aes(Wavelength, Median_Reflectance, group = Functional_group1_wN)) +
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +
  labs(title = c("Reflectance by lichen group and sample size with median (red), 75% (dark) and \n 90% (grey) quantiles based on 1302 scans"), y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    # legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
    legend.position = "none",
    title = element_text(size = 25),
    strip.text = element_text(size = 25),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 90)
  ) +
  geom_line(aes(color = "red"), size = 1.5) +

  # annotate("text", label = n) +
  # labs(title = paste(names_of_classes[[x]]," Median, Upper (97%) and Lower (2.5%) Reflectance Profiles", sep = ""))+
  # facet_wrap(vars(Species_name), scales = "fixed")
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3)
# annotation_custom(tableGrob(Cleaned_Speclib_Fnc_grp2_cnt)) #, xmin=35, xmax=50, ymin=-2.5, ymax=-1)
dev.off()
# ggsave("Output/Functional_group1_spectral_profiles.jpg")

# my_grob<-tableGrob(Cleaned_Speclib_Fnc_grp2_cnt)

######## Functional group 2 spectral profiles
jpeg("Output/Fnc_grp2_spectral_profiles.jpg", height = 9000, width = 6000, res = 350)
ggplot(Cleaned_Speclib_tall_Fnc_grp2, aes(Wavelength, Median_Reflectance), scales = "fixed") +

  # labs(title = c("Reflectance by plant functional group and sample \n size with median (black), 75% (dark) and 90% (grey) \n quantiles based on 1302 scans  with vertical bars \n showing Sentinel-2 bandpasses "), y="Reflectance")+
  labs(x = "Wavelength (nm)", y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    # legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
    legend.position = "none",
    title = element_text(size = 40),
    strip.text = element_text(size = 30),
    axis.text = element_text(size = 30),
    axis.text.x = element_text(angle = 90)
  ) +
  # Band 1
  # annotate("rect", xmin = 442.7-(21/2), xmax = 442.7+(21/2), ymin = 0, ymax = 1, alpha = .2, color=color[1], fill =color[1])+
  # Band2, fill =
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
  annotate("rect", xmin = 1373.5 - (31 / 2), xmax = 1373.5 + (31 / 2), ymin = 0, ymax = 100, alpha = .2) +
  # Band11<-1613.7 91
  annotate("rect", xmin = 1613.7 - (91 / 2), xmax = 1613.7 + (91 / 2), ymin = 0, ymax = 100, alpha = .2) +
  # Band12<-2202.4 175
  annotate("rect", xmin = 2202.4 - (175 / 2), xmax = 2202.4 + (175), ymin = 0, ymax = 100, alpha = .2) +
  geom_line(aes(Wavelength, Median_Reflectance, color = "black"), size = 1.5) +
  scale_color_grey() +
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +
  facet_wrap(vars(Functional_group2_wN), scales = "fixed", ncol = 2)
# facet_wrap(vars(forcats::fct_relevel(Functional_group2_wN,
#                          levels = c("Lichen  (n= 328)",
#                                     "Moss  (n= 86)",
#                                     "Graminoid  (n= 128)",
#                                     "Forb  (n= 158)",
#                                     "Dwarf Shrub  (n= 130)",
#                                     "Shrub  (n= 326)",
#                                     "Tree  (n= 29)",
#                                     "Non-vegetated surface  (n= 57)"))))
dev.off()


######## Functional group 1 spectral profiles
jpeg("Output/Fnc_grp1_spectral_profiles.jpg", height = 10000, width = 9000, res = 350)
ggplot(Cleaned_Speclib_tall_Fnc_grp1, aes(Wavelength, Median_Reflectance,group = Functional_group1), scales = "fixed")+
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


######## Moss species spectral profiles
jpeg("Output/Moss_species_spectral_profiles.jpg", height = 5000, width = 7000, res = 350)
ggplot(Moss_Cleaned_Speclib_tall, aes(Wavelength, Median_Reflectance, group = Functional_group1_wN)) +
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance, alpha = 0.3)) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2)) +
  labs(title = c("Reflectance by Moss group and sample size with median (red), 75% (dark) and \n 90% (grey) quantiles based on 1302 scans"), y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    # legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
    legend.position = "none",
    title = element_text(size = 25),
    strip.text = element_text(size = 25),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 90)
  ) +
  geom_line(aes(color = "red"), size = 1.5) +

  # annotate("text", label = n) +
  # labs(title = paste(names_of_classes[[x]]," Median, Upper (97%) and Lower (2.5%) Reflectance Profiles", sep = ""))+
  # facet_wrap(vars(Species_name), scales = "fixed")
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3)
# annotation_custom(tableGrob(Cleaned_Speclib_Fnc_grp2_cnt)) #, xmin=35, xmax=50, ymin=-2.5, ymax=-1)
dev.off()
# ggsave("Output/Functional_group1_spectral_profiles.jpg")

# my_grob<-tableGrob(Cleaned_Speclib_Fnc_grp2_cnt)

######## Non_vegetated_surface species spectral profiles
jpeg("Output/Non_vegetated_surface_species_spectral_profiles.jpg", height = 5000, width = 7000, res = 350)
ggplot(Abiotic_Cleaned_Speclib_tall, aes(Wavelength, Median_Reflectance, group = Functional_group1_wN)) +
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance, alpha = 0.3)) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2)) +
  labs(title = c("Reflectance by Non_vegetated_surface group and sample size with median (red), 75% (dark) and \n 90% (grey) quantiles based on 1302 scans"), y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    # legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
    legend.position = "none",
    title = element_text(size = 25),
    strip.text = element_text(size = 25),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 90)
  ) +
  geom_line(aes(color = "red"), size = 1.5) +

  # annotate("text", label = n) +
  # labs(title = paste(names_of_classes[[x]]," Median, Upper (97%) and Lower (2.5%) Reflectance Profiles", sep = ""))+
  # facet_wrap(vars(Species_name), scales = "fixed")
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 2)
# annotation_custom(tableGrob(Cleaned_Speclib_Fnc_grp2_cnt)) #, xmin=35, xmax=50, ymin=-2.5, ymax=-1)
dev.off()



######## Tree species spectral profiles
jpeg("Output/Tree_species_spectral_profiles.jpg", height = 3500, width = 9000, res = 350)
ggplot(Tree_Cleaned_Speclib_tall, aes(Wavelength, Median_Reflectance, group = Functional_group1_wN)) +
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance, alpha = 0.3)) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2)) +
  labs(title = c("Reflectance by Tree group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 1302 scans"), y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    legend.position = "none",
    title = element_text(size = 25),
    strip.text = element_text(size = 25),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 90)
  ) +
  geom_line(aes(color = "red"), size = 1.5) +
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3)
dev.off()

######## Shrub species spectral profiles
jpeg("Output/Shrub_species_spectral_profiles.jpg", height = 5000, width = 7000, res = 350)
ggplot(Shrub_Cleaned_Speclib_tall, aes(Wavelength, Median_Reflectance, group = Functional_group1_wN)) +
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance, alpha = 0.3)) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2)) +
  labs(title = c("Reflectance by Shrub group and sample size with median (red), 75% (dark) and 90% (grey) \n quantiles based on 1302 scans"), y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    legend.position = "none",
    title = element_text(size = 25),
    strip.text = element_text(size = 25),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 90)
  ) +
  geom_line(aes(color = "red"), size = 1.5) +
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = )
dev.off()


######## Forb species spectral profiles
jpeg("Output/Forb_species_spectral_profiles.jpg", height = 5000, width = 7000, res = 350)
ggplot(Forb_Cleaned_Speclib_tall, aes(Wavelength, Median_Reflectance, group = Functional_group1_wN)) +
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance, alpha = 0.3)) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance, alpha = 0.2)) +
  labs(title = c("Reflectance by Forb group and sample size with median (red), \n 75% (dark) and 90% (grey) quantiles based on 1302 scans"), y = "Reflectance") +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    legend.position = "none",
    title = element_text(size = 25),
    strip.text = element_text(size = 25),
    axis.text = element_text(size = 20),
    axis.text.x = element_ztext(angle = 90)
  ) +
  geom_line(aes(color = "red"), size = 1.5) +
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 2)
dev.off()
