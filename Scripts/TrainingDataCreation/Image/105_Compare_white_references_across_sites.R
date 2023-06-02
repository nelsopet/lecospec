#Load packages
source("Functions/lecospectR.R")

#Read in white ref images
path = "Data/Ground_Validation/WhiteRefs/"
allfiles<-list.files(path) 
imgs = grep(".hdr*", allfiles, value = TRUE)
imgs<-imgs[1:4]
#Print and paste list of names rather than messing with grepping out stuff
imgs_names<-c("BisonGulch_100251_Bison_Gulch_line2_2019_08_12_01_07_28_sceneWhiteReference"       
,"Bonanza_100066_2018_07_25_21_18_45_sceneWhiteReference"
,"Chatanika_100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59_sceneWhiteReference"
,"EightMile_100124_BlacktandardFlight2_2018_07_28_22_56_17_sceneWhiteReference")
#,"MurphyDome_100158_MurphyDomeFlight1_2018_07_31_19_47_11_sceneWhiteReference"
#,"TwelveMile_100241_12mile_line3_2019_08_09_21_28_52_sceneWhiteReference")

#Read in the band names
band_names <- read.csv("./assets/bands.csv")$x[1:326] %>% as.vector()

#For each white reference, read in the image, cast it to dataframe, add site and band info
# and rename columns
WhiteRef_df<-lapply(1:length(imgs_names), function(x){ 
band_path<-brick(paste(path,imgs_names[x], sep=""))
band_count<-names(band_path) %>% length()
tst<-band_path
df <- raster::rasterToPoints(tst) %>% 
  as.data.frame() %>%
  dplyr::select(-x,-y) %>%
  t() %>%
  as.data.frame()
colnames(df)<-"Radiance"
df$Site_WhiteRef_Image<-imgs_names[x]
df$Wavelength<-band_names
  return(df)
})

WhiteRef_df<-Reduce(function (x,y) merge(x, y, all=TRUE), WhiteRef_df)
colnames(WhiteRef_df)


jpeg("figures/WhiteRef_by_Site.jpg", height = 3000, width = 5000, res = 350)

ggplot(WhiteRef_df, aes(Wavelength, Radiance, color = Site_WhiteRef_Image), scales = "fixed")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_line(aes(Wavelength, Radiance), size = 2)+
  labs(title = "Spectralon panel Radiance prior to flight by site with image name",
  x = "Wavelength (nm)",
  y = "Radiance (mW/(cm2*sr*um))")
 dev.off()


##Plot tarp reflectance by site

tarp_refl<-read.csv("./Data/Ground_Validation/WhiteRefs/Tarp/TarpSpectra.csv")
tarp_refl_r<-resample_df(tarp_refl)#, drop_existing = TRUE)
## Make histogram of values
tarp_refl_for_hist<-tarp_refl_r %>% 
dplyr::select(site, refl, `X402.593_5nm`:`X992.593_5nm`) %>% #dim
#tidyr::pivot_longer(`X401.296`:`X999.42`, names_to = "Wavelength", values_to = "Reflectance") %>% 
tidyr::pivot_longer(`X402.593_5nm`:`X992.593_5nm`, names_to = "Wavelength", values_to = "Reflectance") %>% 
dplyr::filter(refl == "HighRefl")
#windows();
'X11'()
ggplot(tarp_refl_for_hist, aes(Reflectance, color = site), scales = "fixed")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_histogram(aes(Reflectance, color = site), size = 1.5) +
  #scale_color_grey() +
  #geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +
  #geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +
  labs(title = "Tarp reflectance from imagery by site with image name",
  x = "Wavelength (nm)",
  y = "Reflectance") +
  geom_vline(xintercept = 0.55)+
  facet_wrap(vars(site), scales = "free_y", ncol = 1)


colnames(tarp_refl)
tarp_refl_tall<-tarp_refl_r %>% 
dplyr::select(site, refl, `X402.593_5nm`:`X992.593_5nm`) %>% #dim
tidyr::pivot_longer(`X402.593_5nm`:`X992.593_5nm`, names_to = "Wavelength", values_to = "Reflectance") %>% #head()
mutate(Wavelength = gsub("X","",Wavelength)) %>% 
mutate(Wavelength = gsub("_5nm","",Wavelength)) %>% 
group_by(site, refl, Wavelength) %>%
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
  as.data.frame()
#mutate(Wavelength = as.numeric(Wavelength))

unique(tarp_refl_tall$site)

jpeg("figures/WhiteTarp_by_Site_reprocessed_all.jpg", height = 3000, width = 7000, res = 350)

ggplot(tarp_refl_tall, aes(Wavelength, Median_Reflectance, color = site), scales = "fixed")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_smooth(aes(Wavelength, Median_Reflectance, color = site), size = 1.5) +
  #scale_color_grey() +
  #geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +
 # geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +
  labs(title = "Tarp reflectance from imagery by site with image name",
  x = "Wavelength (nm)",
  y = "Reflectance") +
  ggplot2::geom_hline(yintercept = c(0.55, 0.32, 0.1))+
  ##facet_wrap(vars(site, refl), scales = "free_y", ncol = 3)+
  scale_y_continuous(limits = c(0, 1))+
  facet_wrap(vars(refl), scales = "fixed", ncol = 3)

 dev.off()

table(tarp_refl_tall_)