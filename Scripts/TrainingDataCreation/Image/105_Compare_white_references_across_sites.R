#Load packages
source("Functions/lecospectR.R")

#Read in white ref images
path = "Data/Ground_Validation/WhiteRefs/"
allfiles<-list.files(path) 
imgs = grep(".hdr*", allfiles, value = TRUE)
imgs<-imgs[1:4]
#Print and paste list of names rather than messing with grepping out stuff
imgs_names_t<-c("BisonGulch_100251_Bison_Gulch_line2_2019_08_12_01_07_28_sceneWhiteReference")       
#,"Bonanza_100066_2018_07_25_21_18_45_sceneWhiteReference"
#,"Chatanika_100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59_sceneWhiteReference"
#,"EightMile_100124_BlacktandardFlight2_2018_07_28_22_56_17_sceneWhiteReference")

#,"MurphyDome_100158_MurphyDomeFlight1_2018_07_31_19_47_11_sceneWhiteReference"
#,"TwelveMile_100241_12mile_line3_2019_08_09_21_28_52_sceneWhiteReference")

#Read in the band names
band_names <- read.csv("./assets/bands.csv")$x[1:326] %>% as.vector()
band_path<-terra::rast(paste(path,imgs_names_t[1], sep="")
#For each white reference, read in the image, cast it to dataframe, add site and band info
# and rename columns
WhiteRef_df<-lapply(1:length(imgs_names), function(x){ 
band_path<-terra::rast(paste(path,imgs_names_t[x], sep=""))
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
unique(WhiteRef_df$Site_WhiteRef_Image)
head(WhiteRef_df)
WhiteRef_df %>% dplyr::filter(Site_WhiteRef_Image=="Bonanza_100066_2018_07_25_21_18_45_sceneWhiteReference") %>% head
WhiteRef_df %>% dplyr::filter(Site_WhiteRef_Image=="BisonGulch_100251_Bison_Gulch_line2_2019_08_12_01_07_28_sceneWhiteReference") %>% head

#Read in new white references taken from brightest section of calibration tarp (66%)
#Flight lines for use in manuscript

imgs_names<-c("Bison","Bonanza", "Eightmile","Chatanika")

Bison_dir="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/"
Bonanza_dir="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/"
EightMile_dir = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/"
Chatanika_dir = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/"

#Get image names for running predictions for all datacubes in a flight
Bison_SinglePixWhiteRef<-list.files(Bison_dir)[grepl("*sceneWhiteReference$",list.files(Bison_dir))]
Bonanza_SinglePixWhiteRef<-list.files(Bonanza_dir)[grepl("*sceneWhiteReference$",list.files(Bonanza_dir))]
EightMile_SinglePixWhiteRef<-list.files(EightMile_dir)[grepl("*sceneWhiteReference$",list.files(EightMile_dir))]
Chatanika_SinglePixWhiteRef<-list.files(Chatanika_dir)[grepl("*sceneWhiteReference$",list.files(Chatanika_dir))]

NewWhiteRefs<-c(paste(Bison_dir,Bison_SinglePixWhiteRef, sep=""),
paste(Bonanza_dir,Bonanza_SinglePixWhiteRef, sep=""),
paste(EightMile_dir,EightMile_SinglePixWhiteRef, sep=""),
paste(Chatanika_dir,Chatanika_SinglePixWhiteRef, sep="")
)


NewWhiteRef_df<-lapply(1:length(NewWhiteRefs), function(x){ 
band_path<-brick(NewWhiteRefs[x])
band_count<-names(band_path) %>% length()
tst<-band_path
df <- raster::rasterToPoints(tst) %>% 
  as.data.frame() %>%
  dplyr::select(-x,-y) %>%
  t() %>%
  as.data.frame()
colnames(df)<-"Radiance"
df$Site<-imgs_names[x]
df$Wavelength<-band_names
  return(df)
})

NewWhiteRef_df<-Reduce(function (x,y) merge(x, y, all=TRUE), NewWhiteRef_df)

write.csv(NewWhiteRef_df, "Data/Ground_Validation/WhiteRefs/Tarp/SinglePixelWhiteRefs/Single_Tarp_pixel_white_refs.csv")
head(NewWhiteRef_df)

#jpeg("figures/WhiteRef_by_Site.jpg", height = 3000, width = 5000, res = 350)
X11()
ggplot(NewWhiteRef_df, aes(Wavelength, Radiance, color = Site_WhiteRef_Image), scales = "fixed")+
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

jpeg("figures/Tarp66pctReflWhiteRef_by_Site.jpg", height = 3000, width = 5000, res = 350)
#X11()
ggplot(NewWhiteRef_df, aes(Wavelength, Radiance, color = Site_WhiteRef_Image), scales = "fixed")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_line(aes(Wavelength, Radiance), size = 2)+
  labs(title = "Bright (66% reflectance) tarp pixel radiance prior to flight by site with image name",
  x = "Wavelength (nm)",
  y = "Radiance (mW/(cm2*sr*um))")
dev.off()

#Compare Spectralon prior to fligh vs 66 pct reflectance tarp pixel
WhiteRef_df$Source<-"Spectralon"
NewWhiteRef_df$Source<-"Tarp"

FullWhiteRef_df<-bind_rows(WhiteRef_df,NewWhiteRef_df) #%>% str
colnames(FullWhiteRef_df)

FullWhiteRef_df_wide<- FullWhiteRef_df  %>% 
tidyr::pivot_wider(names_from = Source, values_from = Radiance)  %>% 
dplyr::mutate(Wavelength = as.numeric(Wavelength), 
Spectralon = as.numeric(Spectralon), 
Tarp = as.numeric(Tarp), 
Radiance_proportion = Tarp/Spectralon,
Radiance_diff = Spectralon-Tarp) 


jpeg("figures/Spectralon_vs_Tarp66pctReflWhiteRef_by_Site.jpg", height = 3000, width = 5000, res = 350)
#X11()
ggplot(FullWhiteRef_df, aes(Wavelength, Radiance, color = Source), scales = "fixed")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_line(aes(Wavelength, Radiance), size = 2) +
  facet_wrap(vars(Site_WhiteRef_Image))
dev.off()

jpeg("figures/Spectralon_vs_Tarp66pctReflWhiteRef_by_Site_Proportion.jpg", height = 3000, width = 5000, res = 350)
#X11()
ggplot(FullWhiteRef_df_wide, aes(Wavelength, Radiance_proportion, color = Site_WhiteRef_Image), scales = "fixed")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  #  geom_line(aes(Wavelength, Radiance_diff), size = 2) #+
    geom_smooth(aes(Wavelength, Radiance_proportion), size = 2) #+
  #  geom_boxplot(aes(Wavelength, Radiance_proportion), size = 2) #+

  #facet_wrap(vars(Site_WhiteRef_Image))
dev.off()

##Plot tarp reflectance by site
tarp_refl<-read.csv("./Data/Ground_Validation/WhiteRefs/Tarp/TarpSpectra.csv")
head(tarp_refl)
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

ggplot(tarp_refl_tall %>% dplyr::filter(site != "TwelveMile") %>% dplyr::filter(site != "MurphyDome"), aes(Wavelength, Median_Reflectance, color = site), scales = "fixed")+
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

