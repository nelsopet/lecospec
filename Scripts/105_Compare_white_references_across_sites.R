#Load packages
source("Functions/lecospectR.R")

#Read in white ref images
path = "Data/Ground_Validation/WhiteRefs/"
allfiles<-list.files(path) 
imgs = grep(".hdr*", allfiles, value = TRUE)

#Print and paste list of names rather than messing with grepping out stuff
imgs_names<-c("BisonGulch_100251_Bison_Gulch_line2_2019_08_12_01_07_28_sceneWhiteReference"       
,"Bonanza_100066_2018_07_25_21_18_45_sceneWhiteReference"
,"Chatanika_100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59_sceneWhiteReference"
,"EightMile_100124_BlacktandardFlight2_2018_07_28_22_56_17_sceneWhiteReference"
,"MurphyDome_100158_MurphyDomeFlight1_2018_07_31_19_47_11_sceneWhiteReference"
,"TwelveMile_100241_12mile_line3_2019_08_09_21_28_52_sceneWhiteReference")

#Read in the band names
band_names <- read.csv("./assets/bands.csv")$x[1:band_count] %>% as.vector()

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
