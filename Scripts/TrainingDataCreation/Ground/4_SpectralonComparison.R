#Load packages
source("Functions/lecospectR.R")

Bison_path<-"M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100248_Bison_Gulch_white_2019_08_12_00_33_04"
#Bonanza_path="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/"
EightMile_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100122_WhitetandardFlight2_2018_07_28_22_55_08"
Chatanika_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100127_white_ref_2018_07_29_20_23_01/"
TwelveMile_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100240_12mile_line3_white_2019_08_09_21_27_10/"
Murphy_path = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73118/ImagingSpectrometer/DataFolders/100155_WhiteStandard_2018_07_31_19_39_41"

Bison_whiteref<-list.files(Bison_path)[grepl("*sceneWhiteReference$",list.files(Bison_path))]
#Bonanza_whiteref<-list.files(Bonanza_path)[grepl("*sceneWhiteReference$",list.files(Bonanza_path))]
Eightmile_whiteref<-list.files(EightMile_path)[grepl("*sceneWhiteReference$",list.files(EightMile_path))]
Chatanika_whiteref<-list.files(Chatanika_path)[grepl("*sceneWhiteReference$",list.files(Chatanika_path))]
TwelveMile_whiteref<-list.files(TwelveMile_path)[grepl("*sceneWhiteReference$",list.files(TwelveMile_path))]
Murphy_whiteref<-list.files(Murphy_path)[grepl("*sceneWhiteReference$",list.files(Murphy_path))]


Bison_spectralon<-terra::rast(paste(Bison_path,Bison_whiteref, sep="/"))
#Bonanza_spectralon<-terra::rast(paste(Bonanza_path,"whiteReference_2018_07_25_14_51_06", sep="/"))
Eightmile_spectralon<-terra::rast(paste(EightMile_path,Eightmile_whiteref, sep="/"))
Chatanika_spectralon<-terra::rast(paste(Chatanika_path,Chatanika_whiteref, sep="/"))
TwelveMile_spectralon<-terra::rast(paste(TwelveMile_path,TwelveMile_whiteref, sep="/"))
Murphy_spectralon<-terra::rast(paste(Murphy_path,Murphy_whiteref, sep="/"))

#Read in the band names
band_names <- read.csv("./assets/bands.csv")$x[1:326] %>% as.vector()
t(as.data.frame(Bison_spectralon))

#For each white reference, read in the image, cast it to dataframe, add site and band info
# and rename columns
All_spectralon<-cbind(Bison_spectralon,Eightmile_spectralon, Chatanika_spectralon, TwelveMile_spectralon, Murphy_spectralon)
head(t(All_spectralon))
All_spectralon_names<-c("Bison","Eightmile","Chatanika", "TwelveMile", "Murphy")
Spectralon_df<-lapply(1:length(All_spectralon_names), function(x){ 
#band_path<-brick(paste(path,imgs_names[x], sep=""))
#band_count<-names(band_path) %>% length()
tst<-All_spectralon[x]
df <- #raster::rasterToPoints(tst) %>% 
  as.data.frame(tst) %>%
  #dplyr::select(-x,-y) %>%
  t() %>%
  as.data.frame()
colnames(df)<-"Radiance"
df$Site<-All_spectralon_names[x]
df$Wavelength<-rownames(df)
rownames(df)<-NULL  
  return(df)
  rm(tst)
})

Spectralon_df<-Reduce(function (x,y) merge(x, y, all=TRUE), Spectralon_df)
Spectralon_df<- Spectralon_df %>%
mutate(Wavelength = gsub("X","",Wavelength)) %>% 
mutate(Wavelength = gsub(".nm","",Wavelength)) %>%
mutate(Wavelength = as.numeric(Wavelength)) %>% 
group_by(Site, Wavelength, Radiance)

str(ungroup(Spectralon_df))
windows()

#Visualize radiance from spectralong across sites
ggplot(Spectralon_df, aes(Wavelength, Radiance, group=Site))+geom_line(aes(color=Site))

#Write spectralon radiance by site to disk
write.csv(Spectralon_df, "Data/Ground_Validation/WhiteRefs/Spectralon/radiance/Spectralon_all_sites.csv")
