#Unit test of reading in each image of pixels from a PFT and converting it to a spectral library
#path<-("Data/Ground_Validation/Imagery/BisonPFT/")
#allfiles<-list.files(path) 
#imgs = grep(".envi$", allfiles, value = TRUE)
#x=1
#
#imgs_names<-str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
#  as.data.frame() %>% 
#  dplyr::select(V2) %>% 
#  as.data.frame()

#tst<-brick(paste(path,imgs[x], sep=""))
#plot(tst)
#
#names(tst)<-band_names
#
#df <- raster::rasterToPoints(tst) %>% 
#  as.data.frame() %>%
#  dplyr::select(-x,-y)
#new_names<-extract_bands(df)
#names(df)<-new_names
##df <- remove_noisy_cols(df, min_index = 50, max_index = 200) %>% as.data.frame()
#df <- filter_bands(df)
#df <- df_to_speclib(df, type="spectrolab")
##plot(df)
#df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
##plot(df)
##df_norm<-spectrolab::normalize(df)
##plot(df_norm)
##Solution for parsing here https://stackoverflow.com/questions/42133934/how-to-split-a-string-on-first-number-only
#PFT<-separate(data.frame(A = imgs_names), col = "V2" , into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
#
#PFT$ScanNum<-ifelse(is.na(PFT$ScanNum)==TRUE,1,PFT$ScanNum)
#PFT<-as.data.frame(PFT)
#
#PFT$UID<-str_match(imgs[x], "(.*?)\\s*.envi") %>%
#  as.data.frame() %>% 
#  dplyr::select(V2) %>%
#  dplyr::rename(PFT_UID=V2)
##  as.data.frame() #%>%
#
#meta(df)<-rep(PFT,length(df))
##plot(df)
#
#meta(df)


#Parse metadata and add to each raster of a single PFT
#Bison Gulch
path<-("Data/Ground_Validation/Imagery/BisonPFT/")
allfiles<-list.files(path) 
imgs = grep(".envi$", allfiles, value = TRUE)
band_path<-brick(paste(path,imgs[1], sep=""))
band_count<-names(band_path) %>% length()
band_names <- read.csv("./assets/bands.csv")$x[1:band_count] %>% as.vector()
band_count<-names(path) %>% length()

BisonPFT_labeled<-lapply(1:length(imgs), function(x){ 
imgs_names<-str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
  as.data.frame() %>% 
  dplyr::select(V2) %>% 
  as.data.frame()

tst<-brick(paste(path,imgs[x], sep=""))
band_count<-names(tst) %>% length()
names(tst)<-band_names
df <- raster::rasterToPoints(tst) %>% 
  as.data.frame()%>%
  dplyr::select(-x,-y)
new_names<-extract_bands(df)
names(df)<-new_names
df <- filter_bands(df)
df <- df_to_speclib(df, type="spectrolab")
#df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
df<-spectrolab::resample(df, new_bands = seq(398, 999, 5), parallel = FALSE)
PFT<-separate(data.frame(A = imgs_names), col = "V2" , into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")

PFT$ScanNum<-ifelse(is.na(PFT$ScanNum)==TRUE,1,PFT$ScanNum)
PFT<-as.data.frame(PFT)

PFT$UID<-str_match(imgs[x], "(.*?)\\s*.envi") %>%
  as.data.frame() %>% 
  dplyr::select(V2) %>%
  dplyr::rename(PFT_UID=V2)
#  as.data.frame() #%>%

meta(df)<-rep(PFT,length(df))
return(df)
})

#Chatanika
path<-("Data/Ground_Validation/Imagery/ChatanikaPFT/")
allfiles<-list.files(path) 
imgs = grep(".envi$", allfiles, value = TRUE)

ChatanikaPFT_labeled<-lapply(1:length(imgs), function(x){ 
  imgs_names<-str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
    as.data.frame() %>% 
    dplyr::select(V2) %>% 
    as.data.frame()
  
  tst<-brick(paste(path,imgs[x], sep=""))
  band_count<-names(tst) %>% length()
  names(tst)<-band_names
  df <- raster::rasterToPoints(tst) %>% 
    as.data.frame()%>%
    dplyr::select(-x,-y)
  new_names<-extract_bands(df)
  names(df)<-new_names
  df <- filter_bands(df)
  df <- df_to_speclib(df, type="spectrolab")
  #df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
  df<-spectrolab::resample(df, new_bands = seq(398, 999, 5), parallel = FALSE)
  
  PFT<-separate(data.frame(A = imgs_names), col = "V2" , into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
  
  PFT$ScanNum<-ifelse(is.na(PFT$ScanNum)==TRUE,1,PFT$ScanNum)
  PFT<-as.data.frame(PFT)
  
  PFT$UID<-str_match(imgs[x], "(.*?)\\s*.envi") %>%
    as.data.frame() %>% 
    dplyr::select(V2) %>%
    dplyr::rename(PFT_UID=V2)
  #  as.data.frame() #%>%
  
  meta(df)<-rep(PFT,length(df))
  return(df)
})

#Eight Mile
path<-("Data/Ground_Validation/Imagery/EightMilePFT/")
allfiles<-list.files(path) 
imgs = grep(".envi$", allfiles, value = TRUE)


EightMilePFT_labeled<-lapply(1:length(imgs), function(x){ 
  imgs_names<-str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
    as.data.frame() %>% 
    dplyr::select(V2) %>% 
    as.data.frame()
  
  tst<-brick(paste(path,imgs[x], sep=""))
  band_count<-names(tst) %>% length()
  names(tst)<-band_names
  df <- raster::rasterToPoints(tst) %>% 
    as.data.frame()%>%
    dplyr::select(-x,-y)
  new_names<-extract_bands(df)
  names(df)<-new_names
  df <- filter_bands(df)
  df <- df_to_speclib(df, type="spectrolab")
  #df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
  df<-spectrolab::resample(df, new_bands = seq(398, 999, 5), parallel = FALSE)
  PFT<-separate(data.frame(A = imgs_names), col = "V2" , into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
  
  PFT$ScanNum<-ifelse(is.na(PFT$ScanNum)==TRUE,1,PFT$ScanNum)
  PFT<-as.data.frame(PFT)
  
  PFT$UID<-str_match(imgs[x], "(.*?)\\s*.envi") %>%
    as.data.frame() %>% 
    dplyr::select(V2) %>%
    dplyr::rename(PFT_UID=V2)
  #  as.data.frame() #%>%
  
  meta(df)<-rep(PFT,length(df))
  return(df)
  
})

path<-("Data/Ground_Validation/Imagery/TwelveMilePFT/")
allfiles<-list.files(path) 
imgs = grep(".envi$", allfiles, value = TRUE)
TwelveMilePFT_labeled<-lapply(1:length(imgs), function(x){ 
  imgs_names<-str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
    as.data.frame() %>% 
    dplyr::select(V2) %>% 
    as.data.frame()
  
  tst<-brick(paste(path,imgs[x], sep=""))
  band_count<-names(tst) %>% length()
  names(tst)<-band_names
  df <- raster::rasterToPoints(tst) %>% 
    as.data.frame()%>%
    dplyr::select(-x,-y)
  new_names<-extract_bands(df)
  names(df)<-new_names
  df <- filter_bands(df)
  df <- df_to_speclib(df, type="spectrolab")
  #df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
  df<-spectrolab::resample(df, new_bands = seq(398, 999, 5), parallel = FALSE)
  PFT<-separate(data.frame(A = imgs_names), col = "V2" , into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
  
  PFT$ScanNum<-ifelse(is.na(PFT$ScanNum)==TRUE,1,PFT$ScanNum)
  PFT<-as.data.frame(PFT)
  
  PFT$UID<-str_match(imgs[x], "(.*?)\\s*.envi") %>%
    as.data.frame() %>% 
    dplyr::select(V2) %>%
    dplyr::rename(PFT_UID=V2)
  #  as.data.frame() #%>%
  
  meta(df)<-rep(PFT,length(df))
  return(df)
})


speclib_list<-c(BisonPFT_labeled,ChatanikaPFT_labeled,EightMilePFT_labeled) #,TwelveMilePFT_labeled)
PFT_image_spectra<-Reduce(spectrolab::combine,speclib_list)

write.csv(as.data.frame(PFT_image_spectra), "./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_smooth.csv")

saveRDS(PFT_image_spectra,"./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_smooth.rds")
