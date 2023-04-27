#Parse metadata and add to each raster of a single PFT
#Bison Gulch
path<-("Data/Ground_Validation/WhiteRefs/Tarp/")
allfiles<-list.files(path) 
imgs = grep(".envi$", allfiles, value = TRUE)
band_path<-brick(paste(path,imgs[1], sep=""))
band_count<-names(band_path) %>% length()
band_names <- read.csv("./assets/bands.csv")$x[1:band_count] %>% as.vector()
band_count<-names(path) %>% length()

WhiteRefs_labeled<-lapply(1:length(imgs), function(x){ 
imgs_names<-imgs[[x]] %>%
  as.data.frame() %>% 
  dplyr::rename(Img = ".")

tst<-brick(paste(path,imgs[[x]], sep=""))
band_count<-names(tst) %>% length()
names(tst)<-band_names
df <- raster::rasterToPoints(tst) %>% 
  as.data.frame()%>%
  dplyr::select(-x,-y)
new_names<-extract_bands(df)
names(df)<-new_names
meta_len<-nrow(df)
df <- filter_bands(df)
df <- df_to_speclib(df, type="spectrolab")
#df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
#df<-spectrolab::resample(df, new_bands = seq(398, 999, 1), parallel = FALSE)
#PFT<-separate(data.frame(A = imgs_names), col = "V2" , into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")

#PFT$ScanNum<-ifelse(is.na(PFT$ScanNum)==TRUE,1,PFT$ScanNum)
#PFT<-as.data.frame(PFT)

#PFT$UID<-str_match(imgs[x], "(.*?)\\s*.envi") %>%
#  as.data.frame() %>% 
#  dplyr::select(V2) %>%
#  dplyr::rename(PFT_UID=V2)
#  as.data.frame() #%>%
meta1<-str_split(imgs[[x]], pattern = "Tarp") %>% as.data.frame() %>% t() %>% as.data.frame()
site<-meta1$V1
meta2<-str_split(meta1$V2, pattern = ".envi") %>% as.data.frame() %>% t() %>% as.data.frame()
refl<-meta2$V1
meta_fin<-matrix(ncol=2) %>% as.data.frame()
meta_fin[,1]<-site #,refl) # %>% as.data.frame()
meta_fin[,2]<-refl
colnames(meta_fin)<-c("site","refl")
meta(df)<-rep(meta_fin,meta_len)

return(df)
})

tarp_image_spectra<-Reduce(spectrolab::combine,WhiteRefs_labeled)

colnames(as.data.frame(tarp_image_spectra))
tarp_image_spectra_df<-as.data.frame(tarp_image_spectra) %>% dplyr::select(site, refl, `401.296`:`999.42`) #%>% dim

write.csv(tarp_image_spectra_df, "./Data/Ground_Validation/WhiteRefs/Tarp/TarpSpectra.csv")

