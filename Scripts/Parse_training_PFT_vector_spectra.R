
bandnames <- read.csv("./assets/bands.csv")$x[1:band_count] %>% as.vector()
path<-("Data/Ground_Validation/Imagery/BisonPFT/")
allfiles<-list.files(path) 
imgs = grep(".envi$", allfiles, value = TRUE)

imgs_names<-str_match(imgs, "PFTs\\s*(.*?)\\s*.envi") %>%
  as.data.frame() %>% 
  dplyr::select(V2) %>% 
  as.data.frame()

tst<-brick(paste(path,imgs[1], sep=""))
band_count<-names(tst) %>% length()
names(tst)<-bandnames
df <- raster::rasterToPoints(tst) %>% 
  as.data.frame() %>%
  dplyr::select(-x,-y)
new_names<-extract_bands(df)
names(df)<-new_names
#df <- remove_noisy_cols(df, min_index = 50, max_index = 200) %>% as.data.frame()
df <- filter_bands(df)
df <- df_to_speclib(df, type="spectrolab")
df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
meta(df)<-imgs_names[1,]
plot(df)

BisonPFT_labeled<-lapply(1:length(imgs), function(x){ 
tst<-brick(paste(path,imgs[x], sep=""))
band_count<-names(tst) %>% length()
names(tst)<-bandnames
df <- raster::rasterToPoints(tst) %>% 
  as.data.frame()%>%
  dplyr::select(-x,-y)
new_names<-extract_bands(df)
names(df)<-new_names
df <- remove_noisy_cols(df, max_index = 284) %>% as.data.frame()
df <- filter_bands(df)
df <- df_to_speclib(df, type="spectrolab")
meta(df)<-imgs_names[x,]
return(df)
})


