#Load packages needed
source("Functions/lecospectR.R")
#Define bandnames
band_names <- read.csv("./assets/bands.csv")$x %>% as.vector()
# Parse each site's PFT patches into spectra and metadata

# Bison Gulch
path <- ("Data/Ground_Validation/Imagery/BisonPFT/")
allfiles <- list.files(path)
imgs <- grep(".envi$", allfiles, value = TRUE)

BisonPFT_labeled <- lapply(1:length(imgs), function(x) {
  #Get first image name starting with "PFTs" and ending in ".envi" 
  imgs_names <- str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
    as.data.frame() %>%
    dplyr::select(V2) %>%
    as.data.frame()
  #Load the first PFT patch image
  tst <- brick(paste(path, imgs[x], sep = ""))
  #Get names of and count the number of bands in that PFT patch image and cast from raster to dataframe
  band_count <- names(tst) %>% length()
  names(tst) <- band_names
  df <- raster::rasterToPoints(tst) %>%
    as.data.frame() %>%
    dplyr::select(-x, -y)
  #Get updated names for dataframe that will be numeric
  new_names <- extract_bands(df)
  names(df) <- new_names
  #Filter band that have values >1, <0, Inf or NaN
  df <- filter_bands(df)
  #Cast dataframe to spectrolab object for resampling bands
  df <- df_to_speclib(df, type = "spectrolab")
  #Resample bands to 1nm wide
  df <- spectrolab::resample(df, new_bands = seq(398, 999, 1), parallel = FALSE)
  #Parse image file name int metadata of PFT and scan number for each pixel in the PFT patch image
  PFT <- separate(data.frame(A = imgs_names), col = "V2", into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
  PFT$ScanNum <- ifelse(is.na(PFT$ScanNum) == TRUE, 1, PFT$ScanNum)
  PFT <- as.data.frame(PFT)
  #Create a UID for each image which corresponds to one patch for one PFT from one site
  PFT$UID <- str_match(imgs[x], "(.*?)\\s*.envi") %>%
    as.data.frame() %>%
    dplyr::select(V2) %>%
    dplyr::rename(PFT_UID = V2)
  #Set the metadata for each scan (a pixel) for a single PFT/patch/site 
  meta(df) <- rep(PFT, length(df))
  return(df)
})

# Chatanika
path <- ("Data/Ground_Validation/Imagery/ChatanikaPFT/")
allfiles <- list.files(path)
imgs <- grep(".envi$", allfiles, value = TRUE)

ChatanikaPFT_labeled <- lapply(1:length(imgs), function(x) {
  imgs_names <- str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
    as.data.frame() %>%
    dplyr::select(V2) %>%
    as.data.frame()

  tst <- brick(paste(path, imgs[x], sep = ""))
  band_count <- names(tst) %>% length()
  names(tst) <- band_names
  df <- raster::rasterToPoints(tst) %>%
    as.data.frame() %>%
    dplyr::select(-x, -y)
  new_names <- extract_bands(df)
  names(df) <- new_names
  df <- filter_bands(df)
  df <- df_to_speclib(df, type = "spectrolab")
  # df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
  df <- spectrolab::resample(df, new_bands = seq(398, 999, 1), parallel = FALSE)

  PFT <- separate(data.frame(A = imgs_names), col = "V2", into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")

  PFT$ScanNum <- ifelse(is.na(PFT$ScanNum) == TRUE, 1, PFT$ScanNum)
  PFT <- as.data.frame(PFT)

  PFT$UID <- str_match(imgs[x], "(.*?)\\s*.envi") %>%
    as.data.frame() %>%
    dplyr::select(V2) %>%
    dplyr::rename(PFT_UID = V2)
  #  as.data.frame() #%>%

  meta(df) <- rep(PFT, length(df))
  return(df)
})

# Eight Mile
path <- ("Data/Ground_Validation/Imagery/EightMilePFT/")
allfiles <- list.files(path)
imgs <- grep(".envi$", allfiles, value = TRUE)


EightMilePFT_labeled <- lapply(1:length(imgs), function(x) {
  imgs_names <- str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
    as.data.frame() %>%
    dplyr::select(V2) %>%
    as.data.frame()

  tst <- brick(paste(path, imgs[x], sep = ""))
  band_count <- names(tst) %>% length()
  names(tst) <- band_names
  df <- raster::rasterToPoints(tst) %>%
    as.data.frame() %>%
    dplyr::select(-x, -y)
  new_names <- extract_bands(df)
  names(df) <- new_names
  df <- filter_bands(df)
  df <- df_to_speclib(df, type = "spectrolab")
  # df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
  df <- spectrolab::resample(df, new_bands = seq(398, 999, 1), parallel = FALSE)
  PFT <- separate(data.frame(A = imgs_names), col = "V2", into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")

  PFT$ScanNum <- ifelse(is.na(PFT$ScanNum) == TRUE, 1, PFT$ScanNum)
  PFT <- as.data.frame(PFT)

  PFT$UID <- str_match(imgs[x], "(.*?)\\s*.envi") %>%
    as.data.frame() %>%
    dplyr::select(V2) %>%
    dplyr::rename(PFT_UID = V2)
  #  as.data.frame() #%>%

  meta(df) <- rep(PFT, length(df))
  return(df)
})

#Twelve Mile
#path <- ("Data/Ground_Validation/Imagery/TwelveMilePFT/")
#allfiles <- list.files(path)
#imgs <- grep(".envi$", allfiles, value = TRUE)
#TwelveMilePFT_labeled <- lapply(1:length(imgs), function(x) {
#  imgs_names <- str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
#    as.data.frame() %>%
#    dplyr::select(V2) %>%
#    as.data.frame()
#
#  tst <- brick(paste(path, imgs[x], sep = ""))
#  band_count <- names(tst) %>% length()
#  names(tst) <- band_names
#  df <- raster::rasterToPoints(tst) %>%
#    as.data.frame() %>%
#    dplyr::select(-x, -y)
#  new_names <- extract_bands(df)
#  names(df) <- new_names
#  df <- filter_bands(df)
#  df <- df_to_speclib(df, type = "spectrolab")
#  # df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
#  df <- spectrolab::resample(df, new_bands = seq(398, 999, 1), parallel = FALSE)
#  PFT <- separate(data.frame(A = imgs_names), col = "V2", into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
#
#  PFT$ScanNum <- ifelse(is.na(PFT$ScanNum) == TRUE, 1, PFT$ScanNum)
#  PFT <- as.data.frame(PFT)
#
#  PFT$UID <- str_match(imgs[x], "(.*?)\\s*.envi") %>%
#    as.data.frame() %>%
#    dplyr::select(V2) %>%
#    dplyr::rename(PFT_UID = V2)
#  #  as.data.frame() #%>%
#
#  meta(df) <- rep(PFT, length(df))
#  return(df)
#})

#Bonanza Creek
path <- ("Data/Ground_Validation/Imagery/BonanzaPFT/")
allfiles <- list.files(path)
imgs <- grep(".envi$", allfiles, value = TRUE)


BonanzaPFT_labeled <- lapply(1:length(imgs), function(x) {
  imgs_names <- str_match(imgs[x], "PFTs\\s*(.*?)\\s*.envi") %>%
    as.data.frame() %>%
    dplyr::select(V2) %>%
    as.data.frame()

  tst <- brick(paste(path, imgs[x], sep = ""))
  band_count <- names(tst) %>% length()
  names(tst) <- band_names
  df <- raster::rasterToPoints(tst) %>%
    as.data.frame() %>%
    dplyr::select(-x, -y)
  new_names <- extract_bands(df)
  names(df) <- new_names
  df <- filter_bands(df)
  df <- df_to_speclib(df, type = "spectrolab")
  # df<-spectrolab::resample(df, new_bands = seq(450, 850, 0.5), parallel = FALSE)
  df <- spectrolab::resample(df, new_bands = seq(398, 999, 1), parallel = FALSE)
  PFT <- separate(data.frame(A = imgs_names), col = "V2", into = c("PFT", "ScanNum"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")

  PFT$ScanNum <- ifelse(is.na(PFT$ScanNum) == TRUE, 1, PFT$ScanNum)
  PFT <- as.data.frame(PFT)

  PFT$UID <- str_match(imgs[x], "(.*?)\\s*.envi") %>%
    as.data.frame() %>%
    dplyr::select(V2) %>%
    dplyr::rename(PFT_UID = V2)
  #  as.data.frame() #%>%

  meta(df) <- rep(PFT, length(df))
  return(df)
})
speclib_list <- c(BisonPFT_labeled, ChatanikaPFT_labeled, EightMilePFT_labeled, BonanzaPFT_labeled) # ,TwelveMilePFT_labeled)
PFT_image_spectra <- Reduce(spectrolab::combine, speclib_list)
#range(round(PFT_image_spectra[[values]],digits=4))
write.csv(as.data.frame(PFT_image_spectra), "./Data/Ground_Validation/PFT_image_spectra/PFT_Image_SpectralLib_smooth.csv")
saveRDS(PFT_image_spectra, "./Data/Ground_Validation/PFT_image_spectra/PFT_Image_SpectralLib_smooth.rds")
