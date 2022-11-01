#Image spectra
PFT_IMG_SPEC_clean <- read.csv("./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv")
names_drop<-c("PFT.1",
              "PFT.2",
              "PFT.3", 
              "UID.1",
              "UID.2", 
              "UID.3", 
              "X", 
              "X.1", 
              "ScanNum.1", 
              "ScanNum.2", 
              "ScanNum.3")
cols_to_keep <- setdiff(colnames(PFT_IMG_SPEC_clean), names_drop)

df <- PFT_IMG_SPEC_clean %>% dplyr::select(cols_to_keep)

names_ignore<-colnames(df[,1:5])

print(colnames(df_mat))

# Converts the dataframe to a spectral object and normalizes it
df_mat<- df %>% dplyr::select(-names_ignore)

df_mat_names<-colnames(df_mat)
colnames(df_mat)<-gsub("X","",df_mat_names)

SpeclibObj <- spectrolab::as_spectra(df_mat)

#SpeclibObj <- spectrolab::normalize(SpeclibObj)

print("Resampling spectra every 5nm")

# Creates functions that will do the resampling every 5nm
final <- spectrolab::resample(SpeclibObj, seq(398, 999, 5)) %>%
  as.data.frame() %>%
  dplyr::select(-sample_name)

# Rename columns
#colnames(final) <- paste(colnames(final), "5nm", sep = "_")

# Combines all the dataframes created into one df

# Creates numeric vector of wavelengths
namescolumn <- metaRemove(final) %>%
  colnames() %>%
  as.numeric()

namesColumn<-colnames(final) %>% as.numeric()

# Creates a spectralib object
spec_library <- hsdar::speclib(as.matrix(final), namesColumn)

# creates a vectror of names of all the vegitation indices
AVIRIS_VI <- hsdar::vegindex()[-58]
Headwall_VI <- hsdar::vegindex()[-c(3, 26, 27, 31, 32, 33, 35, 48, 49, 58, 60, 66, 67, 71, 82, 99, 102, 103, 104, 105)]

# Get amount of cores to use
cores <- parallel::detectCores() - 1

# prepare for parallel process
c1 <- parallel::makeCluster(cores, setup_timeout = 0.5)
doParallel::registerDoParallel(c1)


# Creates dataframe with Vegitation indices
VI_CALC <- foreach(i = 1:length(Headwall_VI), .combine = cbind, .packages = "hsdar") %dopar% {
  a <- hsdar::vegindex(spec_library, index = Headwall_VI[[i]])
}
# VI_CALC<-if(ncol(metaRemove(VI)) == 272){
#  foreach(i=1:length(Headwall_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
#    a<-hsdar::vegindex(spec_library,index=Headwall_VI[[i]])}
#
# } else {
#  foreach(i=1:length(AVIRIS_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
#    a<-hsdar::vegindex(spec_library,index=AVIRIS_VI[[i]])}
# }

# Stops cluster
parallel::stopCluster(c1)

# Converts Matrix to a datframe
VI_CALC <- as.data.frame(VI_CALC)

# Function Renames columns
names(VI_CALC) <- Headwall_VI
# if(ncol(VI_CALC) == 95){
#  names(VI_CALC)<-Headwall_VI
# } else {
#  names(VI_CALC)<-AVIRIS_VI}

# Function removes spaces and special charcters from column names
# Models will not run if these aren't removed
names(VI_CALC) <- str_remove_all(names(VI_CALC), "[[:punct:]]| ")

# Conbines VIs and Lat/long info
VI_DF <- cbind(df[,1:5], bandsRemove(final), VI_CALC)

str(VI_DF)

ignore_derivs_image<- c("UID","ScanNum","sample_name", "Species_name","Functional_group1", "species_count")


VI_DF<-VI_DF %>%
  dplyr::rename(Functional_group1 = FncGrp1, Species_name = PFT)

write.csv(VI_DF, "./Data/D_002_Image_SpecLib_Derivs.csv")
