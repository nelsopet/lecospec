library(spectrolab)
library(tidyverse)

####Read in data as spectra
brooksLib_spectra<-read_spectra("/Alaska_Spectral_Library/data/BrooksLib",
                                format="sed")

##Remove eightmileflight1 scans
brooksLib_spectra<-brooksLib_spectra[grep("orangeporpidia",invert=TRUE,names(brooksLib_spectra))]


#######Fix Names
names(brooksLib_spectra)<-gsub(".sed","",names(brooksLib_spectra))
names(brooksLib_spectra)<-gsub("_00","_Brooks",names(brooksLib_spectra))
names(brooksLib_spectra)<-gsub("2_","_",names(brooksLib_spectra))
names(brooksLib_spectra)<-gsub("betann","betnan",names(brooksLib_spectra))

###Create Metadata
brooksLib_metadata<-as.data.frame(names(brooksLib_spectra))
names(brooksLib_metadata)[1]<-"ScanID"

###Create column PFT and column area
brooksLib_metadata<-brooksLib_metadata%>%mutate(PFT= sub("_.*", "",brooksLib_metadata$ScanID))
brooksLib_metadata$area<- "Brooks_Range"

####ensure all scans are different
brooksLib_metadata<-brooksLib_metadata %>%
  group_by(PFT, area) %>%
  mutate(
    ScanID = as.character(ScanID),
    ScanID = as.character(paste0(substr(ScanID, 1, nchar(ScanID) - 1), row_number())))

##Set metadata
meta(brooksLib_spectra) = data.frame(brooksLib_metadata, stringsAsFactors = FALSE)

## Smooth spectra
brooksLib_spectra_smooth = smooth(brooksLib_spectra)

## Trim ends and resample every 5nm
brooksLib_spectra_smooth_5nm = resample(brooksLib_spectra_smooth, seq(350, 2500, 5))
brooksLib_spectra_smooth_10nm = resample(brooksLib_spectra_smooth, seq(350, 2500, 10))
brooksLib_spectra_smooth_50nm = resample(brooksLib_spectra_smooth, seq(350, 2500, 50))
brooksLib_spectra_smooth_100nm = resample(brooksLib_spectra_smooth, seq(350, 2500, 100))

##save spectra
saveRDS(brooksLib_spectra,"/Alaska_Spectral_Library/processed spec/BrooksLib/brooksLib_spectra.rds")
saveRDS(brooksLib_spectra_smooth,"/Alaska_Spectral_Library/processed spec/BrooksLib/brooksLib_spectra_smooth.rds")
saveRDS(brooksLib_spectra_smooth_5nm,"/Alaska_Spectral_Library/processed spec/BrooksLib/brooksLib_spectra_smooth_5nm.rds")
saveRDS(brooksLib_spectra_smooth_10nm,"/Alaska_Spectral_Library/processed spec/BrooksLib/brooksLib_spectra_smooth_10nm.rds")
saveRDS(brooksLib_spectra_smooth_50nm,"/Alaska_Spectral_Library/processed spec/BrooksLib/brooksLib_spectra_smooth_50nm.rds")
saveRDS(brooksLib_spectra_smooth_100nm,"/Alaska_Spectral_Library/processed spec/BrooksLib/brooksLib_spectra_smooth_100nm.rds")
