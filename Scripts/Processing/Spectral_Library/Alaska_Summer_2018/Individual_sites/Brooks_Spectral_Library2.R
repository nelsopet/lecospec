library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
brooksLib_spectra<-read_spectra("Original_data/Alaska_Summer_2018/BrooksLib",
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

## Trim ends and resample every 005nm, 010nm, 050nm and 100nm (Raw)
brooksLib_spectra_005nm = resample(brooksLib_spectra, seq(350, 2500, 5))
brooksLib_spectra_010nm = resample(brooksLib_spectra, seq(350, 2500, 10))
brooksLib_spectra_050nm = resample(brooksLib_spectra, seq(350, 2500, 50))
brooksLib_spectra_100nm = resample(brooksLib_spectra, seq(350, 2500, 100))

## Trim ends and resample every 005nm, 010nm, 050nm and 100nm (Smooth)
brooksLib_spectra_smooth_005nm = resample(brooksLib_spectra_smooth, seq(350, 2500, 5))
brooksLib_spectra_smooth_010nm = resample(brooksLib_spectra_smooth, seq(350, 2500, 10))
brooksLib_spectra_smooth_050nm = resample(brooksLib_spectra_smooth, seq(350, 2500, 50))
brooksLib_spectra_smooth_100nm = resample(brooksLib_spectra_smooth, seq(350, 2500, 100))

##save spectra (Raw)
saveRDS(brooksLib_spectra      ,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra.rds")
saveRDS(brooksLib_spectra_005nm,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_005nm.rds")
saveRDS(brooksLib_spectra_010nm,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_010nm.rds")
saveRDS(brooksLib_spectra_050nm,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_050nm.rds")
saveRDS(brooksLib_spectra_100nm,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_100nm.rds")

##save spectra (Smooth)
saveRDS(brooksLib_spectra_smooth      ,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_smooth.rds")
saveRDS(brooksLib_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_smooth_005nm.rds")
saveRDS(brooksLib_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_smooth_010nm.rds")
saveRDS(brooksLib_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_smooth_050nm.rds")
saveRDS(brooksLib_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2018/BrooksLib/brooksLib_spectra_smooth_100nm.rds")
