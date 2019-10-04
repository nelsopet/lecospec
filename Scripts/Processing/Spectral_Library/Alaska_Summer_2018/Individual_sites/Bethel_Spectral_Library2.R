library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
bethelLib_spectra<-read_spectra("Original_data/Alaska_Summer_2018/BethelLib",
                             format="sed")
##Remove salex scans
bethelLib_spectra<-bethelLib_spectra[grep("salex",invert=TRUE,names(bethelLib_spectra))]

#######Fix Names with 8 or more scans
names(bethelLib_spectra)<-gsub(".sed","",names(bethelLib_spectra))
names(bethelLib_spectra)<-gsub("clip_00","_Beth",names(bethelLib_spectra))
names(bethelLib_spectra)<-gsub("000","Beth0",names(bethelLib_spectra))
names(bethelLib_spectra)<-gsub("[0-9]+l","",names(bethelLib_spectra))
names(bethelLib_spectra)<-gsub("[0-9]+_","_",names(bethelLib_spectra))
names(bethelLib_spectra)<-gsub("fairypuke","icmeri",names(bethelLib_spectra))
names(bethelLib_spectra)<-gsub("iceericprobe","icmeri",names(bethelLib_spectra))

###Create Metadata
bethelLib_metadata<-as.data.frame(names(bethelLib_spectra))
names(bethelLib_metadata)[1]<-"ScanID"
bethelLib_metadata<-bethelLib_metadata%>%separate(col=ScanID, into=c("species","sample"),sep="_")
bethelLib_metadata$species<-substr(bethelLib_metadata$species,start = 1,stop = 6)
bethelLib_metadata<-unite_(bethelLib_metadata,"ScanID",c("species","sample"),sep="_")

###Create column PFT and column area
bethelLib_metadata<-bethelLib_metadata%>%mutate(PFT=substr(bethelLib_metadata$ScanID,start = 1,stop = 6))
bethelLib_metadata$area<- "Bethel"

####ensure all scans are different
bethelLib_metadata<-bethelLib_metadata %>%
  group_by(PFT, area) %>%
  mutate(
    ScanID = as.character(ScanID),
    ScanID = as.character(paste0(substr(ScanID, 1, nchar(ScanID) - 1), row_number())))

##Set metadata
meta(bethelLib_spectra) = data.frame(bethelLib_metadata, stringsAsFactors = FALSE)

## Smooth spectra
bethelLib_spectra_smooth = smooth(bethelLib_spectra)

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Raw)
bethelLib_spectra_005nm = resample(bethelLib_spectra, seq(350, 2500, 5))
bethelLib_spectra_010nm = resample(bethelLib_spectra, seq(350, 2500, 10))
bethelLib_spectra_050nm = resample(bethelLib_spectra, seq(350, 2500, 50))
bethelLib_spectra_100nm = resample(bethelLib_spectra, seq(350, 2500, 100))

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Smooth)
bethelLib_spectra_smooth_005nm = resample(bethelLib_spectra_smooth, seq(350, 2500, 5))
bethelLib_spectra_smooth_010nm = resample(bethelLib_spectra_smooth, seq(350, 2500, 10))
bethelLib_spectra_smooth_050nm = resample(bethelLib_spectra_smooth, seq(350, 2500, 50))
bethelLib_spectra_smooth_100nm = resample(bethelLib_spectra_smooth, seq(350, 2500, 100))

##save spectra (Raw)
saveRDS(bethelLib_spectra      ,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra.rds")
saveRDS(bethelLib_spectra_005nm,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_005nm.rds")
saveRDS(bethelLib_spectra_010nm,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_010nm.rds")
saveRDS(bethelLib_spectra_050nm,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_050nm.rds")
saveRDS(bethelLib_spectra_100nm,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_100nm.rds")

##save spectra (Smooth)
saveRDS(bethelLib_spectra_smooth      ,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_smooth.rds")
saveRDS(bethelLib_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_smooth_005nm.rds")
saveRDS(bethelLib_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_smooth_010nm.rds")
saveRDS(bethelLib_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_smooth_050nm.rds")
saveRDS(bethelLib_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_smooth_100nm.rds")

