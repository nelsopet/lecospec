library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
bethelLib_spectra<-read_spectra("original_data/BethelLib",
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

## Trim ends and resample every 5nm,10nm,50nm and 100nm
bethelLib_spectra_5nm = resample(bethelLib_spectra, seq(350, 2500, 5))
bethelLib_spectra_10nm = resample(bethelLib_spectra, seq(350, 2500, 10))
bethelLib_spectra_50nm = resample(bethelLib_spectra, seq(350, 2500, 50))
bethelLib_spectra_100nm = resample(bethelLib_spectra, seq(350, 2500, 100))

##save spectra
saveRDS(bethelLib_spectra,"processed spec/bethelLib/bethelLib_spectra.rds")
saveRDS(bethelLib_spectra_5nm,"processed spec/bethelLib/bethelLib_spectra_5nm.rds")
saveRDS(bethelLib_spectra_10nm,"processed spec/bethelLib/bethelLib_spectra_10nm.rds")
saveRDS(bethelLib_spectra_50nm,"processed spec/bethelLib/bethelLib_spectra_50nm.rds")
saveRDS(bethelLib_spectra_100nm,"processed spec/bethelLib/bethelLib_spectra_100nm.rds")
