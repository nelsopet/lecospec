library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
AK2018_spectra<-read_spectra("original_data/20180729/AK2018",
                             format="sed")

##Fix Names by removing file extensions
names(AK2018_spectra)<-gsub(".sed","",names(AK2018_spectra))
names(AK2018_spectra)<-gsub("726_000","_AK2018_",names(AK2018_spectra))
names(AK2018_spectra)<-gsub("bryoria","bryori",names(AK2018_spectra))
names(AK2018_spectra)<-gsub("clarain","claran",names(AK2018_spectra))
names(AK2018_spectra)<-gsub("plagomnium","plagmn",names(AK2018_spectra))
names(AK2018_spectra)<-gsub("salpul-sic","salsic",names(AK2018_spectra))
names(AK2018_spectra)<-gsub("sphag","sphagn",names(AK2018_spectra))
names(AK2018_spectra)<-gsub("sprbarebark","spbark",names(AK2018_spectra))

##Remove eightmileflight1 scans
AK2018_spectra<-AK2018_spectra[grep("eightmileflight1",invert=TRUE,names(AK2018_spectra))]

###Create column PFT and column area
AK2018_metadata<-as.data.frame(names(AK2018_spectra))
names(AK2018_metadata)[1]<-"ScanID"
AK2018_metadata<-AK2018_metadata%>%
  separate(col=ScanID, into=c("species","year","sample"),sep="_")%>%
  mutate(PFT=case_when(species=="bryori"~ "bryoria",
                       species=="claama"~ "claama",
                       species=="claran"~ "claran",
                       species=="eqaswl"~ "equsyl",
                       species=="evemis"~ "evemes",
                       species=="hypaus"~ "hypaus",
                       species=="parsul"~ "parsul",
                       species=="pelleu"~ "pelleu",
                       species=="pelmal"~ "pelmal",
                       species=="picmar"~ "picmar",
                       species=="plagmn"~ "plagiomnium",
                       species=="plisch"~ "plesch",
                       species=="rosase"~ "rosasc",
                       species=="salsic"~ "dead salix",
                       species=="salpul"~ "salpul",
                       species=="sphagn"~ "sphagn",
                       species=="spbark"~ "spruce bark",
                       TRUE     ~"vacvit"))%>%
unite_("ScanID",c("species","year","sample"),sep="_")

AK2018_metadata$area<-"AK2018"

##Set Metadata
meta(AK2018_spectra) = data.frame(AK2018_metadata, stringsAsFactors = FALSE)

## Trim ends and resample every 5nm,10nm,50nm and 100nm
AK2018_spectra_5nm = resample(AK2018_spectra, seq(350, 2500, 5))
AK2018_spectra_10nm = resample(AK2018_spectra, seq(350, 2500, 10))
AK2018_spectra_50nm = resample(AK2018_spectra, seq(350, 2500, 50))
AK2018_spectra_100nm = resample(AK2018_spectra, seq(350, 2500, 100))


##save spectra
saveRDS(AK2018_spectra,"processed spec/AK2018/AK2018_spectra.rds")
saveRDS(AK2018_spectra_5nm,"processed spec/AK2018/AK2018_spectra_5nm.rds")
saveRDS(AK2018_spectra_10nm,"processed spec/AK2018/AK2018_spectra_10nm.rds")
saveRDS(AK2018_spectra_50nm,"processed spec/AK2018/AK2018_spectra_50nm.rds")
saveRDS(AK2018_spectra_100nm,"processed spec/AK2018/AK2018_spectra_100nm.rds")
