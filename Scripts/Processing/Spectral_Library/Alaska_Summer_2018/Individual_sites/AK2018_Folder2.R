library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
AK2018_spectra<-read_spectra("Original_data/Alaska_Summer_2018/20180729/AK2018",
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

## Smooth spectra
AK2018_spectra_smooth = smooth(AK2018_spectra)

## Trim ends and resample every 005nm,010nm,050nm and 100nm
AK2018_spectra_005nm = resample(AK2018_spectra, seq(350, 2500, 5))
AK2018_spectra_010nm = resample(AK2018_spectra, seq(350, 2500, 10))
AK2018_spectra_050nm = resample(AK2018_spectra, seq(350, 2500, 50))
AK2018_spectra_100nm = resample(AK2018_spectra, seq(350, 2500, 100))

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Smooth_Spec)
AK2018_spectra_smooth_005nm   = resample(AK2018_spectra_smooth, seq(350, 2500, 5))
AK2018_spectra_smooth_010nm  = resample(AK2018_spectra_smooth, seq(350, 2500, 10))
AK2018_spectra_smooth_050nm  = resample(AK2018_spectra_smooth, seq(350, 2500, 50))
AK2018_spectra_smooth_100nm = resample(AK2018_spectra_smooth, seq(350, 2500, 100))


##save spectran(raw)
saveRDS(AK2018_spectra      ,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra.rds")
saveRDS(AK2018_spectra_005nm,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_005nm.rds")
saveRDS(AK2018_spectra_010nm,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_010nm.rds")
saveRDS(AK2018_spectra_050nm,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_050nm.rds")
saveRDS(AK2018_spectra_100nm,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_100nm.rds")

##save spectran(Smooth)
saveRDS(AK2018_spectra_smooth      ,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_smooth.rds")
saveRDS(AK2018_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_smooth_005nm.rds")
saveRDS(AK2018_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_smooth_010nm.rds")
saveRDS(AK2018_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_smooth_050nm.rds")
saveRDS(AK2018_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_smooth_100nm.rds")