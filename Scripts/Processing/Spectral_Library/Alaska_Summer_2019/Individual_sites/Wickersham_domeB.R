########################################Wickersham dome B#############################
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Wickersham_domeB_spectra<-read_spectra("Original_data/Alaska_Summer_2019/Wickersham_domeB/original_samples",
                                       format="sed")

##Fix Names 
names(Wickersham_domeB_spectra)<-gsub(".sed","",names(Wickersham_domeB_spectra))

###Create Metadata
Wickersham_domeB_metadata<-as.data.frame(names(Wickersham_domeB_spectra))
names(Wickersham_domeB_metadata)[1]<-"ScanID"

###Create column PFT and column area
Wickersham_domeB_metadata<-Wickersham_domeB_metadata%>%mutate(PFT=substr(Wickersham_domeB_metadata$ScanID,start = 1,stop = 6))
Wickersham_domeB_metadata$area<- "Wickersham B"

##Set metadata
meta(Wickersham_domeB_spectra) = data.frame(Wickersham_domeB_metadata, stringsAsFactors = FALSE)

## Smooth spectra
Wickersham_domeB_spectra_smooth = smooth(Wickersham_domeB_spectra)

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Raw)
Wickersham_domeB_spectra_005nm = resample(Wickersham_domeB_spectra, seq(350, 2500, 5))
Wickersham_domeB_spectra_010nm = resample(Wickersham_domeB_spectra, seq(350, 2500, 10))
Wickersham_domeB_spectra_050nm = resample(Wickersham_domeB_spectra, seq(350, 2500, 50))
Wickersham_domeB_spectra_100nm = resample(Wickersham_domeB_spectra, seq(350, 2500, 100))

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Smooth)
Wickersham_domeB_spectra_smooth_005nm = resample(Wickersham_domeB_spectra_smooth, seq(350, 2500, 5))
Wickersham_domeB_spectra_smooth_010nm = resample(Wickersham_domeB_spectra_smooth, seq(350, 2500, 10))
Wickersham_domeB_spectra_smooth_050nm = resample(Wickersham_domeB_spectra_smooth, seq(350, 2500, 50))
Wickersham_domeB_spectra_smooth_100nm = resample(Wickersham_domeB_spectra_smooth, seq(350, 2500, 100))

##save spectra (Raw)
saveRDS(Wickersham_domeB_spectra      ,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra.rds")
saveRDS(Wickersham_domeB_spectra_005nm,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_005nm.rds")
saveRDS(Wickersham_domeB_spectra_010nm,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_010nm.rds")
saveRDS(Wickersham_domeB_spectra_050nm,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_050nm.rds")
saveRDS(Wickersham_domeB_spectra_100nm,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_100nm.rds")

##save spectra (Smooth)
saveRDS(Wickersham_domeB_spectra_smooth      ,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_smooth.rds")
saveRDS(Wickersham_domeB_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_smooth_005nm.rds")
saveRDS(Wickersham_domeB_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_smooth_010nm.rds")
saveRDS(Wickersham_domeB_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_smooth_050nm.rds")
saveRDS(Wickersham_domeB_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_smooth_100nm.rds")