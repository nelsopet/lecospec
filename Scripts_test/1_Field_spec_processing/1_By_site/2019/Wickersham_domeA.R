####################################Wickersham dome A#################################
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Wickersham_domeA_spectra<-read_spectra("Original_data/Field_spec/Alaska/Wickersham_domeA/original_samples",
                           format="sed")

##Fix Names 
names(Wickersham_domeA_spectra)<-gsub(".sed","",names(Wickersham_domeA_spectra))

###Create Metadata
Wickersham_domeA_metadata<-as.data.frame(names(Wickersham_domeA_spectra))
names(Wickersham_domeA_metadata)[1]<-"ScanID"

###Create column PFT and column area
Wickersham_domeA_metadata<-Wickersham_domeA_metadata%>%mutate(PFT=substr(Wickersham_domeA_metadata$ScanID,start = 1,stop = 6))
Wickersham_domeA_metadata$area<- "Wickersham B"

##Set metadata
meta(Wickersham_domeA_spectra) = data.frame(Wickersham_domeA_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Wickersham_domeA_spectra      ,"Outputs/1_Field_spec/1_Processing/Wickersham_domeA_spectra.rds")
