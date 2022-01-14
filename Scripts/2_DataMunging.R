# -------------------------------------------- Script 2 ------------------------------------------------------------- #
# The script fix names and saves the spectra collected at each location in alaska
# Packages to install 
library(spectrolab)
library(tidyverse)

# ------------------------------------------- 12mile ---------------------------------------------------------------- #
####Read in data as spectra (all scans collected at this location)
TwelveMile_spectra<-read_spectra("./Data/SpectraByLocation/12_Mile/original_samples", format="sed", extract_metadata = TRUE)

##Plots all spectral objects withing this location (scans of individuals)
##plot_interactive(TwelveMile_spectra)

##Fix Names 
names(TwelveMile_spectra)<-gsub(".sed","",names(TwelveMile_spectra))

###Create Metadata
TwelveMile_metadata<-as.data.frame(names(TwelveMile_spectra))
names(TwelveMile_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
TwelveMile_metadata<-TwelveMile_metadata%>%mutate(Code_name=substr(TwelveMile_metadata$ScanID,start = 1,stop = 6))
TwelveMile_metadata$Area<- "12mile"

#Grab metadata from instrument
TwelveMile_metadata_instrument<-meta(TwelveMile_spectra)

#Combine metadata generated in this script with instrument metadata
TwelveMile_metadata<-cbind(TwelveMile_metadata,TwelveMile_metadata_instrument)

##Set metadata
meta(TwelveMile_spectra) = data.frame(TwelveMile_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(TwelveMile_spectra      ,"./Output/A_001_SC2_TwelveMile_spectra.rds")

# ------------------------------------------- AK2018 ---------------------------------------------------------------- #

####Read in data as spectra
AK2018_spectra<-read_spectra("./Data/SpectraByLocation/20180729/AK2018", format="sed", extract_metadata = TRUE)

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

###Create column Code_name and column Area
AK2018_metadata<-as.data.frame(names(AK2018_spectra))
names(AK2018_metadata)[1]<-"ScanID"
AK2018_metadata<-AK2018_metadata%>%
  separate(col=ScanID, into=c("species","year","sample"),sep="_")%>%
  mutate(Code_name=case_when(species=="bryori"~ "bryoria",
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

AK2018_metadata$Area<-"AK2018"
#meta(AK2018_spectra)[,"Area"]<-"AK2018" 

#Grab metadata from instrument
AK2018_metadata_instrument<-meta(AK2018_spectra)

#Combine metadata generated in this script with instrument metadata
AK2018_metadata<-cbind(AK2018_metadata,AK2018_metadata_instrument)

##Set Metadata
meta(AK2018_spectra) = data.frame(AK2018_metadata, stringsAsFactors = FALSE)

##save spectran(raw)
saveRDS(AK2018_spectra      ,"./Output/A_002_SC2_AK2018_spectra.rds"      )
# ------------------------------------------- Bethel ----------------------------------------------------- #
####Read in data as spectra
bethelLib_spectra<-read_spectra("./Data/SpectraByLocation/BethelLib", format="sed", extract_metadata = TRUE)
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

###Create column Code_name and column Area
bethelLib_metadata<-bethelLib_metadata%>%mutate(Code_name=substr(bethelLib_metadata$ScanID,start = 1,stop = 6))
bethelLib_metadata$Area<- "Bethel"

####ensure all scans are different
bethelLib_metadata<-bethelLib_metadata %>%
  group_by(Code_name, Area) %>%
  mutate(
    ScanID = as.character(ScanID),
    ScanID = as.character(paste0(substr(ScanID, 1, nchar(ScanID) - 1), row_number())))

#Grab metadata from instrument
bethelLib_metadata_instrument<-meta(bethelLib_spectra)

#Combine metadata generated in this script with instrument metadata
bethelLib_metadata<-cbind(bethelLib_metadata,bethelLib_metadata_instrument)

##Set metadata
meta(bethelLib_spectra) = data.frame(bethelLib_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(bethelLib_spectra      ,"./Output/A_003_SC2_bethelLib_spectra.rds")
# ------------------------------------------- BigTrail --------------------------------------------------- #


####Read in data as spectra
Big_Trail_Lake_spectra<-read_spectra("./Data/SpectraByLocation/Big_Trail_Lake/original_samples", format="sed", extract_metadata = TRUE)

##Fix Names 
names(Big_Trail_Lake_spectra)<-gsub(".sed","",names(Big_Trail_Lake_spectra))

###Create Metadata
Big_Trail_Lake_metadata<-as.data.frame(names(Big_Trail_Lake_spectra))
names(Big_Trail_Lake_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
Big_Trail_Lake_metadata<-Big_Trail_Lake_metadata%>%mutate(Code_name=substr(Big_Trail_Lake_metadata$ScanID,start = 1,stop = 6))
Big_Trail_Lake_metadata$Area<- "Big Trail"

#Grab metadata from instrument
Big_Trail_Lake_metadata_instrument<-meta(Big_Trail_Lake_spectra)

#Combine metadata generated in this script with instrument metadata
Big_Trail_Lake_metadata<-cbind(Big_Trail_Lake_metadata,Big_Trail_Lake_metadata_instrument)

##Set metadata
meta(Big_Trail_Lake_spectra) = data.frame(Big_Trail_Lake_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Big_Trail_Lake_spectra      ,"./Output/A_004_SC2_Big_Trail_Lake_spectra.rds"      )
# ------------------------------------------- Brooks ----------------------------------------------------- #
####Read in data as spectra
brooksLib_spectra<-read_spectra("./Data/SpectraByLocation/BrooksLib", format="sed", extract_metadata = TRUE)

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

###Create column Code_name and column Area
brooksLib_metadata<-brooksLib_metadata%>%mutate(Code_name= sub("_.*", "",brooksLib_metadata$ScanID))
brooksLib_metadata$Area<- "Brooks_Range"

####ensure all scans are different
brooksLib_metadata<-brooksLib_metadata %>%
  group_by(Code_name, Area) %>%
  mutate(
    ScanID = as.character(ScanID),
    ScanID = as.character(paste0(substr(ScanID, 1, nchar(ScanID) - 1), row_number())))

#Grab metadata from instrument
brooksLib_metadata_instrument<-meta(brooksLib_spectra)

#Combine metadata generated in this script with instrument metadata
brooksLib_metadata<-cbind(brooksLib_metadata,brooksLib_metadata_instrument)

##Set metadata
meta(brooksLib_spectra) = data.frame(brooksLib_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(brooksLib_spectra      ,"./Output/A_005_SC2_brooksLib_spectra.rds")
# ------------------------------------------- EagleSummit ------------------------------------------------ #
####Read in data as spectra
Eagle_summit_spectra<-read_spectra("./Data/SpectraByLocation/Eagle_summit/original_samples", format="sed", extract_metadata = TRUE)

##Fix Names 
names(Eagle_summit_spectra)<-gsub(".sed","",names(Eagle_summit_spectra))

###Create Metadata
Eagle_summit_metadata<-as.data.frame(names(Eagle_summit_spectra))
names(Eagle_summit_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
Eagle_summit_metadata<-Eagle_summit_metadata%>%mutate(Code_name=substr(Eagle_summit_metadata$ScanID,start = 1,stop = 6))
Eagle_summit_metadata$Area<- "Big Trail"

#Grab metadata from instrument
Eagle_summit_metadata_instrument<-meta(Eagle_summit_spectra)

#Combine metadata generated in this script with instrument metadata
Eagle_summit_metadata<-cbind(Eagle_summit_metadata,Eagle_summit_metadata_instrument)

##Set metadata
meta(Eagle_summit_spectra) = data.frame(Eagle_summit_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Eagle_summit_spectra      ,"./Output/A_006_SC2_Eagle_summit_spectra.rds"      )
# ------------------------------------------- MurphyDome1 ------------------------------------------------ #
####Read in data as spectra
Murph_lib_spectra<-read_spectra("./Data/SpectraByLocation/20180729/M_Dome", format="sed", extract_metadata = TRUE)

##Remove eightmileflight1 scans
Murph_lib_spectra<-Murph_lib_spectra[grep("M_Dome_T1",invert=TRUE,names(Murph_lib_spectra))]


#######Fix Names
names(Murph_lib_spectra)<-gsub(".sed","",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("_00","_Murph_",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("arctoparmelia","arctop",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("bareMin","br",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("bare","br",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("loisleuria","loisla",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("DRY","",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("orangePorpidia","Prpdia",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("polytrichum","polytr",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("rhizocarponGREY","rhzGRY",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("sal_wooly","salwol",names(Murph_lib_spectra))
names(Murph_lib_spectra)<-gsub("tofeldia","tfldia",names(Murph_lib_spectra))

###Create Metadata
Murph_lib_metadata<-as.data.frame(names(Murph_lib_spectra))
names(Murph_lib_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
Murph_lib_metadata<-Murph_lib_metadata%>%mutate(Code_name= substr(Murph_lib_metadata$ScanID,start=1,stop=6))
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="brSoil"] <- "bare_soil"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="brrock"] <- "bare rock"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="Prpdia"] <- "orange_Porpidia"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="polytr"] <- "polytrichum"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="rhzGRY"] <- "grey_rhizocarpon"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="salwol"] <- "wooly_salix"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="tfldia"] <- "toefeldia"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="rhwrug"] <-"rhyrug"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="kwarts"] <-"quartz"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="loisla"] <-"loipro"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="melhap"] <-"melhep"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="perdac"] <-"pedrac"
Murph_lib_metadata$Code_name[Murph_lib_metadata$Code_name=="petfer"] <-"petfri"
Murph_lib_metadata$Area<- "Murphy"

#Grab metadata from instrument
Murph_lib_metadata_instrument<-meta(Murph_lib_spectra)

#Combine metadata generated in this script with instrument metadata
Murph_lib_metadata<-cbind(Murph_lib_metadata,Murph_lib_metadata_instrument)

###Set metadata
meta(Murph_lib_spectra) = data.frame(Murph_lib_metadata, stringsAsFactors = FALSE)

###save spectra (Raw)
saveRDS(Murph_lib_spectra      ,"./Output/A_007_SC2_Murph_lib_spectra.rds")
# ------------------------------------------- Murphydome2 ------------------------------------------------ #
Murph2_spectra<-read_spectra("./Data/SpectraByLocation/20180729/murphydomeday2", format="sed", extract_metadata = TRUE)

##Remove polstricum scans
Murph2_spectra<-Murph2_spectra[grep("polstricum",invert=TRUE,names(Murph2_spectra))]
Murph2_spectra<-Murph2_spectra[grep("poljplesch",invert=TRUE,names(Murph2_spectra))]
Murph2_spectra<-Murph2_spectra[grep("russulared",invert=TRUE,names(Murph2_spectra))]

#######Fix Names
names(Murph2_spectra)<-gsub(".sed","",names(Murph2_spectra))
names(Murph2_spectra)<-gsub("_00","_Murp2_",names(Murph2_spectra))
names(Murph2_spectra)<-gsub("wet","_wet",names(Murph2_spectra))
names(Murph2_spectra)<-gsub("trapeliopsisgranulosa","tragra",names(Murph2_spectra))

##Create metadata
Murph2_spectra_metadata<-as.data.frame(names(Murph2_spectra))
names(Murph2_spectra_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
Murph2_spectra_metadata<-Murph2_spectra_metadata%>%mutate(Code_name= substr(Murph2_spectra_metadata$ScanID,start=1,stop = 6))
Murph2_spectra_metadata$Code_name[Murph2_spectra_metadata$Code_name=="bryori"] <- "bryoria"
Murph2_spectra_metadata$Code_name[Murph2_spectra_metadata$Code_name=="dicran"] <- "dicranum"
Murph2_spectra_metadata$Code_name[Murph2_spectra_metadata$Code_name=="melane"] <- "melanelia"
Murph2_spectra_metadata$Area<- "Murphy"

#Grab metadata from instrument
Murph2_metadata_instrument<-meta(Murph2_spectra)

#Combine metadata generated in this script with instrument metadata
Murph2_metadata<-cbind(Murph2_metadata,Murph2_metadata_instrument)

###Set metadata
meta(Murph2_spectra) = data.frame(Murph2_spectra_metadata, stringsAsFactors = FALSE)

###save spectra (Raw)
saveRDS(Murph2_spectra      ,"./Output/A_008_SC2_Murph2_spectra.rds")
# ------------------------------------------- MurphydomeA ------------------------------------------------ #

####Read in data as spectra
Murphy_domeA_spectra<-read_spectra("./Data/SpectraByLocation/Murphy_domeA/original_samples", format="sed", extract_metadata = TRUE)

##Fix Names 
names(Murphy_domeA_spectra)<-gsub(".sed","",names(Murphy_domeA_spectra))

###Create Metadata
Murphy_domeA_metadata<-as.data.frame(names(Murphy_domeA_spectra))
names(Murphy_domeA_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
Murphy_domeA_metadata<-Murphy_domeA_metadata%>%mutate(Code_name=substr(Murphy_domeA_metadata$ScanID,start = 1,stop = 6))
Murphy_domeA_metadata$Area<- "Murphy A"

#Grab metadata from instrument
Murphy_domeA_metadata_instrument<-meta(Murphy_domeA_spectra)

#Combine metadata generated in this script with instrument metadata
Murphy_domeA_metadata<-cbind(Murphy_domeA_metadata,Murphy_domeA_metadata_instrument)

##Set metadata
meta(Murphy_domeA_spectra) = data.frame(Murphy_domeA_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Murphy_domeA_spectra      ,"./Output/A_009_SC2_Murphy_domeA_spectra.rds")
# ------------------------------------------- MurphyDomeB ------------------------------------------------ #
####Read in data as spectra
Murphy_domeB_spectra<-read_spectra("./Data/SpectraByLocation/Murphy_domeB/original_samples", format="sed", extract_metadata = TRUE)

##Fix Names 
names(Murphy_domeB_spectra)<-gsub(".sed","",names(Murphy_domeB_spectra))

###Create Metadata
Murphy_domeB_metadata<-as.data.frame(names(Murphy_domeB_spectra))
names(Murphy_domeB_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
Murphy_domeB_metadata<-Murphy_domeB_metadata%>%mutate(Code_name=substr(Murphy_domeB_metadata$ScanID,start = 1,stop = 6))
Murphy_domeB_metadata$Area<- "Murphy B"

#Grab metadata from instrument
Murphy_domeB_metadata_instrument<-meta(Murphy_domeB_spectra)

#Combine metadata generated in this script with instrument metadata
Murphy_domeB_metadata<-cbind(Murphy_domeB_metadata,Murphy_domeB_metadata_instrument)

##Set metadata
meta(Murphy_domeB_spectra) = data.frame(Murphy_domeB_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Murphy_domeB_spectra, "./Output/A_010_SC2_Murphy_domeB_spectra.rds")
# ------------------------------------------- WickershamDomeB -------------------------------------------- #
####Read in data as spectra
Wickersham_domeA_spectra <- read_spectra("./Data/SpectraByLocation/Wickersham_domeA/original_samples", format="sed", extract_metadata = TRUE)

##Fix Names 
names(Wickersham_domeA_spectra)<-gsub(".sed","",names(Wickersham_domeA_spectra))

###Create Metadata
Wickersham_domeA_metadata<-as.data.frame(names(Wickersham_domeA_spectra))
names(Wickersham_domeA_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
Wickersham_domeA_metadata<-Wickersham_domeA_metadata%>%mutate(Code_name=substr(Wickersham_domeA_metadata$ScanID,start = 1,stop = 6))
Wickersham_domeA_metadata$Area<- "Wickersham B"

#Grab metadata from instrument
Wickersham_domeA_metadata_instrument<-meta(Wickersham_domeA_spectra)

#Combine metadata generated in this script with instrument metadata
Wickersham_domeA_metadata<-cbind(Wickersham_domeA_metadata,Wickersham_domeA_metadata_instrument)

##Set metadata
meta(Wickersham_domeA_spectra) = data.frame(Wickersham_domeA_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Wickersham_domeA_spectra      ,"./Output/A_011_SC2_Wickersham_domeA_spectra.rds")
# ------------------------------------------- WickershamDomeB -------------------------------------------- #
####Read in data as spectra
Wickersham_domeB_spectra<-read_spectra("./Data/SpectraByLocation/Wickersham_domeB/original_samples", format="sed", extract_metadata = TRUE)

##Fix Names 
names(Wickersham_domeB_spectra)<-gsub(".sed","",names(Wickersham_domeB_spectra))

###Create Metadata
Wickersham_domeB_metadata<-as.data.frame(names(Wickersham_domeB_spectra))
names(Wickersham_domeB_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
Wickersham_domeB_metadata<-Wickersham_domeB_metadata%>%mutate(Code_name=substr(Wickersham_domeB_metadata$ScanID,start = 1,stop = 6))
Wickersham_domeB_metadata$Area<- "Wickersham B"

#Grab metadata from instrument
Wickersham_domeB_metadata_instrument<-meta(Wickersham_domeB_spectra)

#Combine metadata generated in this script with instrument metadata
Wickersham_domeB_metadata<-cbind(Wickersham_domeB_metadata,Wickersham_domeB_metadata_instrument)

##Set metadata
meta(Wickersham_domeB_spectra) = data.frame(Wickersham_domeB_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Wickersham_domeB_spectra      ,"./Output/A_012_SC2_Wickersham_domeB_spectra.rds")
# ------------------------------------------- Yukon ----------------------------------------------------- #
####Read in data as spectra
yKDeltLib_spectra<-read_spectra("./Data/SpectraByLocation/YKDeltLib", format="sed", extract_metadata = TRUE)

##Remove eightmileflight1 scans
yKDeltLib_spectra<-yKDeltLib_spectra[grep("site21clasticprobe",invert=TRUE,names(yKDeltLib_spectra))]


#######Fix Names
names(yKDeltLib_spectra)<-gsub(".sed","",names(yKDeltLib_spectra))
names(yKDeltLib_spectra)<-gsub("lclip+[0-9]","",names(yKDeltLib_spectra))
names(yKDeltLib_spectra)<-gsub("clip+[0-9]","",names(yKDeltLib_spectra))
names(yKDeltLib_spectra)<-gsub("lclip","",names(yKDeltLib_spectra))
names(yKDeltLib_spectra)<-gsub("clip","",names(yKDeltLib_spectra))
names(yKDeltLib_spectra)<-gsub("site+[0-9]+[0-9]","",names(yKDeltLib_spectra))
names(yKDeltLib_spectra)<-gsub("lcli2","",names(yKDeltLib_spectra))
names(yKDeltLib_spectra)<-gsub("cli2","",names(yKDeltLib_spectra))
names(yKDeltLib_spectra)<-gsub("probe","",names(yKDeltLib_spectra))

##Create metadata
yKDeltLib_metadata<-as.data.frame(names(yKDeltLib_spectra))
names(yKDeltLib_metadata)[1]<-"ScanID"

###Create column Code_name and column Area
yKDeltLib_metadata<-yKDeltLib_metadata%>%mutate(Code_name= substr(yKDeltLib_metadata$ScanID,start=1,stop = 6))
names(yKDeltLib_metadata)<-gsub("Salova","salova",names(yKDeltLib_metadata))
yKDeltLib_metadata$Area<- "Yukon_Delta"

####ensure all scans are different
yKDeltLib_metadata<-yKDeltLib_metadata %>%
  group_by(Code_name, Area) %>%
  mutate(
    ScanID = as.character(ScanID),
    ScanID = as.character(paste0(substr(ScanID, 1, nchar(ScanID) - 1), row_number())))


#Grab metadata from instrument
yKDeltLib_metadata_instrument<-meta(yKDeltLib_spectra)

#Combine metadata generated in this script with instrument metadata
yKDeltLib_metadata<-cbind(yKDeltLib_metadata,yKDeltLib_metadata_instrument)


###Set metadata
meta(yKDeltLib_spectra) = data.frame(yKDeltLib_metadata, stringsAsFactors = FALSE)

###save spectra (Raw)
saveRDS(yKDeltLib_spectra      ,"./Output/A_013_SC2_yKDeltLib_spectra.rds")


