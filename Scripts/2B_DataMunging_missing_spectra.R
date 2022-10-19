# -------------------------------------------- Script 2B ------------------------------------------------------------- #
# The script fix names and saves the spectra collected at each location in alaska that were not included in the original
# spectra library. 
# Packages to install
library(spectrolab)
library(tidyverse)

#Species groupings
Species_groups <- read.csv("./Data/SpeciesTable_20220125.csv", encoding = "UTF-8")

##Custom spectra reading
#Make a list directories
dirs_spec_new<-list.dirs("./Data/SpectraByLocation/20180729/")
dirs_spec_new<-c(
 "./Data/SpectraByLocation/20180729/BirchLakeFlight1",
 "./Data/SpectraByLocation/20180729/bouchardsoilscans",
 "./Data/SpectraByLocation/20180729/chatnika_F3",                         
 "./Data/SpectraByLocation/20180729/eightmile",                           
 "./Data/SpectraByLocation/20180729/eightmileflight2",                    
 "./Data/SpectraByLocation/20180729/juliasite",                           
 "./Data/SpectraByLocation/20180729/littlelakeflight1",                   
 "./Data/SpectraByLocation/20180729/M_Dome",                              
 "./Data/SpectraByLocation/20180729/murphydomeday2",                      
 "./Data/SpectraByLocation/20180729/spinachcircle",                       
 "./Data/SpectraByLocation/20180729/valtrd_f2") 

dirs_spec_new_meta<-c(
  "BirchLake",
  "Bonanza Creek LTER",
  "Caribou Poker LTER",
  "Eight Mile",
  "Eight Mile",
  "Eight Mile Schuur Transect",
  "Little Lake",
  "Murphy Dome",
  "Murphy Dome",
  "Fairbanks Area Spinach Circle",
  "Fairbanks Area Vault Road"
) #%>% as.data.frame() %>%
  #dplyr::rename(Area = '.')

#Base input list of spectra by location
dir_spec_list <- lapply(1:length(dirs_spec_new), function(x) {list.files(dirs_spec_new[x], pattern = ".sed", full.names = T)})
dir_spec_names <- lapply(1:length(dirs_spec_new), function(x) {list.files(dirs_spec_new[x], pattern = ".sed", full.names = F)})
dir_spec_rds<-lapply(1:length(dirs_spec_new), function(x) read_spectra(dirs_spec_new[x], format = "sed", extract_metadata = TRUE))

#Unit test: read in data without band issues

#Custom read spectra function for the missing spectra
read_spectra_fix_missing_spec= function(x)
{
  tst<-read_delim(tst_speclib_onesite[x], skip =26, show_col_types = FALSE) %>% 
    as.data.frame() %>% 
    dplyr::rename(Refl = `Reflect. %`) %>% 
    mutate(Wvl = as.numeric(Wvl),
           Refl = as.numeric(Refl)) %>% 
    pivot_wider(values_from = Refl, names_from = Wvl) %>%
    as_spectra()
  names(tst)<-tst_speclib_onesite[x]
  return(tst)
}


#Unit test for all the spectra at one site
tst_speclib_onesite<-dir_spec_list[[8]]
dir_spec_fix<-lapply(1:length(tst_speclib_onesite), read_spectra_fix_missing_spec)
dir_spec_fix<-Reduce(spectrolab::combine, dir_spec_fix)
meta(dir_spec_fix)<-dirs_spec_new_meta[8] %>% as.data.frame() %>% rename(Area = '.')
names(dir_spec_fix)<-dir_spec_names[8]

spec_missing_all<-
  lapply(1:length(dir_spec_list), 
       function(y){
          tst_speclib_onesite<-dir_spec_list[[y]]
          tst_spec_names <- list.files("./Data/SpectraByLocation/12_Mile/original_samples", pattern = ".sed", full.names = F)
          dir_spec_fix<-lapply(1:length(tst_speclib_onesite), read_spectra_fix_missing_spec)
          dir_spec_fix<-Reduce(spectrolab::combine, dir_spec_fix)
          meta(dir_spec_fix)<-dirs_spec_new_meta[y] %>% as.data.frame() %>% rename(Area = '.')
          names(dir_spec_fix)<-dir_spec_names[y]
          print(dirs_spec_new_meta[y])
          summary(dir_spec_fix)
          return(dir_spec_fix)
       })


#Combine all missing spectra into one speclib
spec_missing_all<-Reduce(spectrolab::combine, spec_missing_all)

## Fix Names
names(spec_missing_all) <- gsub(".sed", "", names(spec_missing_all))

### Create Metadata
spec_missing_all_metadata <- as.data.frame(names(spec_missing_all))
names(spec_missing_all_metadata)[1] <- "ScanID"

### Create column Code_name and column Area
#TwelveMile_metadata <- 
#  TwelveMile_metadata %>% 
#  mutate(Code_name = substr(TwelveMile_metadata$ScanID, start = 1, stop = 6))
#TwelveMile_metadata$Area <- "12mile"

# Grab metadata from instrument
spec_missing_all_metadata_instrument <- lapply(1:length(dir_spec_rds), function(x) meta(dir_spec_rds[[x]])) 
spec_missing_all_metadata_instrument<-Reduce(rbind,spec_missing_all_metadata_instrument)

# Combine metadata generated in this script with instrument metadata
spec_missing_all_metadata <- cbind(spec_missing_all_metadata, spec_missing_all_metadata_instrument)
spec_missing_all_metadata$Area<-meta(spec_missing_all)
## Set metadata
meta(spec_missing_all) <- data.frame(spec_missing_all_metadata, stringsAsFactors = FALSE)
dim(meta(spec_missing_all))

## save spectra (Raw)
range(spec_missing_all)

#Change scan names that have more that one delimter

meta_clean<-meta(spec_missing_all)
meta_clean$ScanID <- gsub("chatnika_F3", "chatnikaF3", meta_clean$ScanID)
meta_clean$ScanID <- gsub("M_Dome_T1", "MDomeT1", meta_clean$ScanID)
meta_clean$ScanID <- gsub("sal_wooly", "salixwooly", meta_clean$ScanID)
meta_clean$ScanID <- gsub("birchlake_carex", "carex", meta_clean$ScanID)
meta_clean$ScanID <- gsub("valtrd_f2", "valtrdf2", meta_clean$ScanID)

#Omit scans that aren't pure scans of a single surface

CodeNames_omit<-c("1676083"
                  ,"birchlakeflight1"
                  ,"blueberryfruit"
                  ,"bonanza"
                  ,"bonanza2lclip"
                  ,"bonzana2"
                  ,"s1d"
                  ,"s1w"
                  ,"grey11"
                  ,"grey33"
                  ,"grey66"
                  ,"white"
                  ,"eightmileflight2"
                  ,"SchurrGradient"
                  ,"Aquatic1"
                  ,"Aquatic2"
                  ,"littlelakefligh1"
                  ,"MDomeT1"
                  ,"whiteref"
                  ,"whiterest"
                  ,"wildrhubarb"
                  ,"wildrhubarbflowre"
                  ,"valtrdf2") %>% 
  as.data.frame() %>%
  rename(Code_name = ".")


meta_clean %>% 
  tidyr::separate(ScanID, into = c("Code_name","ScanNum"), sep="_") %>% 
  anti_join(CodeNames_omit, by = "Code_name") %>%
  group_by(Code_name) %>% tally() %>% View()
  #dplyr::select(Code_name) %>%
  #unique() %>% 
  #anti_join(Species_groups, by = "Code_name", keep = FALSE) %>%
  #dim


  #tst_meta %>% 
  #mutate(Code_name = substr(tst_meta$ScanID, start = 1, stop = 10)) %>%
  #dplyr::select(Code_name) %>% unique()
#saveRDS(TwelveMile_spectra, "./Output/A_001_SC2_TwelveMile_spectra.rds")

