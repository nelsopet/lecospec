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
spec_missing_all_raw<-Reduce(spectrolab::combine, spec_missing_all)
names(spec_missing_all_raw) %>% length()
spec_missing_all<-spec_missing_all_raw
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
meta_raw<-meta(spec_missing_all)
meta_clean<-meta(spec_missing_all)

meta_clean$ScanID <- gsub("chatnika_F3", "chatnikaF3", meta_clean$ScanID)
meta_clean$ScanID <- gsub("M_Dome_T1", "MDomeT1", meta_clean$ScanID)
meta_clean$ScanID <- gsub("sal_wooly", "salixwooly", meta_clean$ScanID)
meta_clean$ScanID <- gsub("birchlake_carex", "carex", meta_clean$ScanID)
meta_clean$ScanID <- gsub("valtrd_f2", "valtrdf2", meta_clean$ScanID)

#Code_name
meta_clean$ScanID <- gsub("alderspe", "ALVI5", meta_clean$ScanID)
meta_clean$ScanID <- gsub("betulspe", "betneo", meta_clean$ScanID)
meta_clean$ScanID <- gsub("carex","carlin", meta_clean$ScanID)
meta_clean$ScanID <- gsub("eqiswl", "equsyl", meta_clean$ScanID)
#5                    kalmea
meta_clean$ScanID <- gsub("labradortea","LEDUM", meta_clean$ScanID)
meta_clean$ScanID <- gsub("larix","larlar", meta_clean$ScanID)
#8                    potfru
meta_clean$ScanID <- gsub("evnes","evemes", meta_clean$ScanID)
meta_clean$ScanID <- gsub("ledgro","LEDUM", meta_clean$ScanID)
meta_clean$ScanID <- gsub("rubus","rubcam", meta_clean$ScanID)
meta_clean$ScanID <- gsub("sphag","sphagn", meta_clean$ScanID)
meta_clean$ScanID <- gsub("sphagnn","sphagn", meta_clean$ScanID)
#13           tuckermanopsis
#14         
#15         
#16         
meta_clean$ScanID <- gsub("sphagnum","sphagn", meta_clean$ScanID)
meta_clean$ScanID <- gsub("arctoparmelia","arccen", meta_clean$ScanID)
meta_clean$ScanID <- gsub("bareMinSoil","bare_soil", meta_clean$ScanID)
meta_clean$ScanID <- gsub("barerock","bare rock", meta_clean$ScanID)
meta_clean$ScanID <- gsub("bare_soil","bare soil", meta_clean$ScanID)
meta_clean$ScanID <- gsub("bare_soil","bare soil", meta_clean$ScanID)

meta_clean$ScanID <- gsub("kwarts","quartz", meta_clean$ScanID)
meta_clean$ScanID <- gsub("loisleuria","loipro", meta_clean$ScanID)
meta_clean$ScanID <- gsub("masricDRY","masric", meta_clean$ScanID)
meta_clean$ScanID <- gsub("melhap","melhep", meta_clean$ScanID)
#meta_clean$ScanID <- gsub("orangePorpidia","orange_Porpidia", meta_clean$ScanID)
meta_clean$ScanID <- gsub("perdac","pedrac", meta_clean$ScanID)
meta_clean$ScanID <- gsub("petfer","petfri", meta_clean$ScanID)
#28          rhizocarponGREY,
meta_clean$ScanID <- gsub("rhwrug","rhyrug", meta_clean$ScanID)
#meta_clean$ScanID <- gsub("salixwooly","wooly_salix", meta_clean$ScanID)
meta_clean$ScanID <- gsub("tofeldia","toefeldia", meta_clean$ScanID)
#32               bryoriawet
#33                cetislwet
#34                clamitwet
#35                claranwet
#36                clastywet
#37                clasulwet
#38              dicranumwet
#39                evemeswet
#40                hylsplwet
#41                hypauswet
#42             melaneliawet
#43                parambwet
#44                parsulwet
#45                pelaptwet
#46                pelscawet
#47            poljpleschwet
#48                poljunwet
#49            polstricumwet
#50            russularedwet
#51 trapeliopsisgranulosawet
#52                usnlapwet
#53                usnscawet
#54                vulpinwet
meta_clean$ScanID <- gsub("alnvir","ALVI5", meta_clean$ScanID)
#56                   corcan
meta_clean$ScanID <- gsub("dryas","DROCA2", meta_clean$ScanID)
#58                   epiang
meta_clean$ScanID <- gsub("poljunspopyt","poljun", meta_clean$ScanID)
#60                   spibea
meta_clean$ScanID <- gsub("blueberry","vaculi", meta_clean$ScanID)
#vaculifruit
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
                  ,"valtrdf2"
                  ,"chatnika"
                  ,"chatnikaF3"
                  ,"eightmileflight1") %>% 
  as.data.frame() %>%
  rename(Code_name = ".")

meta_clean_filt<-
meta_clean %>% 
  tidyr::separate(ScanID, into = c("Code_name","ScanNum"), sep="_", remove= TRUE) %>% 
  anti_join(CodeNames_omit, by = "Code_name") %>%
  dplyr::mutate(Code_name = replace(Code_name, Code_name == "bare soil", "bare_soil")) %>%
  #dplyr::select(Code_name) %>%
  #unique() %>% 
  inner_join(Species_groups, by = "Code_name", keep = FALSE) # %>% #dim
  #dplyr::select(Code_name)
  #group_by(Functional_group1) %>%
  #tally()

meta_clean<-  meta_clean %>%
  tidyr::separate(ScanID, into = c("Code_name","ScanNum"), sep="_", remove = FALSE) # %>% 

meta_clean$ScanID<-meta_raw$ScanID


spec_missing_out<-  
spec_missing_all %>% 
  speclib_to_df() %>% #colnames()
  inner_join((meta_clean %>% dplyr::select(ScanID, Area, Code_name, ScanNum)), by=c("ScanID", "Area"), keep=FALSE) %>% #colnames()
  inner_join((meta_clean_filt %>% dplyr::select(Code_name, ScanNum, Area)), by=c("Code_name", "ScanNum","Area"), keep=FALSE) #%>% colnames()
  #dplyr::select(Code_name, Area) %>%
  #unique() %>% dim
  #tst_meta %>% 
  #mutate(Code_name = substr(tst_meta$ScanID, start = 1, stop = 10)) %>%
  #dplyr::select(Code_name) %>% unique()

meta_colnames<-c(colnames(spec_missing_out[,1:26]), "Code_name", "ScanNum")

spec_missing_out<-spec_missing_out %>% dplyr::select(all_of(meta_colnames), everything())  

Target_names <- unique(sort(spec_missing_out$Code_name))

# Creates an empty list
each_target <- list()
meta_columns <- 28

# Function splits the spectral library into spectral objects based on each target (105 Spectral Objects)
for (i in 1:length(Target_names)) {
  
  # Subset a functional group
  each_target[[i]] <- subset(spec_missing_out, Code_name == Target_names[i])
  
  # saves metadata
  metadata <- each_target[[i]][, c(1:(meta_columns))] %>% as.data.frame()
  
  # Convert to a spectral object
  each_target[[i]] <- as_spectra(each_target[[i]][-1:-(meta_columns)])
  # each_target[[i]] <-normalize(each_target[[i]])
  # Add metadata
  meta(each_target[[i]]) <- data.frame(metadata[, c(1:(meta_columns))], stringsAsFactors = FALSE)
}

# Renames each target in list
each_target <- each_target %>% setNames(Target_names)

plot_interactive(each_target[["aleoch"]])#1       Alectoria ochroleuca    6 #Remove scan 2
plot_interactive(each_target[["ALVI5"]])#1       Alnus      #Two different surfaces here
plot_interactive(each_target[["arccen"]])#1       Arctoparmelia      #Remove scan 4
plot_interactive(each_target[["arcrub"]])#1             #Remove scan 11
plot_interactive(each_target[["aulpal"]])#1             #Looks good
plot_interactive(each_target[["aultur"]])#1             #Looks good
plot_interactive(each_target[["bare rock"]])#1             #Looks good
plot_interactive(each_target[["betnan"]]) # Not sure these are good. Compare to other betnan
plot_interactive(each_target[["betneo"]])#1             #mixture of things ... need to reassess
plot_interactive(each_target[["betpap"]])#1             #mixture of things ... need to reassess
plot_interactive(each_target[["bryoria"]])#1             #mixture ... reassess
plot_interactive(each_target[["carlin"]])#1             #mixture ... reassess
plot_interactive(each_target[["cetisl"]])#1             #looks good
plot_interactive(each_target[["cetlae"]])#1             #Looks good
plot_interactive(each_target[["claama"]])#1             #Looks good
plot_interactive(each_target[["clacor"]])#1             #Scan 3 is very dark
plot_interactive(each_target[["clagra"]])#1             #Looks good
plot_interactive(each_target[["clamit"]])#1             #Looks good
plot_interactive(each_target[["claste"]])#1             #Looks good
plot_interactive(each_target[["DROCA2"]])#1             #Looks good
plot_interactive(each_target[["equsyl"]])#1             #Remove scan 4 ... shape looks like a lichen
plot_interactive(each_target[["erivag"]])#1             #Appears to be two different things ... reassess
plot_interactive(each_target[["evemes"]])#1             #Not good ... delete
plot_interactive(each_target[["flacuc"]])#1             #REmove scan 3 .... data looks like it was from bare fiber optic
plot_interactive(each_target[["flaniv"]])#1             #Remove scan 4
plot_interactive(each_target[["gravel"]])#1             #Remove Scan 5
plot_interactive(each_target[["hylspl"]])#1             #Looks good
plot_interactive(each_target[["larlar"]])#1             #Werid mix ... delete
plot_interactive(each_target[["LEDUM"]])#1             #Werid mix ... delete
plot_interactive(each_target[["loipro"]])#1             #Looks good
plot_interactive(each_target[["luparc"]])#1             #Remove scan 1
plot_interactive(each_target[["masric"]])#1             #Looks good
plot_interactive(each_target[["melhep"]])#1             #Looks good
plot_interactive(each_target[["naparc"]])#1             #Looks good but name is misspelled
plot_interactive(each_target[["pedrac"]])#1             #Remove scan 11
plot_interactive(each_target[["petfri"]])#1             #Looks good
plot_interactive(each_target[["picmar"]])#1             #Delete or investigate ... not a green plant spectrum
plot_interactive(each_target[["plesch"]])#1             #Looks good but suspciously like a yellow lichen
plot_interactive(each_target[["poljun"]])#1             #Looks good but suspciously like a yellow lichen
plot_interactive(each_target[["polytrichum"]])#1             #Looks good but suspciously like a green plant
plot_interactive(each_target[["quartz"]])#1             #Remove scan 13
plot_interactive(each_target[["rhigeo"]])#1             #Looks good
plot_interactive(each_target[["rhyrug"]])#1             #Looks good
plot_interactive(each_target[["rubcam"]])#1             #Delete or investigate ... not a green plant spectrum
plot_interactive(each_target[["salpul"]])#1             #Delete
plot_interactive(each_target[["sphagn"]])#1             #Mix of more than one thing ... delete?
plot_interactive(each_target[["toefeldia"]])#1             #Remove scans 2 and 5
plot_interactive(each_target[["tomnit"]])#1             #Looks good
plot_interactive(each_target[["umbhyp"]])#1             #Looks good
plot_interactive(each_target[["vaculi"]])#1             #Delete




#saveRDS(TwelveMile_spectra, "./Output/A_001_SC2_TwelveMile_spectra.rds")

