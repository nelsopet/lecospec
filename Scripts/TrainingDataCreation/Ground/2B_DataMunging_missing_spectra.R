# -------------------------------------------- Script 2B ------------------------------------------------------------- #
# The script fix names and saves the spectra collected at each location in alaska that were not included in the original
# spectra library. 
# Packages to install
library(spectrolab)
library(tidyverse)

#Species groupings
Species_groups <- read.csv("./Data/SpeciesTable_20230417.csv", encoding = "UTF-8")

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
meta(dir_spec_fix)<-dirs_spec_new_meta[8] %>% as.data.frame() %>% dplyr::rename(Area = '.')
#names(dir_spec_fix)<-dir_spec_names[8]

spec_missing_all<-
  lapply(1:length(dir_spec_list), 
       function(y){
          tst_speclib_onesite<-dir_spec_list[[y]]
          tst_spec_names <- list.files("./Data/SpectraByLocation/12_Mile/original_samples", pattern = ".sed", full.names = F)
          dir_spec_fix<-lapply(1:length(tst_speclib_onesite), read_spectra_fix_missing_spec)
          dir_spec_fix<-Reduce(spectrolab::combine, dir_spec_fix)
          meta(dir_spec_fix)<-dirs_spec_new_meta[[y]] %>% as.data.frame() %>% dplyr::rename(Area = '.')
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
# Grab metadata from instrument
spec_missing_all_metadata_instrument <- lapply(1:length(dir_spec_rds), function(x) meta(dir_spec_rds[[x]])) 
spec_missing_all_metadata_instrument<-Reduce(rbind,spec_missing_all_metadata_instrument) %>% as.data.frame()

# Combine metadata generated in this script with instrument metadata
spec_missing_all_metadata <- cbind(spec_missing_all_metadata, spec_missing_all_metadata_instrument)
spec_missing_all_metadata<-cbind(spec_missing_all_metadata,meta(spec_missing_all_raw) %>% dplyr::select(Area))
## Set metadata
meta(spec_missing_all) <- spec_missing_all_metadata #data.frame(spec_missing_all_metadata, stringsAsFactors = FALSE)
dim(meta(spec_missing_all))

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
  dplyr::rename(Code_name = ".")

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
  
meta_colnames<-c(colnames(spec_missing_out[,1:26]), "Code_name", "ScanNum")

spec_missing_out<-spec_missing_out %>% dplyr::select(all_of(meta_colnames), everything())  

missing_target_names <- unique(sort(spec_missing_out$Code_name))

# Creates an empty list
missing_target <- list()
missing_meta_cols <- 28

# Function splits the spectral library into spectral objects based on each target (105 Spectral Objects)
for (i in 1:length(missing_target_names)) {
  
  # Subset a functional group
  missing_target[[i]] <- subset(spec_missing_out, Code_name == missing_target_names[i])
  
  # saves metadata
  metadata <- missing_target[[i]][, c(1:(missing_meta_cols))] %>% as.data.frame()
  
  # Convert to a spectral object
  missing_target[[i]] <- as_spectra(missing_target[[i]][-1:-(missing_meta_cols)])
  # missing_target[[i]] <-normalize(missing_target[[i]])
  # Add metadata
  meta(missing_target[[i]]) <- data.frame(metadata[, c(1:(missing_meta_cols))], stringsAsFactors = FALSE)
}

# Renames each target in list
missing_target <- missing_target %>% setNames(missing_target_names)

#plot_interactive(missing_target[["aleoch"]])#1       Alectoria ochroleuca    6 #Remove scan 2
 missing_target[["aleoch"]]<-missing_target[["aleoch"]][-c(2),]
#plot_interactive(missing_target[["ALVI5"]])#1       Alnus      #Two different surfaces here
 missing_target[["ALVI5"]]<-missing_target[["ALVI5"]][-c(1:5,16:26, 37:44),]
#plot_interactive(missing_target[["arccen"]])#1       Arctoparmelia      #Remove scan 4
 missing_target[["arccen"]]<-missing_target[["arccen"]][-c(4),]
#plot_interactive(missing_target[["arcrub"]])#1             #Remove scan 11
 missing_target[["arcrub"]]<-missing_target[["arcrub"]][-c(11),]
#plot_interactive(missing_target[["aulpal"]])#1             #Looks good
#plot_interactive(missing_target[["aultur"]])#1             #Looks good
#plot_interactive(missing_target[["bare rock"]])#1             #Looks good
#plot_interactive(missing_target[["betnan"]]) # Remove all for now: Not sure these are good. #Remove scans 2:3, 8
 missing_target[["betnan"]]<-NULL #missing_target[["betnan"]][-c(2:3,8),]
#plot_interactive(missing_target[["betneo"]])#1             #mixture of things ... need to reassess Remove 1:8, 18:23
 missing_target[["betneo"]]<-missing_target[["betneo"]][-c(1:8, 18:23),]
#plot_interactive(missing_target[["betpap"]])#1             #mixture of things ... need to reassess: Delete all
  missing_target[["betpap"]]<-NULL #missing_target[["betnan"]][-c(2:3,8),]
#plot_interactive(missing_target[["bryoria"]])#1             #mixture ... reassess: Delete all
  missing_target[["bryoria"]]<-NULL #missing_target[["betnan"]][-c(2:3,8),]
#plot_interactive(missing_target[["carlin"]])#1             #mixture ... reassess: Delete all
  missing_target[["carlin"]]<-NULL #missing_target[["betnan"]][-c(2:3,8),]
#plot_interactive(missing_target[["cetisl"]])#1             #looks good
#plot_interactive(missing_target[["cetlae"]])#1             #Looks good
#plot_interactive(missing_target[["claama"]])#1             #Looks good
#plot_interactive(missing_target[["clacor"]])#1             #Scan 3 is very dark
 missing_target[["clacor"]]<-missing_target[["clacor"]][-c(3),]
#plot_interactive(missing_target[["clagra"]])#1             #Looks good
#plot_interactive(missing_target[["clamit"]])#1             #Looks good
#plot_interactive(missing_target[["claste"]])#1             #Looks good
#plot_interactive(missing_target[["DROCA2"]])#1             #Looks good
#plot_interactive(missing_target[["equsyl"]])#1             #Remove all: Remove scan 4 ... shape looks like a lichen: Exactly the same data as flavniv?
 missing_target[["equsyl"]]<-NULL
#plot_interactive(missing_target[["erivag"]])#1             #Appears to be two different things ... reassess: Remove all
 missing_target[["erivag"]]<-NULL
#plot_interactive(missing_target[["evemes"]])#1             #Not good ... delete: Remove all
 missing_target[["evemes"]]<-NULL
#plot_interactive(missing_target[["flacuc"]])#1             #REmove scan 3: Remove all .... data looks like it was from bare fiber optic
 missing_target[["flacuc"]]<-NULL
#plot_interactive(missing_target[["flaniv"]])#1             #Remove scan 4
 missing_target[["flaniv"]]<-missing_target[["flaniv"]][-c(1),]
#plot_interactive(missing_target[["gravel"]])#1             #Remove Scan 5
 missing_target[["gravel"]]<-missing_target[["gravel"]][-c(5),]
#plot_interactive(missing_target[["hylspl"]])#1             #Looks good
#plot_interactive(missing_target[["larlar"]])#1             #Werid mix ... delete: Remove all
 missing_target[["larlar"]]<-NULL
#plot_interactive(missing_target[["LEDUM"]])#1              #Werid mix ... delete: Remove all
 missing_target[["LEDUM"]]<-NULL
#plot_interactive(missing_target[["loipro"]])#1             #Looks good
#plot_interactive(missing_target[["luparc"]])#1             #Remove scan 1
 missing_target[["luparc"]]<-missing_target[["luparc"]][-c(1),]
#plot_interactive(missing_target[["masric"]])#1             #Looks good
#plot_interactive(missing_target[["melhep"]])#1             #Looks good
#plot_interactive(missing_target[["naparc"]])#1             #Looks good but name is misspelled
#plot_interactive(missing_target[["pedrac"]])#1             #Remove scan 11
 missing_target[["pedrac"]]<-missing_target[["pedrac"]][-c(11),]
#plot_interactive(missing_target[["petfri"]])#1             #Looks good
#plot_interactive(missing_target[["picmar"]])#1             #Delete or investigate ... not a green plant spectrum
 missing_target[["picmar"]]<-NULL
#plot_interactive(missing_target[["plesch"]])#1             #Looks good but suspciously like a yellow lichen
#plot_interactive(missing_target[["poljun"]])#1             #Looks good but suspciously like a yellow lichen
#plot_interactive(missing_target[["polytrichum"]])#1        #Looks good but suspciously like a green plant
#plot_interactive(missing_target[["quartz"]])#1             #Remove scan 13
#plot_interactive(missing_target[["rhigeo"]])#1             #Looks good
#plot_interactive(missing_target[["rhyrug"]])#1             #Looks good
#plot_interactive(missing_target[["rubcam"]])#1             #Delete or investigate ... not a green plant spectrum
missing_target[["rubcam"]]<-NULL
#plot_interactive(missing_target[["salpul"]])#1             #Delete
missing_target[["salpul"]]<-NULL
#plot_interactive(missing_target[["sphagn"]])#1             #Mix of more than one thing ... delete?
missing_target[["sphagn"]]<-NULL
#plot_interactive(missing_target[["toefeldia"]])#1             #Remove scans 2 and 5
missing_target[["toefeldia"]]<-missing_target[["toefeldia"]][-c(2,5),]
#plot_interactive(missing_target[["tomnit"]])#1             #Looks good
#plot_interactive(missing_target[["umbhyp"]])#1             #Looks good
#plot_interactive(missing_target[["vaculi"]])#1             #Delete
missing_target[["vaculi"]]<-NULL


# Creates a new object with cleaned spectral library
missing_target_filt <- missing_target

# Remove scans that are Epiphytes
#missing_target_filt[c(
#  "Vulpicida pinastri",
#  "Usnea scabrata",
#  "Usnea lapponica",
#  "Parmelis sulcata",
#  "Hypogymnia austerodes",
#  "Evernia mesomorpha",
#  "Flavocetraria cucculata",
#  "Bryoria sp."
#)] <- NULL

# Combines all species into one spectral library if satisfied with our results

# The result is a dataframe
missing_cleaned_speclib <- Reduce(spectrolab::combine, missing_target_filt) %>% 
  as.data.frame() %>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)

# Creates .rds object
missing_cleaned_speclib_rds <- Reduce(spectrolab::combine, missing_target_filt)

MissingSpecLib <- missing_cleaned_speclib %>%
  as.data.frame() %>%
  inner_join(Species_groups, by = "Code_name") %>% #colnames() %>% as.data.frame() %>% View()# Joins dataframe with all the species info to our spectral library
  dplyr::select(
    ScanID,
    Code_name,
    Species_name,
    Genus,
    Functionalgroup2,
    Functional_group1,
    Functional_Group0,
    Area,
    everything()
  ) %>%
  mutate(ScanID = as.character(ScanID)) %>%
  dplyr::rename(Functional_group2= Functionalgroup2) #%>% colnames() # Reorders columns
table(MissingSpecLib$Area) %>% as.data.frame()

write.csv(MissingSpecLib, "./Output/C_001_SC3_Missing_Cleaned_SpectralLib.csv")

saveRDS(missing_cleaned_speclib_rds, "./Output/C_002_SC3__Missing_Cleaned_Speclib.rds")

