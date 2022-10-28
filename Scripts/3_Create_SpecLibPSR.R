# ----------------------- Script 3 -------------------------- #
# Script creates a Cleaned Spectral library object,
# combining all the Scans collected in Alaksa during 2018 and 2019
# Packages to install
library(plyr)
library(dplyr)
library(ggplot2)
library(spectrolab)
library(tidyverse)
library(parallel)
library(doParallel)
library(gridExtra)
library(glue)

source("Functions/lecospectR.R")
# ------------------------------------------------ Step1: Data Munging Step ------------------------------------------------- #
# In this step we will combine all our spectral profiles to form one spectral library

# Creates a file path to where our spectral libraries for each site is loacated
output_path <- "./Output/"

# Reads in species and functional level groups dataframe creatd in script 1
Species_groups <- read.csv("./Data/SpeciesTable_20220125.csv", encoding = "UTF-8")

colnames(Species_groups) <- c(
  "Code_name",
  "Species_name",
  "Genus",
  "Functional_group2",
  "Functional_group1"
)

# Reads in scans from ecosis website
# Additional spectral profiles that will be combined to the ones collected in Alaska

Spectra_Ecosis_1 <- read.csv("Data/leaf_spectra_barrow_2013_20180824_ecosis.csv", check.names = F)
Metadata_Ecosis_1 <- read.csv("Data/ngeearctic_bnl_2013_leaf_spectra_traits_metadata.csv", check.names = F)

# Merges meta and spectra
Ecosis_data_1 <- cbind(Metadata_Ecosis_1[, -3], Spectra_Ecosis_1[, -1])

# Change name of column and add an area column
names(Ecosis_data_1)[2] <- "Code_name"
names(Ecosis_data_1)[1] <- "ScanID"
Ecosis_data_1$Area <- "Barrow"

Ecosis_data_1$ScanID <- as.character(Ecosis_data_1$ScanID)

Ecosis_data_1 <- Ecosis_data_1 %>% dplyr::mutate(dplyr::across(.cols = `350`:`2500`,~ .x * 100, na.rm = TRUE)) # ,

###

Spectra_Ecosis_2 <- read.csv("Data/sewpen_2019_canopy_spectral_reflectance.csv", check.names = F)
Metadata_Ecosis_2 <- read.csv("Data/sewpen_2019_canopy_spectral_reflectance_metadata.csv", check.names = F)

Ecosis_data_2 <- inner_join(Metadata_Ecosis_2, Spectra_Ecosis_2, by = "SampleID", keep = FALSE)

# Change name of column and add an area column
Ecosis_data_2 <- Ecosis_data_2 %>%
  dplyr::rename(
    Code_name = Dominant_Species,
    ScanID = SampleID
  ) %>%
  mutate(
    Area = "Seward_Penn",
    ScanID = as.character(ScanID)
  )
Ecosis_data_2 <- Ecosis_data_2 %>% dplyr::mutate(dplyr::across(.cols = `350`:`2500`, na.rm = TRUE)) # ,~ .x / 100

Eco1_names <- colnames(Ecosis_data_1)
Ecosis_data_2 <- Ecosis_data_2 %>% dplyr::select(Eco1_names)

Ecosis_data_2_missing_names <- Ecosis_data_2 %>%
  anti_join(Species_groups, by = "Code_name", keep = FALSE) %>%
  group_by(Code_name) %>%
  tally()

###
Spectra_Ecosis_3_1 <- read.csv("Data/ngee-arctic_2014_barrow_svchr1024i_canopy_spectral_reflectance.csv", check.names = F)
Spectra_Ecosis_3_2 <- read.csv("Data/ngee-arctic_2015_barrow_svchr1024i_canopy_spectral_reflectance.csv", check.names = F)

Spectra_Ecosis_3 <- bind_rows(Spectra_Ecosis_3_1, Spectra_Ecosis_3_2)

Metadata_Ecosis_3 <- read.csv("Data/ngee-arctic_2014_to_2016_barrow_canopy_spectral_reflectance_metadata.csv", check.names = F)

Ecosis_data_3 <- Metadata_Ecosis_3 %>%
  inner_join(Spectra_Ecosis_3, by = "Sample_ID", keep = FALSE) # %>%

# Change name of column and add an area column
Ecosis_data_3 <- Ecosis_data_3 %>%
  dplyr::rename(Code_name = USDA_Species_Code, ScanID = Sample_ID
  ) %>%
  mutate(Area = "Barrow",
    ScanID = as.character(ScanID)
  )

Ecosis_data_3 <- Ecosis_data_3 %>% dplyr::select(Eco1_names)

Ecosis_data_3 <- Ecosis_data_3 %>% dplyr::mutate(dplyr::across(.cols = `350`:`2500`,   na.rm = TRUE)) #~ .x / 100,

Ecosis_data_3 %>%
  anti_join(Species_groups, by = "Code_name", keep = FALSE) %>%
  group_by(Code_name) %>%
  tally()


Ecosis_data_all <- bind_rows(Ecosis_data_1, Ecosis_data_2, Ecosis_data_3)

# Creates a spectral library from ecosis data
Ecosis_data <- Ecosis_data_all %>%
  inner_join(Species_groups, by = "Code_name") %>% # head
  dplyr::select(
    ScanID,
    Code_name,
    Species_name,
    Genus,
    Functional_group2,
    Functional_group1,
    Area,
    everything()
  )
Ecosis_data %>%
  group_by(Species_name) %>%
  tally()

Ecosis_Colnames<-colnames(Ecosis_data)
meta_names<-Ecosis_Colnames[1:7]
as_spectra(Ecosis_data %>% dplyr::select(-meta_names))  %>% as.matrix() %>% hist()


### Read in our data by site
# Import file path names of .rds files into character list (Spectral libraries based on each location in alaska)
SpecLib_by_location <- list.files(output_path, pattern = "A_0", full.names = T)

# Reads in the spectral libraries for each location in a list...List of 13 spectral objects
list_of_SpecLib <- lapply(SpecLib_by_location, readRDS) %>% # Reads in the spectral library for each site
  setNames(gsub("./Output/", "", SpecLib_by_location)) # Removes dir path from the name

# Combines specral libraries from all locations
SpecLib_raw <- Reduce(spectrolab::combine, list_of_SpecLib) # %>% # dim(n_samples=1989, n_wavelegths=2151)

SpecLib_raw %>% as.data.frame() %>% dim #dplyr::select(-1:-29) %>% as.matrix() %>% hist()

Speclib_metadata <- spectrolab::meta(SpecLib_raw)

write_csv(Speclib_metadata, "./Output/C_000_Speclib_raw_metadata.csv")

SpecLib <- SpecLib_raw %>%
  as.data.frame() %>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name) %>% # Removes unwanted column ~ should remove this later instead
  inner_join(Species_groups, by = "Code_name") %>% #colnames() %>% View()# Joins dataframe with all the species info to our spectral library
  dplyr::select(
    all_of(Ecosis_Colnames)
    #ScanID,
    #Code_name,
    #Species_name,
    #Genus,
    #Functional_group2,
    #Functional_group1,
    #Area,
    #everything()
  ) %>%
  mutate(ScanID = as.character(ScanID)) #%>% colnames() # Reorders columns

# Write out full spectral library including Dryas scans from Stasinski et al. 2021
write_csv(SpecLib, "./Output/C_000_Speclib_raw.csv")

# Combines Ecosis data and spectral library

SpecLib_EcoSis_raw <- bind_rows(SpecLib, Ecosis_data) 
SpecLib_EcoSis_raw_out_db<-as.data.frame(SpecLib_EcoSis_raw)


# Adds more details to our spectral library (Freq columns = The count of each Species)
# Frequency values represent the number of scans per species and the number of scans per functional group
SpecLib_new_All <- SpecLib_EcoSis_raw %>% # SpecLib_new%>%
  plyr::ddply(
    .(Species_name),
    mutate,
    Species_name_Freq = length(Species_name)
  ) %>% # Add column to data frame that shows frequency of species
  plyr::ddply(
    .(Functional_group1),
    mutate,
    Functional_group1_Freq = length(Functional_group1)
  ) %>% # Add column to data frame that shows frequency of functional group
  plyr::ddply(.(Functional_group2),
    mutate,
    Functional_group2_Freq = length(Functional_group2)
  ) %>% # Add column to data frame that shows frequency of courser functional groups
  dplyr::select(
    ScanID,
    Area,
    Code_name,
    Species_name,
    Functional_group1,
    Functional_group2,
    Area,
    Species_name_Freq,
    Functional_group1_Freq,
    Functional_group2_Freq,
    everything()
  ) # Rearrange columns

# Removes all unknown scans
SpecLib_new_All <- SpecLib_new_All[!(SpecLib_new_All$Species_name == "Unknown"), ]

# Filter out rows with really low reflectance at a band for which all surfaces should have elevated reflectance
SpecLib_new_All <- SpecLib_new_All %>% #dplyr::filter(`1250` > 0.1) %>% head() %>% View()
  dplyr::filter(`1250` >0.1) %>% #group_by(Area) %>% tally()
  mutate(Species_name = replace(Species_name, Species_name == "Cladonia steallaris", "Cladonia stellaris")) %>%
  mutate(Species_name = replace(Species_name, Species_name == "Petasites frigida", "Petasites frigidus"))

# ------------------------------------------------- Step 2: Spectral Library Clean Up -------------------------------------------------- #
# This section pf the script removes weird scans from each species in the combines spectral library
# table(SpecLib_new_All$Species_name)%>%as.data.frame() # There are 105 species in our spectral library


# Creates an object with all the species names sorted in alphabetical order
Target_names <- unique(sort(SpecLib_new_All$Species_name))

# Creates an empty list
each_target <- list()
meta_columns <- 10

# Function splits the spectral library into spectral objects based on each target (105 Spectral Objects)
for (i in 1:length(Target_names)) {

  # Subset a functional group
  each_target[[i]] <- subset(SpecLib_new_All, Species_name == Target_names[i])
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
# Var1 Freq
# plot_interactive(each_target[["Alectoria ochroleuca"]])#1       Alectoria ochroleuca    6 #Remove scan 2
  each_target[["Alectoria ochroleuca"]] <- each_target[["Alectoria ochroleuca"]][-c(2), ]
#plot_interactive(each_target[["Alnus sp."]])#2                  Alnus sp.   80 # Need to clean out half of these because there are two groups.
 each_target[["Alnus sp."]] <- each_target[["Alnus sp."]][-c(1:43), ]
# plot_interactive(each_target[["Arctagrostis latifolia"]])#3     Arctagrostis latifolia    5#Remove scan 1
 each_target[["Arctagrostis latifolia"]] <- each_target[["Arctagrostis latifolia"]][-c(1), ]
# plot_interactive(each_target[["Arctocetraria centrifuga"]])#4   Arctocetraria centrifuga    4 
# plot_interactive(each_target[["Arctophila fulva"]])#5           Arctophila fulva   12 #Remove scans 1-5
  each_target[["Arctophila fulva"]] <- each_target[["Arctophila fulva"]][-c(1:5)] #               ]
# plot_interactive(each_target[["Arctostaphyllos"]])#6            Arctostaphyllos   25 #Mixed of two things, Remove scans 20:25 based on SWIR detector edge artifact
# plot_interactive(each_target[["Arenaria pseudofrigida"]])#7     Arenaria pseudofrigida    1
# plot_interactive(each_target[["Asahinea chrysantha"]])#8        Asahinea chrysantha   19
# plot_interactive(each_target[["Aulacomnium palustre"]])#9       Aulacomnium palustre    6
# plot_interactive(each_target[["Aulacomnium turgidum"]])#10      Aulacomnium turgidum    6

#   plot_interactive(each_target[["Bare Rock"]])#11                 Bare Rock    7
# plot_interactive(each_target[["Bare Soil"]])#12                 Bare Soil   13
# plot_interactive(each_target[["Betula nana"]])#13               Betula nana   65 # #Remove scan 33 and 49
  each_target[["Betula nana"]] <- each_target[["Betula nana"]][-c(33,49), ] # [-c(5,18,26,27,32,43,56),              ]
# plot_interactive(each_target[["Betula neoalaskana"]])#14        Betula neoalaskana    4 
# plot_interactive(each_target[["Bryoria sp."]])#15               Bryoria sp.   10
# plot_interactive(each_target[["Calamogrostis sp."]])#16         Calamogrostis sp.    4
# plot_interactive(each_target[["Carex aquatilis"]])#17           Carex aquatilis   36 #Remove scan 1:8, 36
  each_target[["Carex aquatilis"]] <- each_target[["Carex aquatilis"]][-c(1:8, 36), ] # [-c(10,12,13,14,15),                   ]
# plot_interactive(each_target[["Carex sp."]])#18                 Carex sp.   16 #Remove scan 10, 13:15
   each_target[["Carex sp."]] <- each_target[["Carex sp."]][-c(10,13:15), ] # [-c(10,12,13,14,15),                   ]
# plot_interactive(each_target[["Cassiope tetragona"]])#19        Cassiope tetragona    9
# plot_interactive(each_target[["Ceratadon purpureus"]])#20       Ceratadon purpureus    5
# plot_interactive(each_target[["Cetraria islandica"]])#21        Cetraria islandica   14 #Remove 1:4
   each_target[["Cetraria islandica"]] <- each_target[["Cetraria islandica"]][-c(1:4), ]
# plot_interactive(each_target[["Cetraria laevigata"]])#22        Cetraria laevigata    3
# plot_interactive(each_target[["Cladonia amaurocraea"]])#23      Cladonia amaurocraea    6
# plot_interactive(each_target[["Cladonia cornuta"]])#24          Cladonia cornuta    3
# plot_interactive(each_target[["Cladonia gracilis"]])#25         Cladonia gracilis   18 #Remove scans 13-18
   each_target[["Cladonia gracilis"]] <- each_target[["Cladonia gracilis"]][-c(13:18), ]
# plot_interactive(each_target[["Cladonia mitis"]])#26            Cladonia mitis   17
# plot_interactive(each_target[["Cladonia rangiferina"]])#27      Cladonia rangiferina   20 #UPDATE: trim <450nm and keep evrything: Old: Two groups, esp 350-400n, remove scans 1:4
# plot_interactive(each_target[["Cladonia stellaris"]])#28       Cladonia stellaris   20
# plot_interactive(each_target[["Cladonia stygia"]])#29           Cladonia stygia   18 #Remove scan 13
  each_target[["Cladonia stygia"]] <- each_target[["Cladonia stygia"]][-c(13), ]
# plot_interactive(each_target[["Cladonia sulphurina"]])#30       Cladonia sulphurina    3
# plot_interactive(each_target[["Cladonia uncialis"]])#31         Cladonia uncialis   10  #Remove scan 6
  each_target[["Cladonia uncialis"]] <- each_target[["Cladonia uncialis"]][-c(6), ]
# plot_interactive(each_target[["Dactylina arctica"]])#32         Dactylina arctica    7
# plot_interactive(each_target[["Dead Salix"]])#33                Dead Salix    8
# plot_interactive(each_target[["Dicranum sp."]])#34              Dicranum sp.    5
# plot_interactive(each_target[["Dryas sp."]])#35                 Dryas sp.  925 #Trim some of these out!!
  each_target[["Dryas sp."]] <- each_target[["Dryas sp."]][-c(10, 11, 29, 33, 32, 38, 41, 42, 48, 61:925), ]
# plot_interactive(each_target[["Dupontia fisheri"]])#36          Dupontia fisheri   10
# plot_interactive(each_target[["Empetrum nigrum"]])#37           Empetrum nigrum   14 #Remove scan 5
  each_target[["Empetrum nigrum"]] <- each_target[["Empetrum nigrum"]][-c(5), ]
# plot_interactive(each_target[["Equisetum arvense"]])#38         Equisetum arvense    7
# plot_interactive(each_target[["Equisetum sylvaticum"]])#39      Equisetum sylvaticum    4
# plot_interactive(each_target[["Eriophorum angustifolium"]])#40  Eriophorum angustifolium   44 #Remove scan 22, 25, 28
  each_target[["Eriophorum angustifolium"]] <- each_target[["Eriophorum angustifolium"]][-c(22, 25, 28), ]
# plot_interactive(each_target[["Eriophorum vaginatum"]])#41      Eriophorum vaginatum    8 #Remove scans 16
  each_target[["Eriophorum vaginatum"]] <- each_target[["Eriophorum vaginatum"]][-c(16), ]
# plot_interactive(each_target[["Evernia mesomorpha"]])#42        Evernia mesomorpha   20 #Remove scan 8
  each_target[["Evernia mesomorpha"]] <- each_target[["Evernia mesomorpha"]][-c(8), ]
# plot_interactive(each_target[["Flavocetraria cucculata"]])#43   Flavocetraria cucculata   14 #Remove scan 7
  each_target[["Flavocetraria cucculata"]] <- each_target[["Flavocetraria cucculata"]][-c(7), ]
# plot_interactive(each_target[["Flavocetraria nivalis"]])#44     Flavocetraria nivalis   19 #Remove scan 11
  each_target[["Flavocetraria nivalis"]] <- each_target[["Flavocetraria nivalis"]][-c(11), ]
# plot_interactive(each_target[["Heracleum lanatum"]])#45         Heracleum lanatum    8
# plot_interactive(each_target[["Hieracium sp."]])#46             Hieracium sp.    1
# plot_interactive(each_target[["Hylocomium splendens"]])#47      Hylocomium splendens   13 #Removes scans 5-7
  each_target[["Hylocomium splendens"    ]]<-each_target[["Hylocomium splendens"    ]][-c(5:7),                            ]
# plot_interactive(each_target[["Hypogymnia austerodes"]])#48     Hypogymnia austerodes   13 #Remove 1,3 and 4
  each_target[["Hypogymnia austerodes"]] <- each_target[["Hypogymnia austerodes"]][-c(1, 3, 4), ]
#plot_interactive(each_target[["Icmadophila ericetorum"]])#49    Icmadophila ericetorum    8#Update: Why is this species no longer there? #Remove scan 7
# plot_interactive(each_target[["Iris sp."]])#50                  Iris sp.    4
# plot_interactive(each_target[["Ledum decumbens"]])#51           Ledum decumbens   22 #Remove 12-14
  each_target[["Ledum decumbens"]] <- each_target[["Ledum decumbens"]][-c(12:14), ]
# plot_interactive(each_target[["Loisleuria procumbens"]])#52     Loisleuria procumbens    6 #Remove scan 1
  each_target[["Loisleuria procumbens"]] <- each_target[["Loisleuria procumbens"]][-c(1), ]
# plot_interactive(each_target[["Lupinus sp."]])#53               Lupinus sp.   11
# plot_interactive(each_target[["Masonhalea richardsonii"]])#54   Masonhalea richardsonii   14
# plot_interactive(each_target[["Melanelia sp."]])#55             Melanelia sp.   13
# plot_interactive(each_target[["Nephroma arcticum"]])#56         Nephroma arcticum   17 #Remove 10-14
  each_target[["Nephroma arcticum"]] <- each_target[["Nephroma arcticum"]][-c(10:14), ]
# plot_interactive(each_target[["Parmelia omphalodes"]])#57       Parmelia omphalodes    4
# plot_interactive(each_target[["Parmeliopsis ambigua"]])#58      Parmeliopsis ambigua    4
# plot_interactive(each_target[["Parmelis sulcata"]])#59          Parmelis sulcata   12
# plot_interactive(each_target[["Pedicularis racemosa"]])#60      Pedicularis racemosa   11 #Remove scan 1
  each_target[["Pedicularis racemosa"]] <- each_target[["Pedicularis racemosa"]][-c(1), ]
# plot_interactive(each_target[["Pedicularis sudetica"]])#61      Pedicularis sudetica    4
# plot_interactive(each_target[["Peltigera apthosa"]])#62         Peltigera apthosa   14 #Remove 2-5
  each_target[["Peltigera apthosa"]] <- each_target[["Peltigera apthosa"]][-c(2:5), ]
# plot_interactive(each_target[["Peltigera malacea"]])#63         Peltigera malacea    4
# plot_interactive(each_target[["Peltigera scabrata"]])#64        Peltigera scabrata    7
# plot_interactive(each_target[["Peltigers leucophlebia"]])#65    Peltigers leucophlebia    4
# plot_interactive(each_target[["Pestasites frigidus"]])#66       Pestasites frigidus   62 #Remove scans 1:10
   each_target[["Pestasites frigidus"]] <- each_target[["Pestasites frigidus"]][-c(1:10), ]
# plot_interactive(each_target[["Picea mariana"]])#68             Picea mariana   17
# plot_interactive(each_target[["Pices (bark)"]])#69              Pices (bark)    5
# plot_interactive(each_target[["Pilophorus acicularis"]])#70     Pilophorus acicularis   15
# plot_interactive(each_target[["Plagiomnium sp."]])#71           Plagiomnium sp.    4
#plot_interactive(each_target[["Pleurozium schreberi"]])#72      Pleurozium schreberi    4
# plot_interactive(each_target[["Polytrichum juniperinum"]])#73   Polytrichum juniperinum   10 #Remove scans 1-4
  each_target[["Polytrichum juniperinum"]] <- each_target[["Polytrichum juniperinum"]][-c(1:4), ]
# plot_interactive(each_target[["Polytrichum sp."]])#74           Polytrichum sp.   13
# plot_interactive(each_target[["Populus balsamifera"]])#75       Populus balsamifera    8
# plot_interactive(each_target[["Porpidia sp."]])#76              Porpidia sp.   11
# plot_interactive(each_target[["Quartz"]])#77                    Quartz   25 #
# plot_interactive(each_target[["Racomitrium lanoiginosum"]])#78  Racomitrium lanoiginosum    4
# plot_interactive(each_target[["Rhizocarpon geographicum"]])#79  Rhizocarpon geographicum   18
# plot_interactive(each_target[["Rhizocarpon sp."]])#80           Rhizocarpon sp.    3
# plot_interactive(each_target[["Rhytidum rugosum"]])#81          Rhytidum rugosum    6
# plot_interactive(each_target[["Rosa acicularis"]])#82           Rosa acicularis   19
# plot_interactive(each_target[["Rubus sp."]])#83                 Rubus sp.   18 : Remove scans 1-4
  each_target[["Rubus sp."]]<-each_target[["Rubus sp."]][-c(1:4),                              ]
# plot_interactive(each_target[["Salix (wooly)"]])#84             Salix (wooly)   10
# plot_interactive(each_target[["Salix alaxensis"]])#85           Salix alaxensis   47
# plot_interactive(each_target[["Salix arbusculoides"]])#86       Salix arbusculoides    4
# plot_interactive(each_target[["Salix glauca"]])#87              Salix glauca   16
# plot_interactive(each_target[["Salix lanata"]])#88              Salix lanata    4
# plot_interactive(each_target[["Salix ovalifolia"]])#89          Salix ovalifolia    6
# plot_interactive(each_target[["Salix phlebophylla"]])#90        Salix phlebophylla    3
# plot_interactive(each_target[["Salix pulchra"]])#91             Salix pulchra   40 #Two distinct groups of scans, remove 1:20
  each_target[["Salix pulchra"]] <- each_target[["Salix pulchra"]][-c(1:20), ]
# plot_interactive(each_target[["Salix richardsonii"]])#92        Salix richardsonii    9
# plot_interactive(each_target[["Saxifraga punctata"]])#93        Saxifraga punctata   16 Two groups: Remove scans 1:4
  each_target[["Saxifraga punctata"]] <- each_target[["Saxifraga punctata"]][-c(1:4), ]
# #plot_interactive(each_target[["Sphagnum fuscum"]])#94           Sphagnum fuscum    4 #Update: Where did this species go?
# plot_interactive(each_target[["Sphagnum sp."]])#95              Sphagnum sp.    8
# plot_interactive(each_target[["Stereocaulon sp."]])#96          Stereocaulon sp.    8
# plot_interactive(each_target[["Toefeldia sp."]])#97             Toefeldia sp.    5 # Remove scan 5
  each_target[["Toefeldia sp."]]<-each_target[["Toefeldia sp."]][-c(5),]
# plot_interactive(each_target[["Tomenthypnum nitens"]])#98       Tomenthypnum nitens    2
# plot_interactive(each_target[["Trapelopsis granulosa"]])#99     Trapelopsis granulosa    5 #Remove scans 1-2
# plot_interactive(each_target[["Umbilicaria arctica"]])#100      Umbilicaria arctica    4
# plot_interactive(each_target[["Umbilicaria hyperborea"]])#101   Umbilicaria hyperborea   15
# plot_interactive(each_target[["Usnea lapponica"]])#102          Usnea lapponica   12
# plot_interactive(each_target[["Usnea scabrata"]])#103           Usnea scabrata   12 #Remove scans 9-12
  each_target[["Usnea scabrata"]]<-each_target[["Usnea scabrata"]][-c(9:12),]
# plot_interactive(each_target[["Vaccinium uliginosum"]])#104     Vaccinium uliginosum   16 #Two groups .. remove scans 1-10
  each_target[["Vaccinium uliginosum"]]<-each_target[["Vaccinium uliginosum"]][-c(1:10),]
# plot_interactive(each_target[["Vaccinium vitis-idea"]])#105     Vaccinium vitis-idea   25 #Remove scans 1-8
  each_target[["Vaccinium vitis-idea"]] <- each_target[["Vaccinium vitis-idea"]][-c(1:8), ]
# plot_interactive(each_target[["Vulpicida pinastri"]])#106       Vulpicida pinastri   12 #Remove scan 11
  each_target[["Vulpicida pinastri"      ]]<-each_target[["Vulpicida pinastri"      ]][-c(11),                           ]
 

# Creates a new object with cleaned spectral library
New_targets <- each_target

# Remove scans that are Epiphytes
New_targets[c(
  "Vulpicida pinastri",
  "Usnea scabrata",
  "Usnea lapponica",
  "Parmelis sulcata",
  "Hypogymnia austerodes",
  "Evernia mesomorpha",
  "Flavocetraria cucculata",
  "Bryoria sp."
)] <- NULL

Speclib_out <- Reduce(spectrolab::combine, New_targets) %>% 
  as.data.frame() %>% # dplyr::select(Area)# Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)


Speclib_out %>% dplyr::select(-1:-38) %>% as.matrix() %>% hist()

Speclib_out %>% group_by(Area,Functional_group1) %>% tally() %>% pivot_wider(names_from = Functional_group1, values_from = n)

meta_names<-colnames(Speclib_out[,1:10])
SpecLib_new <- filter_all_between(
  Speclib_out,
  0,
  100,
  ignore_cols = meta_names
) #%>% str()

SpecLib_new %>% group_by(Area,Functional_group1) %>% tally() %>% pivot_wider(names_from = Functional_group1, values_from = n)

SpecLib_new %>% dplyr::select(-1:-10) %>% as.matrix() %>% hist()

print(paste0(
  "Filtered data from  ",
  nrow(Speclib_out),
  " rows to ",
  nrow(SpecLib_new),
  " rows."
))




# Combines all species into one spectral library if satisfied with our results
MissingSpecLib <- read.csv("./Output/C_001_SC3_Missing_Cleaned_SpectralLib.csv", check.names = F) #%>% 
  #dplyr::select(-sample_num)
#MissingSpecLib$sample_num<-NULL
#MissingSpecLib %>% dplyr::select(colnames(SpecLib_new))
MissingSpecLib<-MissingSpecLib[,-1]
MissingSpecLib %>% dplyr::select(-1:-30) %>% as.matrix() %>% hist()
#MissingSpecLib_out <- columnwise_min_max_scale(MissingSpecLib, ignore_cols = meta_names) #%>% str()
#MissingSpecLib_VNIR_out <- columnwise_min_max_scale(MissingSpecLib_VNIR, ignore_cols = meta_names) #%>% str()
meta_names<-colnames(MissingSpecLib[,1:31])
MissingSpecLib_new <- filter_all_between(
  as.data.frame(MissingSpecLib),
  0,
  100,
  ignore_cols = meta_names
) #%>% str()

print(paste0(
  "Filtered data from  ",
  nrow(MissingSpecLib),
  " rows to ",
  nrow(MissingSpecLib_new),
  " rows."
))


AllSpecRaw<-bind_rows(SpecLib_new, MissingSpecLib_new)

meta_names<-colnames(AllSpecRaw[,1:10])

AllSpecRaw_tall<-AllSpecRaw %>%
pivot_longer(cols = `350`:`2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%    
  mutate(Wavelength = gsub("X","",Wavelength)) %>%
  group_by(Functional_group1, Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength))  %>%
  as.data.frame() #%>%
 
 jpeg("Output/AllSpecRaw_VNIR_quantiles.jpg", height = 10000, width = 9000, res = 350)
 
 ggplot(AllSpecRaw_tall %>% dplyr::filter(Wavelength>424&Wavelength<1000), aes(Wavelength, Median_Reflectance, group = Functional_group1), scales = "fixed")+
   labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 1242 scans"), y="Reflectance")+
   theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
         #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
         legend.position = "none",
         title = element_text(size=25),
         strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+  
   geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +
   geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +
  facet_wrap(vars(Functional_group1), scales = "fixed", ncol = 3) 

dev.off()


#AllSpecRaw_minmax_resscale <- columnwise_min_max_scale(AllSpecRaw, ignore_cols = meta_names)#%>% str()
#meta_names<-colnames(AllSpecRaw_minmax_resscale[,1:11])
#as_spectra(AllSpecRaw_minmax_resscale %>% dplyr::select(-meta_names))  %>% as.matrix() %>% hist()

#AllSpecRaw_minmax_resscale_tall<- AllSpecRaw_minmax_resscale %>%
#  pivot_longer(cols = `350`:`2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%    
#  mutate(Wavelength = gsub("X","",Wavelength)) %>%
#  group_by(Functional_group1, Wavelength) %>%  
#  dplyr::summarise(Median_Reflectance = median(Reflectance),
#                   Max_Reflectance = max(Reflectance),
#                   Min_Reflectance = min(Reflectance),
#                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
#                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
#                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
#                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
#  mutate(Wavelength = as.numeric(Wavelength))  %>%
#  as.data.frame() %>%

  #jpeg("Output/MissingSpecLib_out_tall_minmax_median_PFT_refl.jpg", height = 10000, width = 9000, res = 350)
  #
  #ggplot(MissingSpecLib_out_tall %>% dplyr::filter(Functional_group1 == "TreeBroadleaf"), 
  #       aes(Wavelength, Median_Reflectance, group = Functional_group1), scales = "fixed")+
  #  labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 1242 scans"), y="Reflectance")+
  #  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
  #        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
  #        legend.position = "none",
  #        title = element_text(size=25),
  #        strip.text = element_text(size = 25),
#        axis.text = element_text(size = 20),
#        axis.text.x = element_text(angle = 90)) +
#  geom_point(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+
#  facet_wrap(vars(Functional_group1), scales = "fixed", ncol = 3) 
#
#dev.off()
##

write.csv(AllSpecRaw, "./Output/C_001_SC3_Cleaned_SpectralLib.csv")

#saveRDS(Cleaned_Speclib_rds, "./Output/C_002_SC3_Cleaned_Speclib.rds")


# Make a table showing all the scans tallied by the three groups.

AllSpecRaw %>%
  #dplyr::filter(is.na(File.Name.1) == FALSE) %>%
  group_by(Functional_group1, Functional_group2) %>% # ,Species_name) %>%
  tally() %>%
  as.data.frame() %>% # View()
  write.csv("./Output/C_003_SC3_Cleaned_SpectralLib_table.csv")

AllSpecRaw %>% group_by(Area,Functional_group1) %>% tally() %>% pivot_wider(names_from = Functional_group1, values_from = n)

### Run LandCoverEstimator to generate Spectral Derivatives.
# source("Functions/1_Simple_LandCoverEstimator.R")
# source("Functions/2_Simple_LandCoverEstimator.R")
source("Functions/1_LCE_derivs.R")
source("Functions/2_LCE_veg_index.R")
source("Functions/lecospectR.R")

out_file <- "Data/"

Make_Speclib_Derivs("./Output/C_001_SC3_Cleaned_SpectralLib.csv", out_file)
# Make_Speclib_Derivs("Output/C_001_SC3_Cleaned_SpectralLib.csv",out_file="Output/")
# Make_Speclib_Derivs("Output/C_001_SC3_Cleaned_SpectralLib4.csv", out_file = "Output/resampled/")
# Make_Speclib_Derivs("Output/C_001_SC3_Cleaned_SpectralLib29.csv", out_file = "Output/resampled/FncGrp2/")
# "Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv"

