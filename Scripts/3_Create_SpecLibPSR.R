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
# ------------------------------------------------ Step1: Data Munging Step ------------------------------------------------- # 
# In this step we will combine all our spectral profiles to form one spectral library

# Creates a file path to where our spectral libraries for each site is loacated
mypath = "./Output/"

# Reads in species and functional level groups dataframe creatd in script 1
Species_groups<-read.csv("./Data/SpeciesTable_20220113.csv")

# Reads in scans from ecosis website
# Additional spectral profiles that will be combined to the ones collected in Alaska

Spectra_Ecosis_1<-read.csv("Data/leaf_spectra_barrow_2013_20180824_ecosis.csv", check.names = F)
Metadata_Ecosis_1<-read.csv("Data/ngeearctic_bnl_2013_leaf_spectra_traits_metadata.csv", check.names = F)

# Merges meta and spectra
Ecosis_data_1<-cbind(Metadata_Ecosis_1[,-3],Spectra_Ecosis_1[,-1])
range(Ecosis_data_1$`1000`)
# Change name of column and add an area column
names(Ecosis_data_1)[2]<-"Code_name"
names(Ecosis_data_1)[1]<-"ScanID"
Ecosis_data_1$Area<- "NOT RECORDED"

Ecosis_data_1$ScanID = as.character(Ecosis_data_1$ScanID)

Ecosis_data_1 %>% 
left_join(Species_groups,by="Code_name", keep=FALSE) %>%
  group_by(Species_name) %>% tally()
  #Joins dataframe with all the species info to our spectral library
  

# A tibble: 9 × 2
#Code_name     n
#<chr>     <int>
#  1 ARFU2         5
#2 ARLA2         5
#3 CAAQ          8
#4 DUFI         10
#5 ERAN6        10
#6 PEFR5        10
#7 SAPU15       10
#8 SAPU6        10
#9 VAVI          1

###

Spectra_Ecosis_2<-read.csv("Data/sewpen_2019_canopy_spectral_reflectance.csv", check.names = F)
Metadata_Ecosis_2<-read.csv("Data/sewpen_2019_canopy_spectral_reflectance_metadata.csv", check.names = F)

Ecosis_data_2<- inner_join(Metadata_Ecosis_2,Spectra_Ecosis_2, by = "SampleID", keep=FALSE)
#Ecosis_data_2<-bind_cols(Metadata_Ecosis_2,Spectra_Ecosis_2, .id = "id")
# Merges meta and spectra
#Ecosis_data_1<-cbind(Metadata_Ecosis_1[,-3],Spectra_Ecosis_1[,-1])

# Change name of column and add an area column
Ecosis_data_2<-Ecosis_data_2 %>%
  dplyr::rename(Code_name = Dominant_Species, 
         ScanID = SampleID) %>%
  mutate(Area = "Seward_Penn",
         ScanID = as.character(ScanID))
Ecosis_data_2<-Ecosis_data_2 %>% dplyr::mutate(dplyr::across(.cols = `350`:`2500`, ~.x/100, na.rm=TRUE)) 
#names(Ecosis_data_1$Dominant_Species)<-"Code_name"
#names(Ecosis_data_1)[1]<-"ScanID"
#Ecosis_data_1$Area<- "NOT RECORDED"

Eco1_names<-colnames(Ecosis_data_1)
Ecosis_data_2<-Ecosis_data_2 %>% dplyr::select(Eco1_names)

Ecosis_data_2_missing_names<-Ecosis_data_2 %>%
anti_join(Species_groups,by="Code_name", keep=FALSE) %>%
  group_by(Code_name) %>% tally() 

###
Spectra_Ecosis_3_1<-read.csv("Data/ngee-arctic_2014_barrow_svchr1024i_canopy_spectral_reflectance.csv", check.names = F)
Spectra_Ecosis_3_2<-read.csv("Data/ngee-arctic_2015_barrow_svchr1024i_canopy_spectral_reflectance.csv", check.names = F)

Spectra_Ecosis_3<-bind_rows(Spectra_Ecosis_3_1,Spectra_Ecosis_3_2)

Metadata_Ecosis_3<-read.csv("Data/ngee-arctic_2014_to_2016_barrow_canopy_spectral_reflectance_metadata.csv", check.names = F)

Ecosis_data_3<- Metadata_Ecosis_3 %>% 
  inner_join(Spectra_Ecosis_3, by = "Sample_ID" , keep=FALSE) #%>%
#  left_join(Spectra_Ecosis_3_2, by ="Sample_ID", keep = FALSE)
#Ecosis_data_3 %>% filter(is.na(`350`)==FALSE) %>% dim
#dim(Ecosis_data_3)
#Ecosis_data_3<-bind_cols(Metadata_Ecosis_3,Spectra_Ecosis_3, .id = "id")
# Merges meta and spectra
#Ecosis_data_1<-cbind(Metadata_Ecosis_1[,-3],Spectra_Ecosis_1[,-1])

# Change name of column and add an area column
Ecosis_data_3<-Ecosis_data_3 %>%
  dplyr::rename(Code_name = USDA_Species_Code, 
                ScanID = Sample_ID) %>%
  mutate(Area = "Barrow",
         ScanID = as.character(ScanID))
#names(Ecosis_data_1$Dominant_Species)<-"Code_name"
#names(Ecosis_data_1)[1]<-"ScanID"
#Ecosis_data_1$Area<- "NOT RECORDED"
Ecosis_data_3<-Ecosis_data_3 %>% dplyr::select(Eco1_names)
range(Ecosis_data_3$`1000`)

Ecosis_data_3<-Ecosis_data_3 %>% dplyr::mutate(dplyr::across(.cols = `350`:`2500`, ~.x/100, na.rm=TRUE)) 

Ecosis_data_3 %>%
  anti_join(Species_groups,by="Code_name", keep=FALSE) %>%
  group_by(Code_name) %>% tally()


Ecosis_data_all<-bind_rows(Ecosis_data_1,Ecosis_data_2,Ecosis_data_3)


colnames(Ecosis_data_all)
# Creates a spectral library from ecosis data
Ecosis_data<-Ecosis_data_all %>% 
  inner_join(Species_groups,by="Code_name") %>% #head
  dplyr::select(ScanID,
               Code_name,
               Species_name,
               Functional_group1,
               Functionalgroup2,
               Area,
               everything()) %>%
  rename(Functional_group2 = Functionalgroup2)

Ecosis_data$ %>% group_by(Functional_group1) %>% tally()

#Check EcoSIS data


          Target_names<-unique(sort(Ecosis_data$Species_name))
          
          # Creates an empty list
          each_target<-list()
          
          # Function splits the spectral library into spectral objects based on each target (105 Spectral Objects)
          for(i in 1:length(Target_names)){
            
            # Subset a functional group
            each_target[[i]]<-subset(Ecosis_data,Species_name == Target_names[i])
            
            # saves metadata
            metadata<-each_target[[i]][,c(1:6)]%>%as.data.frame()
            
            # Convert to a spectral object
            each_target[[i]] <- as_spectra(each_target[[i]][-1:-6])
            
            # Add metadata
            meta(each_target[[i]])<-data.frame(metadata[,c(1:6)], stringsAsFactors = FALSE)
            
          }
          
          # Renames each target in list 
          each_target<-each_target%>%setNames(Target_names)



###Read in our data by site
# Import file path names of .rds files into character list (Spectral libraries based on each location in alaska) 
SpecLib_by_location = list.files(mypath, pattern="A_0",full.names = T) 

# Reads in the spectral libraries for each location in a list...List of 13 spectral objects
list_of_SpecLib<-lapply(SpecLib_by_location,readRDS)%>% # Reads in the spectral library for each site 
  setNames(gsub("./Output/","",SpecLib_by_location)) # Removes dir path from the name

# Combines specral libraries from all locations
SpecLib_raw<-Reduce(spectrolab::combine,list_of_SpecLib) #%>% # dim(n_samples=1989, n_wavelegths=2151)

Speclib_metadata<-meta(SpecLib_raw)

write_csv(Speclib_metadata,"./Output/C_000_Speclib_raw_metadata.csv")

SpecLib<- SpecLib_raw %>%
  as.data.frame() %>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name) %>% # Removes unwanted column ~ should remove this later instead
  left_join(Species_groups,by="Code_name") %>% #Joins dataframe with all the species info to our spectral library
  dplyr::select(ScanID,Code_name,Species_name,Functional_group1,Functional_group2,Area,everything()) #Reorders columns 


# Combines Ecosis data and spectral library
SpecLib_out<-bind_rows(SpecLib,Ecosis_data)

## Please note: these are the number of samples we have for each functional group
# table(SpecLib_out$Functional_group1)%>%as.data.frame()
#                         Var1  Freq
#            Dwarf_Shrub_Decid  968
#                         Forb   80
#              Graminoid_Grass   24
#              Graminoid_Sedge   44
#         Lichen_Crustose_Dark   32
#        Lichen_Crustose_Light   28
#         Lichen_Epiphyte_Dark   23
#       Lichen_Epiphyte_Yellow   56
#          Lichen_Foliose_Dark   46
#Lichen_Foliose_Dark_Peltigera   29
#         Lichen_Foliose_Light   16
#        Lichen_Foliose_Yellow   53
#        Lichen_Fruticose_Dark   43
#       Lichen_Fruticose_Light   46
#      Lichen_Fruticose_Yellow   97
#                       Litter    8
#             Moss_Aulacomnium   12
#               Moss_Ceratadon    5
#                Moss_Dicranum    5
#             Moss_Hylocomnium   13
#             Moss_Plagiomnium    4
#              Moss_Pleurozium    4
#             Moss_Polytrichum   23
#             Moss_Racomitrium    4
#               Moss_Rhytidium    6
#         Moss_Sphagnum_fuscum    4
#          Moss_Sphagnum_other    8
#            Moss_Tomenthypnum    2
#                         Rock   32
#                  Shrub_Alder   44
#                 Shrub_Betula   56
#              Shrub_Evergreen   65
#                   Shrub_Rosa   20
#                  Shrub_Salix  111
#                         Soil   13
#                   Tree_Decid   12
#               Tree_Evergreen   17
#                  Wood_Coarse    5


# Removes all the rows with negative values or Values >2
SpecLib_new <-  SpecLib_out %>% 
  dplyr::filter(if_all(7:ncol(SpecLib_out), ~between(., 0, 1))) #%>% dim()

print(paste0("Filtered data from  ", nrow(SpecLib_out), " rows to ", nrow(SpecLib_new), " rows."))

# table(SpecLib_new$Functional_group1)%>%as.data.frame()

# Adds more details to our spectral library (Freq columns = The count of each Species)
# Frequency values represent the number of scans per species and the number of scans per functional group
SpecLib_new_All<- SpecLib_new %>% #SpecLib_new%>%
  plyr::ddply( .(Species_name), mutate, Species_name_Freq = length(Species_name))%>% # Add column to data frame that shows frequency of species
  plyr::ddply( .(Functional_group1), mutate, Functional_group1_Freq = length(Functional_group1))%>% # Add column to data frame that shows frequency of functional group
  plyr::ddply( .(Functional_group2), mutate, Functional_group2_Freq = length(Functional_group2))%>% # Add column to data frame that shows frequency of courser functional groups
  dplyr::select(ScanID,Area,Code_name,Species_name,Functional_group1,Functional_group2,Area,Species_name_Freq,Functional_group1_Freq,Functional_group2_Freq,everything()) # Rearrange columns 

# Removes all unknown scans
SpecLib_new_All<-SpecLib_new_All[!(SpecLib_new_All$Species_name=="Unknown"),]
# Filter out rows with really low reflectance at a band for which all surfaces should have elevated reflectance
SpecLib_new_All<-SpecLib_new_All %>% dplyr::filter(`1250`>0.1) %>%
  mutate(Species_name=replace(Species_name, Species_name=="Cladonia steallaris", "Cladonia stellaris")) %>%
  mutate(Species_name=replace(Species_name, Species_name=="Petasites frigida", "Petasites frigidus"))

dim(SpecLib_new_All)
# ------------------------------------------------- Step 2: Spectral Library Clean Up -------------------------------------------------- #
# This section pf the script removes weird scans from each species in the combines spectral library
#table(SpecLib_new_All$Species_name)%>%as.data.frame() # There are 105 species in our spectral library

 
# Creates an object with all the species names sorted in alphabetical order
Target_names<-unique(sort(SpecLib_new_All$Species_name))

# Creates an empty list
each_target<-list()

# Function splits the spectral library into spectral objects based on each target (105 Spectral Objects)
for(i in 1:length(Target_names)){
  
  # Subset a functional group
  each_target[[i]]<-subset(SpecLib_new_All,Species_name == Target_names[i])
  
  # saves metadata
  metadata<-each_target[[i]][,c(1:9)]%>%as.data.frame()
  
  # Convert to a spectral object
  each_target[[i]] <- as_spectra(each_target[[i]][-1:-9])
  #each_target[[i]] <-normalize(each_target[[i]])
  # Add metadata
  meta(each_target[[i]])<-data.frame(metadata[,c(1:9)], stringsAsFactors = FALSE)
  
}

# Renames each target in list 
each_target<-each_target%>%setNames(Target_names)

#Var1 Freq
#plot_interactive(each_target[["Alectoria ochroleuca"]])#1       Alectoria ochroleuca    6 #Remove scan 2
#plot_interactive(each_target[["Alnus sp."]])#2                  Alnus sp.   80 # Need to clean out half of these because there are two groups.
#plot_interactive(each_target[["Arctagrostis latifolia"]])#3     Arctagrostis latifolia    5
#plot_interactive(each_target[["Arctocetraria centrifuga"]])#4   Arctocetraria centrifuga    4
#plot_interactive(each_target[["Arctophila fulva"]])#5           Arctophila fulva   12
#plot_interactive(each_target[["Arctostaphyllos"]])#6            Arctostaphyllos   25 #Mixed of two things, Remove scans 20:25 based on SWIR detector edge artifact
#plot_interactive(each_target[["Arenaria pseudofrigida"]])#7     Arenaria pseudofrigida    1
#plot_interactive(each_target[["Asahinea chrysantha"]])#8        Asahinea chrysantha   19
#plot_interactive(each_target[["Aulacomnium palustre"]])#9       Aulacomnium palustre    6
#plot_interactive(each_target[["Aulacomnium turgidum"]])#10      Aulacomnium turgidum    6
#plot_interactive(each_target[["Bare Rock"]])#11                 Bare Rock    7
#plot_interactive(each_target[["Bare Soil"]])#12                 Bare Soil   13
#plot_interactive(each_target[["Betula nana"]])#13               Betula nana   65 #Remove scan 37
#plot_interactive(each_target[["Betula neoalaskana"]])#14        Betula neoalaskana    4
#plot_interactive(each_target[["Bryoria sp."]])#15               Bryoria sp.   10
#plot_interactive(each_target[["Calamogrostis sp."]])#16         Calamogrostis sp.    4
#plot_interactive(each_target[["Carex aquatilis"]])#17           Carex aquatilis   36 #Remove scan 36 but looks like two populations
#plot_interactive(each_target[["Carex sp."]])#18                 Carex sp.   16 #Remove scan 10
#plot_interactive(each_target[["Cassiope tetragona"]])#19        Cassiope tetragona    9 
#plot_interactive(each_target[["Ceratadon purpureus"]])#20       Ceratadon purpureus    5
#plot_interactive(each_target[["Cetraria islandica"]])#21        Cetraria islandica   14 #Remove 1,2 and 8
#plot_interactive(each_target[["Cetraria laevigata"]])#22        Cetraria laevigata    3
#plot_interactive(each_target[["Cladonia amaurocraea"]])#23      Cladonia amaurocraea    6
#plot_interactive(each_target[["Cladonia cornuta"]])#24          Cladonia cornuta    3
#plot_interactive(each_target[["Cladonia gracilis"]])#25         Cladonia gracilis   18 #Remove scans 13-18
#plot_interactive(each_target[["Cladonia mitis"]])#26            Cladonia mitis   17
#plot_interactive(each_target[["Cladonia rangiferina"]])#27      Cladonia rangiferina   20 #Two groups, esp 350-400n, remove scans 1:4
#plot_interactive(each_target[["Cladonia stellaris"]])#28       Cladonia stellaris   20
#plot_interactive(each_target[["Cladonia stygia"]])#29           Cladonia stygia   18 #Remove scan 13
#plot_interactive(each_target[["Cladonia sulphurina"]])#30       Cladonia sulphurina    3
#plot_interactive(each_target[["Cladonia uncialis"]])#31         Cladonia uncialis   10  #Remove scan 6
#plot_interactive(each_target[["Dactylina arctica"]])#32         Dactylina arctica    7
#plot_interactive(each_target[["Dead Salix"]])#33                Dead Salix    8
#plot_interactive(each_target[["Dicranum sp."]])#34              Dicranum sp.    5
#plot_interactive(each_target[["Dryas sp."]])#35                 Dryas sp.  925
#plot_interactive(each_target[["Dupontia fisheri"]])#36          Dupontia fisheri   10
#plot_interactive(each_target[["Empetrum nigrum"]])#37           Empetrum nigrum   14 #Remove scan 1
#plot_interactive(each_target[["Equisetum arvense"]])#38         Equisetum arvense    7
#plot_interactive(each_target[["Equisetum sylvaticum"]])#39      Equisetum sylvaticum    4
#plot_interactive(each_target[["Eriophorum angustifolium"]])#40  Eriophorum angustifolium   44 #Remove scan 19,21,23
#plot_interactive(each_target[["Eriophorum vaginatum"]])#41      Eriophorum vaginatum    8 #Remove scans 1 and 2
#plot_interactive(each_target[["Evernia mesomorpha"]])#42        Evernia mesomorpha   20 #Remove scan 8
#plot_interactive(each_target[["Flavocetraria cucculata"]])#43   Flavocetraria cucculata   14 #Remove scan 12
#plot_interactive(each_target[["Flavocetraria nivalis"]])#44     Flavocetraria nivalis   19 #Remove scan 11
#plot_interactive(each_target[["Heracleum lanatum"]])#45         Heracleum lanatum    8
#plot_interactive(each_target[["Hieracium sp."]])#46             Hieracium sp.    1
#plot_interactive(each_target[["Hylocomium splendens"]])#47      Hylocomium splendens   13
#plot_interactive(each_target[["Hypogymnia austerodes"]])#48     Hypogymnia austerodes   13 #Remove 1,3 and 4
#plot_interactive(each_target[["Icmadophila ericetorum"]])#49    Icmadophila ericetorum    8 #Remove scan 7
#plot_interactive(each_target[["Iris sp."]])#50                  Iris sp.    4
#plot_interactive(each_target[["Ledum decumbens"]])#51           Ledum decumbens   22 #Remove 20 through 22
#plot_interactive(each_target[["Loisleuria procumbens"]])#52     Loisleuria procumbens    6
#plot_interactive(each_target[["Lupinus sp."]])#53               Lupinus sp.   11
#plot_interactive(each_target[["Masonhalea richardsonii"]])#54   Masonhalea richardsonii   14
#plot_interactive(each_target[["Melanelia sp."]])#55             Melanelia sp.   13
#plot_interactive(each_target[["Nephroma arcticum"]])#56         Nephroma arcticum   17 #Remove 10-14
#plot_interactive(each_target[["Parmelia omphalodes"]])#57       Parmelia omphalodes    4
#plot_interactive(each_target[["Parmeliopsis ambigua"]])#58      Parmeliopsis ambigua    4
#plot_interactive(each_target[["Parmelis sulcata"]])#59          Parmelis sulcata   12
#plot_interactive(each_target[["Pedicularis racemosa"]])#60      Pedicularis racemosa   11 #Remove scan 11
#plot_interactive(each_target[["Pedicularis sudetica"]])#61      Pedicularis sudetica    4
#plot_interactive(each_target[["Peltigera apthosa"]])#62         Peltigera apthosa   14 #Remove 2-4
#plot_interactive(each_target[["Peltigera malacea"]])#63         Peltigera malacea    4
#plot_interactive(each_target[["Peltigera scabrata"]])#64        Peltigera scabrata    7
#plot_interactive(each_target[["Peltigers leucophlebia"]])#65    Peltigers leucophlebia    4
#plot_interactive(each_target[["Pestasites frigidus"]])#66       Pestasites frigidus   62
#plot_interactive(each_target[["Picea mariana"]])#68             Picea mariana   17
#plot_interactive(each_target[["Pices (bark)"]])#69              Pices (bark)    5
#plot_interactive(each_target[["Pilophorus acicularis"]])#70     Pilophorus acicularis   15
#plot_interactive(each_target[["Plagiomnium sp."]])#71           Plagiomnium sp.    4
#plot_interactive(each_target[["Pleurozium schreberi"]])#72      Pleurozium schreberi    4
#plot_interactive(each_target[["Polytrichum juniperinum"]])#73   Polytrichum juniperinum   10 #Remove scans 1-4
#plot_interactive(each_target[["Polytrichum sp."]])#74           Polytrichum sp.   13
#plot_interactive(each_target[["Populus balsamifera"]])#75       Populus balsamifera    8
#plot_interactive(each_target[["Porpidia sp."]])#76              Porpidia sp.   11
#plot_interactive(each_target[["Quartz"]])#77                    Quartz   25 #Remove scan 4
#plot_interactive(each_target[["Racomitrium lanoiginosum"]])#78  Racomitrium lanoiginosum    4
#plot_interactive(each_target[["Rhizocarpon geographicum"]])#79  Rhizocarpon geographicum   18
#plot_interactive(each_target[["Rhizocarpon sp."]])#80           Rhizocarpon sp.    3
#plot_interactive(each_target[["Rhytidum rugosum"]])#81          Rhytidum rugosum    6
#plot_interactive(each_target[["Rosa acicularis"]])#82           Rosa acicularis   19
#plot_interactive(each_target[["Rubus sp."]])#83                 Rubus sp.   18
#plot_interactive(each_target[["Salix (wooly)"]])#84             Salix (wooly)   10
#plot_interactive(each_target[["Salix alaxensis"]])#85           Salix alaxensis   47
#plot_interactive(each_target[["Salix arbusculoides"]])#86       Salix arbusculoides    4
#plot_interactive(each_target[["Salix glauca"]])#87              Salix glauca   16
#plot_interactive(each_target[["Salix lanata"]])#88              Salix lanata    4
#plot_interactive(each_target[["Salix ovalifolia"]])#89          Salix ovalifolia    6
#plot_interactive(each_target[["Salix phlebophylla"]])#90        Salix phlebophylla    3
#plot_interactive(each_target[["Salix pulchra"]])#91             Salix pulchra   40 #Two distinct groups of scans, remove 21:40
#plot_interactive(each_target[["Salix richardsonii"]])#92        Salix richardsonii    9
#plot_interactive(each_target[["Saxifraga punctata"]])#93        Saxifraga punctata   16
#plot_interactive(each_target[["Sphagnum fuscum"]])#94           Sphagnum fuscum    4
#plot_interactive(each_target[["Sphagnum sp."]])#95              Sphagnum sp.    8
#plot_interactive(each_target[["Stereocaulon sp."]])#96          Stereocaulon sp.    8
#plot_interactive(each_target[["Toefeldia sp."]])#97             Toefeldia sp.    5
#plot_interactive(each_target[["Tomenthypnum nitens"]])#98       Tomenthypnum nitens    2
#plot_interactive(each_target[["Trapelopsis granulosa"]])#99     Trapelopsis granulosa    5 #Remove scan 1
#plot_interactive(each_target[["Umbilicaria arctica"]])#100      Umbilicaria arctica    4
#plot_interactive(each_target[["Umbilicaria hyperborea"]])#101   Umbilicaria hyperborea   15
#plot_interactive(each_target[["Usnea lapponica"]])#102          Usnea lapponica   12
#plot_interactive(each_target[["Usnea scabrata"]])#103           Usnea scabrata   12
#plot_interactive(each_target[["Vaccinium uliginosum"]])#104     Vaccinium uliginosum   16
#plot_interactive(each_target[["Vaccinium vitis-idea"]])#105     Vaccinium vitis-idea   25 #Remove scans 1-8
#plot_interactive(each_target[["Vulpicida pinastri"]])#106       Vulpicida pinastri   12



# Hard code removes bad spectra taken for each species
each_target[["Alectoria ochroleuca"    ]]<-each_target[["Alectoria ochroleuca"    ]][-c(2),                                ]
each_target[["Arctostaphyllos"         ]]<-each_target[["Arctostaphyllos"         ]][-c(20:25)]#OLD[-c(19,13,10,11,16,17)                 ]
#each_target[["Asahinea chrysantha"     ]]<-each_target[["Asahinea chrysantha"     ]][-c(3),                                ]
#each_target[["Aulacomnium turgidum"    ]]<-each_target[["Aulacomnium turgidum"    ]][-c(3,4),                              ]
#each_target[["Bare Soil"               ]]<-each_target[["Bare Soil"               ]][-c(1,3,7),                            ]
each_target[["Betula nana"             ]]<-each_target[["Betula nana"             ]][-c(37),              ] #[-c(5,18,26,27,32,43,56),              ]
each_target[["Carex aquatilis"         ]]<-each_target[["Carex aquatilis"         ]][-c(36),                   ]#[-c(10,12,13,14,15),                   ]
each_target[["Carex sp."               ]]<-each_target[["Carex sp."               ]][-c(10),                   ]#[-c(10,12,13,14,15),                   ]
each_target[["Cetraria islandica"      ]]<-each_target[["Cetraria islandica"      ]][-c(1,2,8),                        ]
each_target[["Cladonia gracilis"       ]]<-each_target[["Cladonia gracilis"       ]][-c(13,14,15,16,17,18),                ]
#each_target[["Cladonia mitis"          ]]<-each_target[["Cladonia mitis"          ]][-c(8,12,13,14,15,16,17),              ]
#each_target[["Cladonia rangiferina"    ]]<-each_target[["Cladonia rangiferina"    ]][-c(1,2,3,4,17,18),                    ]
each_target[["Cladonia stellaris"     ]]<-each_target[["Cladonia stellaris"     ]][-c(1,2,3,4),                       ]
each_target[["Cladonia stygia"         ]]<-each_target[["Cladonia stygia"         ]][-c(13),                               ]
each_target[["Cladonia uncialis"       ]]<-each_target[["Cladonia uncialis"       ]][-c(6),                                ]
#each_target[["Dactylina arctica"       ]]<-each_target[["Dactylina arctica"       ]][-c(3,7),                              ]
#each_target[["Dead Salix"              ]]<-each_target[["Dead Salix"              ]][-c(3,4,8),                            ]
each_target[["Dryas sp."               ]]<-each_target[["Dryas sp."        ]][-c(10,11,29,33,32,38,41,42,48,61:925),]
each_target[["Empetrum nigrum"         ]]<-each_target[["Empetrum nigrum"         ]][-c(1),                              ]
each_target[["Eriophorum angustifolium"      ]]<-each_target[["Eriophorum angustifolium"      ]][-c(19,21,23),                  ]
each_target[["Eriophorum vaginatum"      ]]<-each_target[["Eriophorum vaginatum"      ]][-c(1,2),                  ]
each_target[["Evernia mesomorpha"      ]]<-each_target[["Evernia mesomorpha"      ]][-c(8),                  ]
each_target[["Flavocetraria cucculata" ]]<-each_target[["Flavocetraria cucculata" ]][-c(12),                          ]
each_target[["Flavocetraria nivalis"   ]]<-each_target[["Flavocetraria nivalis"   ]][-c(11),                               ]
#each_target[["Gravel"                  ]]<-each_target[["Gravel"                  ]][-c(5),                                 ] #Why is this gone?
#each_target[["Heracleum lanatum"       ]]<-each_target[["Heracleum lanatum"       ]][-c(2),                                ]
#each_target[["Hylocomium splendens"    ]]<-each_target[["Hylocomium splendens"    ]][-c(5,6,7),                            ]
each_target[["Hypogymnia austerodes"   ]]<-each_target[["Hypogymnia austerodes"   ]][-c(1,3,4),                          ]
each_target[["Icmadophila ericetorum"  ]]<-each_target[["Icmadophila ericetorum"  ]][-c(7),                              ]
each_target[["Iris sp."                ]]<-each_target[["Iris sp."                ]][-c(2),                                ]
each_target[["Ledum decumbens"             ]]<-each_target[["Ledum decumbens"             ]][-c(20,21,22),                             ]
#each_target[["Lupinus sp."             ]]<-each_target[["Lupinus sp."             ]][-c(1,12),                             ]
#each_target[["Masonhalea richardsonii" ]]<-each_target[["Masonhalea richardsonii" ]][-c(2),                                ]
each_target[["Nephroma arcticum"       ]]<-each_target[["Nephroma arcticum"       ]][-c(10,11,12,13,14),                      ]
#each_target[["Parmeliopsis ambigua"    ]]<-each_target[["Parmeliopsis ambigua"    ]][-c(4),                                ]
#each_target[["Parmelis sulcata"        ]]<-each_target[["Parmelis sulcata"        ]][-c(1,11),                             ]
each_target[["Pedicularis racemosa"    ]]<-each_target[["Pedicularis racemosa"    ]][-c(11),                              ]
each_target[["Peltigera apthosa"       ]]<-each_target[["Peltigera apthosa"       ]][-c(2,3,4),                            ]
each_target[["Polytrichum juniperinum" ]]<-each_target[["Polytrichum juniperinum" ]][-c(1,2,3,4),                          ]
#each_target[["Polytrichum sp."         ]]<-each_target[["Polytrichum sp."         ]][-c(1,4),                              ]
each_target[["Quartz"                  ]]<-each_target[["Quartz"                  ]][-c(4),                               ]
#each_target[["Rhizocarpon geographicum"]]<-each_target[["Rhizocarpon geographicum"]][-c(8,18),                             ]
#each_target[["Rosa acicularis"         ]]<-each_target[["Rosa acicularis"         ]][-c(19),                               ]
#each_target[["Rubus sp."               ]]<-each_target[["Rubus sp."               ]][-c(8,6),                              ]
each_target[["Salix pulchra"               ]]<-each_target[["Salix pulchra"               ]][-c(21:40),                              ]

#each_target[["Toefeldia sp."           ]]<-each_target[["Toefeldia sp."           ]][-c(5),                                ]
each_target[["Trapelopsis granulosa"   ]]<-each_target[["Trapelopsis granulosa"   ]][-c(1),                                ]
each_target[["Vaccinium vitis+--idea"  ]]<-each_target[["Vaccinium vitis-idea"    ]][-c(1,2,3,4,5,6,7,8),                  ]
#each_target[["Vulpicida pinastri"      ]]<-each_target[["Vulpicida pinastri"      ]][-c(1,2,11),                           ]

# Creates a new object with cleaned spectral library
New_targets<-each_target

# Remove scans that are Epiphytes
New_targets[c("Vulpicida pinastri"
              ,"Usnea scabrata"
              ,"Usnea lapponica"
              ,"Parmelis sulcata"
              ,"Hypogymnia austerodes"
              ,"Evernia mesomorpha"     
              ,"Flavocetraria cucculata"
              ,"Bryoria sp." )]<-NULL

# Combines all species into one spectral library if satisfied with our results
# The result is a dataframe
Cleaned_Speclib<-Reduce(spectrolab::combine,New_targets)%>% 
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)

# Creates .rds object
Cleaned_Speclib_rds<-Reduce(spectrolab::combine,New_targets)

table(Cleaned_Speclib$Functional_group2)%>%as.data.frame()

#Var1 Freq
#1              Dwarf_Shrub_Decid  107
#2                           Forb  139
#3                Graminoid_Grass   31
#4                Graminoid_Sedge   97
#5           Lichen_Crustose_Dark   32
#6          Lichen_Crustose_Light   26
#7            Lichen_Foliose_Dark   46
#8  Lichen_Foliose_Dark_Peltigera   26
#9           Lichen_Foliose_Light    4
#10         Lichen_Foliose_Yellow   39
#11         Lichen_Fruticose_Dark   29
#12        Lichen_Fruticose_Light   45
#13       Lichen_Fruticose_Yellow   81
#14                        Litter    8
#15              Moss_Aulacomnium   12
#16                Moss_Ceratadon    5
#17                 Moss_Dicranum    5
#18              Moss_Hylocomnium   13
#19              Moss_Plagiomnium    4
#20               Moss_Pleurozium    4
#21              Moss_Polytrichum   19
#22              Moss_Racomitrium    4
#23                Moss_Rhytidium    6
#24          Moss_Sphagnum_fuscum    4
#25           Moss_Sphagnum_other    8
#26             Moss_Tomenthypnum    2
#27                          Rock   31
#28                   Shrub_Alder   80
#29                  Shrub_Betula   64
#30               Shrub_Evergreen   89
#31                    Shrub_Rosa   19
#32                   Shrub_Salix  116
#33                          Soil   13
#34                    Tree_Decid   12
#35                Tree_Evergreen   17
#36                   Wood_Coarse    5

write.csv(Cleaned_Speclib, "./Output/C_001_SC3_Cleaned_SpectralLib.csv")
saveRDS(Cleaned_Speclib_rds,"./Output/C_002_SC3_Cleaned_Speclib.rds")




###Run LandCoverEstimator to generate Spectral Derivatives.
#source("Functions/1_Simple_LandCoverEstimator.R")
#source("Functions/2_Simple_LandCoverEstimator.R")
source("Functions/2_LCE_veg_index.R")

Make_Speclib_Derivs("Output/C_001_SC3_Cleaned_SpectralLib.csv",out_file="Output/")
#Make_Speclib_Derivs("Output/C_001_SC3_Cleaned_SpectralLib4.csv", out_file = "Output/resampled/")
#Make_Speclib_Derivs("Output/C_001_SC3_Cleaned_SpectralLib29.csv", out_file = "Output/resampled/FncGrp2/")

