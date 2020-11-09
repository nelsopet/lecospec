# ---- Script creates a spectral library object, combining all the scans collected in Alaksa during 2018 and 2019 ---------
library(plyr)
library(dplyr)
library(ggplot2)
library(spectrolab)
library(tidyverse)
library(raster)
library(SpaDES)
library(doParallel)
library(parallel)
library(hsdar)
library(caret)
library(ranger)
library(tools)
library(randomForest)

# ------------------------------------------------ Data Munging Step ---------------------------------------------------------

# Creates a file path to where our spectral libraries for each site is loacated
mypath_atkin = "Output/"

# Reads in species and functional level groups dataframe
Species_groups<-read.csv("Output/B_001_Species_Table.csv")

# Import file path names of .rds files into character list (Spectral libraries based on each location) 
SpecLib_by_location = list.files(mypath_atkin, pattern="A_0",full.names = T) 

# Reads in the spectral libraries for each location in a list...List of 13 spectral objects
list_of_SpecLib<-lapply(SpecLib_by_location,readRDS)%>% # Reads in the spectral library for each site 
  setNames(gsub("Output/","",SpecLib_by_location)) # Removes dir path from the name

# Combines specral libraries from all locations
SpecLib<-Reduce(spectrolab::combine,list_of_SpecLib)%>% # dim(n_samples=1989, n_wavelegths=2151)
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)%>% # Removes unwanted column 
  inner_join(Species_groups,by="Class1")%>% #Joins dataframe with all the species info to our spectral library
  dplyr::select(ScanID,Class1,Class2,Class3,Class4,Area,everything()) #Reorders columns 

# Please note these are the number of samples we have for each functional group
# table(SpecLib$Class3)%>%as.data.frame() The Dwarf shrub category is high because we scanned a high number of Dryas sp. in summer 2019

# Lets remove all the rows with negative values or Values >2
SpecLib_new<-SpecLib%>% 
  dplyr::filter_at(vars(-(ScanID:Area)), all_vars((.) < 1)) %>% # Removes rows with values greater than 2
  dplyr::filter_at(vars(-(ScanID:Area)), all_vars((.) >=0)) # Removes row with values less than 0, # dim (nrows = 1984 ncol = 2157)

# Lets check the number of scans we have for each functional group because we removed all the bad scans
# table(SpecLib_new$Class3)%>%as.data.frame()

# Lets add more details to our spectral library by adding frequency columns
# Frequency values represent the number of scans per species and the number of scans per functional group
SpecLib_new_All<-SpecLib_new%>%
  plyr::ddply( .(Class2), mutate, Class2_Freq = length(Class2))%>% # Add column to data frame that shows frequency of species
  plyr::ddply( .(Class3), mutate, Class3_Freq = length(Class3))%>% # Add column to data frame that shows frequency of functional group
  plyr::ddply( .(Class4), mutate, Class4_Freq = length(Class4))%>% # Add column to data frame that shows frequency of courser functional groups
  dplyr::select(ScanID,Class1,Class2,Class3,Class4,Area,Class2_Freq,Class3_Freq,Class4_Freq,everything()) # Rearrange columns 

# Remove all unkown scans
SpecLib_new_All<-SpecLib_new_All[!(SpecLib_new_All$Class2=="Unknown"),]

# ------------------------------------------------- Spectral Library Clean Up --------------------------------------------------
# Script removes bad scans from each species in the library
# First lets check how many species are available and the amount of scans we have for each species
table(SpecLib_new_All$Class2)%>%as.data.frame() # There are 100 targets in our spectral library

#                     Var1 Freq
#     Alectoria ochroleuca    6
#                Alnus sp.   44
# Arctocetraria centrifuga    4
#          Arctostaphyllos   19
#      Asahinea chrysantha   19
#     Aulacomnium palustre    6
#     Aulacomnium turgidum    6
#                Bare Rock    7
#                Bare Soil    8
#              Betula nana   56
#       Betula neoalaskana    4
#              Bryoria sp.   10
#        Calamogrostis sp.    4
#                Carex sp.   16
#       Cassiope tetragona    9
#      Ceratadon purpureus    5
#       Cetraria islandica   14
#       Cetraria laevigata    3
#     Cladonia amaurocraea    6
#         Cladonia cornuta    3
#        Cladonia gracilis   18
#           Cladonia mitis   17
#     Cladonia rangiferina   20
#      Cladonia steallaris   20
#          Cladonia stygia   18
#      Cladonia sulphurina    3
#        Cladonia uncialis   10
#        Dactylina arctica    7
#               Dead Salix    8
#             Dicranum sp.    5
#        Dryas alleghenies  272
#         Dryas octopetala  610
#                Dryas sp.   43
#          Empetrum nigrum   12
#        Equisetum arvense    4
#     Equisetum sylvaticum    4
#     Eriophorum vaginatum    2
#       Evernia mesomorpha   20
#  Flavocetraria cucculata   14
#    Flavocetraria nivalis   19
#                   Gravel    5
#        Heracleum lanatum    8
#     Hylocomium splendens   13
#    Hypogymnia austerodes   13
#   Icmadophila ericetorum    8
#                 Iris sp.    4
#          Ledum decumbens   19
#    Loisleuria procumbens    3
#              Lupinus sp.   12
#  Masonhalea richardsonii   14
#            Melanelia sp.   13
#        Nephroma arcticum   17
#      Parmelia omphalodes    4
#     Parmeliopsis ambigua    4
#         Parmelis sulcata   12
#     Pedicularis racemosa   11
#     Pedicularis sudetica    4
#        Peltigera apthosa   14
#        Peltigera malacea    4
#       Peltigera scabrata    7
#   Peltigers leucophlebia    4
#        Petasites frigida    8
#            Picea mariana   17
#             Pices (bark)    5
#    Pilophorus acicularis   15
#          Plagiomnium sp.    4
#     Pleurozium schreberi    4
#  Polytrichum juniperinum   10
#          Polytrichum sp.   13
#      Populus balsamifera    8
#             Porpidia sp.   11
#                   Quartz   25
# Racomitrium lanoiginosum    4
# Rhizocarpon geographicum   18
#          Rhizocarpon sp.    3
#         Rhytidum rugosum    6
#          Rosa acicularis   20
#                Rubus sp.   12
#            Salix (wooly)   10
#          Salix alaxensis   47
#      Salix arbusculoides    4
#             Salix glauca   16
#             Salix lanata    4
#         Salix ovalifolia    6
#            Salix pulchra   10
#       Salix richardsonii    4
#          Sphagnum fuscum    4
#             Sphagnum sp.    8
#         Stereocaulon sp.    8
#            Toefeldia sp.    5
#      Tomenthypnum nitens    2
#    Trapelopsis granulosa    5
#      Umbilicaria arctica    4
#   Umbilicaria hyperborea   15
#          Usnea lapponica   12
#           Usnea scabrata   12
#     Vaccinium uliginosum   10
#     Vaccinium vitis-idea   21
#       Vulpicida pinastri   12

# First lets create an object with all the names and make sure they are sorted in alphabetical order
Target_names<-unique(sort(SpecLib_new_All$Class2))

# Creates an empty list
each_target<-list()

# Function splits the spectrallibrary into spectral objects based on each target (99 Spectral Objects)
for(i in 1:length(Target_names)){
  
  # Subset a functional group
  each_target[[i]]<-subset(SpecLib_new_All,Class2 == Target_names[i])
  
  # saves metadata
  metadata<-each_target[[i]][,c(1:9)]%>%as.data.frame()
  
  # Convert to a spectral object
  each_target[[i]]<-spectrolab::as.spectra(each_target[[i]][-1:-9])
  
  # Add metadata
  meta(each_target[[i]])<-data.frame(metadata[,c(1:9)], stringsAsFactors = FALSE)
  
}

# Renames each target in list 
each_target<-each_target%>%setNames(Target_names)

# Plots spectral profile for each target (You'll have to go through all 99 targets to remove bad spectra)
plot_interactive(each_target[["Alectoria ochroleuca"    ]]) # Target 2 removed. each_target[["Alectoria ochroleuca"]]<-each_target[["Alectoria ochroleuca"    ]][-c(2),       ]
plot_interactive(each_target[["Alnus sp."               ]]) # Signatrues Good.         
plot_interactive(each_target[["Arctocetraria centrifuga"]]) # Signatrues Good.        
plot_interactive(each_target[["Arctostaphyllos"         ]]) # Target 14,13,19 removed. each_target[["Arctostaphyllos"     ]]<-each_target[["Arctostaphyllos"         ]][-c(14,13,19),]
plot_interactive(each_target[["Asahinea chrysantha"     ]]) # Target 3 removed. each_target[["Asahinea chrysantha" ]]<-each_target[["Asahinea chrysantha"     ]][-c(3),       ]
plot_interactive(each_target[["Aulacomnium palustre"    ]]) # Target 1,6 removed. each_target[["Aulacomnium palustre"]]<-each_target[["Aulacomnium palustre"    ]][-c(1,6),     ]
plot_interactive(each_target[["Aulacomnium turgidum"    ]]) # Target 4,5 removed. each_target[["Aulacomnium turgidum"]]<-each_target[["Aulacomnium turgidum"    ]][-c(4,5),     ]
plot_interactive(each_target[["Bare Rock"               ]]) # MAKE THIS A SEPARATE PFT FROM THE NEXT ONE Good but quite variables (PLEASE CHECK AGAIN)
plot_interactive(each_target[["Bare Soil"               ]]) # Target 1,2,3,7 removed. each_target[["Bare Soil"           ]]<-each_target[["Bare Soil"               ]][-c(1,2,3,7), ]
plot_interactive(each_target[["Betula nana"             ]]) # Target 5,18,26,27,32,56 removed. each_target[["Betula nana"         ]]<-each_target[["Betula nana"             ]][-c(5,18,26,27,32,56),]
plot_interactive(each_target[["Betula neoalaskana"      ]]) # Signatrues Good. 
plot_interactive(each_target[["Bryoria sp."             ]]) # Epiphyte: Don't use in spec lib but scans Good.
plot_interactive(each_target[["Calamogrostis sp."       ]]) # Signatrues Good. 
plot_interactive(each_target[["Carex sp."               ]]) # Target 10,13,14,15 removed.each_target[["Carex sp."           ]]<-each_target[["Carex sp."               ]][-c(10,13,14,15),]
plot_interactive(each_target[["Cassiope tetragona"      ]]) # Signatrues Good.
plot_interactive(each_target[["Ceratadon purpureus"     ]]) # Signatrues Good.
plot_interactive(each_target[["Cetraria islandica"      ]]) # Target removed. each_target[["Cetraria islandica"      ]]<-each_target[["Cetraria islandica"      ]][-c(2),] (PLEASE CHECK AND REMOVE ACCORDINGLY)
plot_interactive(each_target[["Cetraria laevigata"      ]]) # Signatrues Good.
plot_interactive(each_target[["Cladonia amaurocraea"    ]]) # Target 5 removed. each_target[["Cladonia amaurocraea"    ]]<-each_target[["Cladonia amaurocraea"    ]][-c(5),]
plot_interactive(each_target[["Cladonia cornuta"        ]]) # Signatrues Good.
plot_interactive(each_target[["Cladonia gracilis"       ]]) # Target 13,14,15,16,17,18 removed. each_target[["Cladonia gracilis"       ]]<-each_target[["Cladonia gracilis"       ]][-c(13,14,15,16,17,18),]
plot_interactive(each_target[["Cladonia mitis"          ]]) # Target 6,8,12,13,14,15,16,17 MIGHT BE DIFFERENT PFT (PLEASE CHECK)
plot_interactive(each_target[["Cladonia rangiferina"    ]]) # Target 2 removed. each_target[["Cladonia rangiferina"    ]]<-each_target[["Cladonia rangiferina"    ]][-c(2),]
plot_interactive(each_target[["Cladonia steallaris"     ]]) # Target 2 removed. each_target[["Cladonia steallaris"     ]]<-each_target[["Cladonia steallaris"     ]][-c(2),]
plot_interactive(each_target[["Cladonia stygia"         ]]) # Scans look like they are a different PFTs
plot_interactive(each_target[["Cladonia sulphurina"     ]]) # Target 2 removed. each_target[["Cladonia sulphurina"     ]]<-each_target[["Cladonia sulphurina"     ]][-c(2),]
plot_interactive(each_target[["Cladonia uncialis"       ]]) # Target 2 removed. each_target[["Cladonia uncialis"       ]]<-each_target[["Cladonia uncialis"       ]][-c(2),]
plot_interactive(each_target[["Dactylina arctica"       ]]) # Target 2 removed. each_target[["Dactylina arctica"       ]]<-each_target[["Dactylina arctica"       ]][-c(2),]
plot_interactive(each_target[["Dead Salix"              ]]) # Target 2 removed. each_target[["Dead Salix"              ]]<-each_target[["Dead Salix"              ]][-c(2),]
plot_interactive(each_target[["Dicranum sp."            ]]) # Target 2 removed. each_target[["Dicranum sp."            ]]<-each_target[["Dicranum sp."            ]][-c(2),]
plot_interactive(each_target[["Dryas alleghenies"       ]]) # Target 2 removed. each_target[["Dryas alleghenies"       ]]<-each_target[["Dryas alleghenies"       ]][-c(2),]
plot_interactive(each_target[["Dryas octopetala"        ]]) # Target 2 removed. each_target[["Dryas octopetala"        ]]<-each_target[["Dryas octopetala"        ]][-c(2),]
plot_interactive(each_target[["Dryas sp."               ]]) # Target 2 removed. each_target[["Dryas sp."               ]]<-each_target[["Dryas sp."               ]][-c(2),]
plot_interactive(each_target[["Empetrum nigrum"         ]]) # Target 2 removed. each_target[["Empetrum nigrum"         ]]<-each_target[["Empetrum nigrum"         ]][-c(2),]
plot_interactive(each_target[["Equisetum arvense"       ]]) # Target 2 removed. each_target[["Equisetum arvense"       ]]<-each_target[["Equisetum arvense"       ]][-c(2),]
plot_interactive(each_target[["Equisetum sylvaticum"    ]]) # Target 2 removed. each_target[["Equisetum sylvaticum"    ]]<-each_target[["Equisetum sylvaticum"    ]][-c(2),]
plot_interactive(each_target[["Eriophorum vaginatum"    ]]) # Target 2 removed. each_target[["Eriophorum vaginatum"    ]]<-each_target[["Eriophorum vaginatum"    ]][-c(2),]
plot_interactive(each_target[["Evernia mesomorpha"      ]]) # Epiphyte and Too variable: Don't use this species in spectral library
plot_interactive(each_target[["Flavocetraria cucculata" ]]) # Epiphyte and Too variable: Don't use this species in spectral library
plot_interactive(each_target[["Flavocetraria nivalis"   ]]) # Target 2 removed. each_target[["Flavocetraria nivalis"   ]]<-each_target[["Flavocetraria nivalis"   ]][-c(2),]
plot_interactive(each_target[["Gravel"                  ]]) # Target 2 removed. each_target[["Gravel"                  ]]<-each_target[["Gravel"                  ]][-c(2),]
plot_interactive(each_target[["Heracleum lanatum"       ]]) # Target 2 removed. each_target[["Heracleum lanatum"       ]]<-each_target[["Heracleum lanatum"       ]][-c(2),]
plot_interactive(each_target[["Hylocomium splendens"    ]]) # Target 2 removed. each_target[["Hylocomium splendens"    ]]<-each_target[["Hylocomium splendens"    ]][-c(2),]
plot_interactive(each_target[["Hypogymnia austerodes"   ]]) # Epiphyte: Don't use this in spectral library but scans looks good.
plot_interactive(each_target[["Icmadophila ericetorum"  ]]) # Target 2 removed. each_target[["Icmadophila ericetorum"  ]]<-each_target[["Icmadophila ericetorum"  ]][-c(2),]
plot_interactive(each_target[["Iris sp."                ]]) # Target 2 removed. each_target[["Iris sp."                ]]<-each_target[["Iris sp."                ]][-c(2),]
plot_interactive(each_target[["Ledum decumbens"         ]]) # Target 2 removed. each_target[["Ledum decumbens"         ]]<-each_target[["Ledum decumbens"         ]][-c(2),]
plot_interactive(each_target[["Loisleuria procumbens"   ]]) # Target 2 removed. each_target[["Loisleuria procumbens"   ]]<-each_target[["Loisleuria procumbens"   ]][-c(2),]
plot_interactive(each_target[["Lupinus sp."             ]]) # Target 2 removed. each_target[["Lupinus sp."             ]]<-each_target[["Lupinus sp."             ]][-c(2),]
plot_interactive(each_target[["Masonhalea richardsonii" ]]) # Target 2 removed. each_target[["Masonhalea richardsonii" ]]<-each_target[["Masonhalea richardsonii" ]][-c(2),]
plot_interactive(each_target[["Melanelia sp."           ]]) # Target 2 removed. each_target[["Melanelia sp."           ]]<-each_target[["Melanelia sp."           ]][-c(2),]
plot_interactive(each_target[["Nephroma arcticum"       ]]) # Target 2 removed. each_target[["Nephroma arcticum"       ]]<-each_target[["Nephroma arcticum"       ]][-c(2),]
plot_interactive(each_target[["Parmelia omphalodes"     ]]) # Target 2 removed. each_target[["Parmelia omphalodes"     ]]<-each_target[["Parmelia omphalodes"     ]][-c(2),]
plot_interactive(each_target[["Parmeliopsis ambigua"    ]]) # Target 2 removed. each_target[["Parmeliopsis ambigua"    ]]<-each_target[["Parmeliopsis ambigua"    ]][-c(2),]
plot_interactive(each_target[["Parmelis sulcata"        ]]) # Epiphyte (mostly): Good (Should I leave this in library?)
plot_interactive(each_target[["Pedicularis racemosa"    ]]) # Target 2 removed. each_target[["Pedicularis racemosa"    ]]<-each_target[["Pedicularis racemosa"    ]][-c(2),]
plot_interactive(each_target[["Pedicularis sudetica"    ]]) # Target 2 removed. each_target[["Pedicularis sudetica"    ]]<-each_target[["Pedicularis sudetica"    ]][-c(2),]
plot_interactive(each_target[["Peltigera apthosa"       ]]) # Target 2 removed. each_target[["Peltigera apthosa"       ]]<-each_target[["Peltigera apthosa"       ]][-c(2),]
plot_interactive(each_target[["Peltigera malacea"       ]]) # Target 2 removed. each_target[["Peltigera malacea"       ]]<-each_target[["Peltigera malacea"       ]][-c(2),]
plot_interactive(each_target[["Peltigera scabrata"      ]]) # Target 2 removed. each_target[["Peltigera scabrata"      ]]<-each_target[["Peltigera scabrata"      ]][-c(2),]
plot_interactive(each_target[["Peltigers leucophlebia"  ]]) # Target 2 removed. each_target[["Peltigers leucophlebia"  ]]<-each_target[["Peltigers leucophlebia"  ]][-c(2),]
plot_interactive(each_target[["Petasites frigida"       ]]) # Target 2 removed. each_target[["Petasites frigida"       ]]<-each_target[["Petasites frigida"       ]][-c(2),]
plot_interactive(each_target[["Picea mariana"           ]]) # Target 2 removed. each_target[["Picea mariana"           ]]<-each_target[["Picea mariana"           ]][-c(2),]
plot_interactive(each_target[["Pices (bark)"            ]]) # Target 2 removed. each_target[["Pices (bark)"            ]]<-each_target[["Pices (bark)"            ]][-c(2),]
plot_interactive(each_target[["Pilophorus acicularis"   ]]) # Target 2 removed. each_target[["Pilophorus acicularis"   ]]<-each_target[["Pilophorus acicularis"   ]][-c(2),]
plot_interactive(each_target[["Plagiomnium sp."         ]]) # Target 2 removed. each_target[["Plagiomnium sp."         ]]<-each_target[["Plagiomnium sp."         ]][-c(2),]
plot_interactive(each_target[["Pleurozium schreberi"    ]]) # Target 2 removed. each_target[["Pleurozium schreberi"    ]]<-each_target[["Pleurozium schreberi"    ]][-c(2),]
plot_interactive(each_target[["Polytrichum juniperinum" ]]) # Target 2 removed. each_target[["Polytrichum juniperinum" ]]<-each_target[["Polytrichum juniperinum" ]][-c(2),]
plot_interactive(each_target[["Polytrichum sp."         ]]) # Target 2 removed. each_target[["Polytrichum sp."         ]]<-each_target[["Polytrichum sp."         ]][-c(2),]
plot_interactive(each_target[["Populus balsamifera"     ]]) # Target 2 removed. each_target[["Populus balsamifera"     ]]<-each_target[["Populus balsamifera"     ]][-c(2),]
plot_interactive(each_target[["Porpidia sp."            ]]) # Target 2 removed. each_target[["Porpidia sp."            ]]<-each_target[["Porpidia sp."            ]][-c(2),]
plot_interactive(each_target[["Quartz"                  ]]) # Target 2 removed. each_target[["Quartz"                  ]]<-each_target[["Quartz"                  ]][-c(2),]
plot_interactive(each_target[["Racomitrium lanoiginosum"]]) # Target 2 removed. each_target[["Racomitrium lanoiginosum"]]<-each_target[["Racomitrium lanoiginosum"]][-c(2),]
plot_interactive(each_target[["Rhizocarpon geographicum"]]) # Target 2 removed. each_target[["Rhizocarpon geographicum"]]<-each_target[["Rhizocarpon geographicum"]][-c(2),]
plot_interactive(each_target[["Rhizocarpon sp."         ]]) # Target 2 removed. each_target[["Rhizocarpon sp."         ]]<-each_target[["Rhizocarpon sp."         ]][-c(2),]
plot_interactive(each_target[["Rhytidum rugosum"        ]]) # Target 2 removed. each_target[["Rhytidum rugosum"        ]]<-each_target[["Rhytidum rugosum"        ]][-c(2),]
plot_interactive(each_target[["Rosa acicularis"         ]]) # Target 2 removed. each_target[["Rosa acicularis"         ]]<-each_target[["Rosa acicularis"         ]][-c(2),]
plot_interactive(each_target[["Rubus sp."               ]]) # Target 2 removed. each_target[["Rubus sp."               ]]<-each_target[["Rubus sp."               ]][-c(2),]
plot_interactive(each_target[["Salix (wooly)"           ]]) # Target 2 removed. each_target[["Salix (wooly)"           ]]<-each_target[["Salix (wooly)"           ]][-c(2),]
plot_interactive(each_target[["Salix alaxensis"         ]]) # Target 2 removed. each_target[["Salix alaxensis"         ]]<-each_target[["Salix alaxensis"         ]][-c(2),]
plot_interactive(each_target[["Salix arbusculoides"     ]]) # Target 2 removed. each_target[["Salix arbusculoides"     ]]<-each_target[["Salix arbusculoides"     ]][-c(2),]
plot_interactive(each_target[["Salix glauca"            ]]) # Target 2 removed. each_target[["Salix glauca"            ]]<-each_target[["Salix glauca"            ]][-c(2),]
plot_interactive(each_target[["Salix lanata"            ]]) # Target 2 removed. each_target[["Salix lanata"            ]]<-each_target[["Salix lanata"            ]][-c(2),]
plot_interactive(each_target[["Salix ovalifolia"        ]]) # Target 2 removed. each_target[["Salix ovalifolia"        ]]<-each_target[["Salix ovalifolia"        ]][-c(2),]
plot_interactive(each_target[["Salix pulchra"           ]]) # Target 2 removed. each_target[["Salix pulchra"           ]]<-each_target[["Salix pulchra"           ]][-c(2),]
plot_interactive(each_target[["Salix richardsonii"      ]]) # Target 2 removed. each_target[["Salix richardsonii"      ]]<-each_target[["Salix richardsonii"      ]][-c(2),]
plot_interactive(each_target[["Sphagnum fuscum"         ]]) # Target 2 removed. each_target[["Sphagnum fuscum"         ]]<-each_target[["Sphagnum fuscum"         ]][-c(2),]
plot_interactive(each_target[["Sphagnum sp."            ]]) # Target 2 removed. each_target[["Sphagnum sp."            ]]<-each_target[["Sphagnum sp."            ]][-c(2),]
plot_interactive(each_target[["Stereocaulon sp."        ]]) # Target 2 removed. each_target[["Stereocaulon sp."        ]]<-each_target[["Stereocaulon sp."        ]][-c(2),]
plot_interactive(each_target[["Toefeldia sp."           ]]) # Target 2 removed. each_target[["Toefeldia sp."           ]]<-each_target[["Toefeldia sp."           ]][-c(2),]
plot_interactive(each_target[["Tomenthypnum nitens"     ]]) # Target 2 removed. each_target[["Tomenthypnum nitens"     ]]<-each_target[["Tomenthypnum nitens"     ]][-c(2),]
plot_interactive(each_target[["Trapelopsis granulosa"   ]]) # Target 2 removed. each_target[["Trapelopsis granulosa"   ]]<-each_target[["Trapelopsis granulosa"   ]][-c(2),]
plot_interactive(each_target[["Umbilicaria arctica"     ]]) # Target 2 removed. each_target[["Umbilicaria arctica"     ]]<-each_target[["Umbilicaria arctica"     ]][-c(2),]
plot_interactive(each_target[["Umbilicaria hyperborea"  ]]) # Target 2 removed. each_target[["Umbilicaria hyperborea"  ]]<-each_target[["Umbilicaria hyperborea"  ]][-c(2),]
plot_interactive(each_target[["Usnea lapponica"         ]]) # Epiphyte: Good but Don't use in spec lib.
plot_interactive(each_target[["Usnea scabrata"          ]]) # Epiphyte: Good but don't use in spec lib.
plot_interactive(each_target[["Vaccinium uliginosum"    ]]) # Target 2 removed. each_target[["Vaccinium uliginosum"    ]]<-each_target[["Vaccinium uliginosum"    ]][-c(2),]
plot_interactive(each_target[["Vaccinium vitis-idea"    ]]) # Target 2 removed. each_target[["Vaccinium vitis-idea"    ]]<-each_target[["Vaccinium vitis-idea"    ]][-c(2),]
plot_interactive(each_target[["Vulpicida pinastri"      ]]) # Epiphyte: Don't use: Scans 5 and 7 are weird


# Creates a new object with clenaed spectral library
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

# Saves each target as a spectral object (.rds)
for(i in 1:length(New_targets)){
  
  # Writes out each target and their spectral profiles
  saveRDS(New_targets[[i]],paste0("Output/Spectral_profiles/",names(New_targets[i]),".rds"))
  
}

# Lets combine into one spectral library if we are satisfied with our results
Cleaned_Speclib<-Reduce(spectrolab::combine,New_targets)

# You could read in all these targets using the code below
# listofnames<-list.files("Output/Spectral_profiles",full.names = T) 
# Targets_speclib<-lapply(listofnames,readRDS)%>%setNames(basename(listofnames))

# Creates a dataframe with the names of the list of species
# target_table<-names(New_targets)%>%
#   as.data.frame()
# 
# write.csv(target_table,"Output/targets_df.csv",row.names = F)



# ------------------------------------------------- Even distribution among species --------------------------------------------

# Creates a dataframe that shows scans per species within each functional group
Species_table_df<-SpecLib_new_All%>%
  group_by(Class3,Class2)%>%
  tally()
# View(Species_table)

# Writes out species per functional group dataframe
Species_table_df%>%
  write.csv("Output/C_002_SpeciesPer_FunctionalGroup.csv", row.names = F)


# Function allows the number of scans per species (Class2) to be equal within each functinal group (Class3)
reduce_SpeciesPerGroup<-function(x){
  
  #dataframe that shows scans per species within each functional group
  Species_table<-x%>%
    group_by(Class3,Class2)%>%
    tally()
  
  # Creates a list that contains a dataframe for each functional group
  Func_groups<-split(x,x$Class3)
  
  # count per species within each functional group
  countperspecies<-split(Species_table,Species_table$Class3)
  
  # Lets make the Class2 groups equal within each functinal group (Class3)
  Func_groups_reduced<-Map(x=Func_groups, y = countperspecies, function(x,y){
    x%>%
      group_by(Class2)%>%
      sample_n(pmin(n(), median(y$n)))%>%
      as.data.frame()
  })
  return(Func_groups_reduced)
}

# Applies function to spectral library
SpecLib_reduced<- reduce_SpeciesPerGroup(SpecLib_new_All)

# Combines the list above into one dataframe
SpecLib_reduced_df<-do.call("rbind", SpecLib_reduced)

# str(SpecLib_reduced_df)

# Writes out dataframe
SpecLib_reduced_df%>%
  write.csv("Output/C_003_SpecLib_FunctionalGroupsEqual_DF.csv",row.names = F)

# Lets check the number of scans we have for each functional category now that we reduced the distribution
# These will be used in models
SpecLib_reduced_df %>%
  group_by(Class3) %>%
  tally()
# Class3                 n
# <fct>              <int>
# Abiotic_Litter        11
# Abiotic_Rock          19
# Abiotic_Soil           8
# Dwarf_Shrub_Broad    101
# Dwarf_Shrub_Needle    19
# Forb                  51
# Graminoid_Grass        4
# Graminoid_Sedge       15
# Lichen_Dark          133
# Lichen_Light          65
# Lichen_Yellow        153
# Moss_Acrocarp         44
# Moss_Pleurocarp       14
# Moss_Sphagnum         10
# Shrub_Alder           44
# Shrub_Other           61
# Shrub_Salix           50
# Tree_Broad            10
# Tree_Needle           17

Spectralobj1<-SpecLib_reduced_df%>%
  dplyr::filter(Class2=="Betula nana")

Spectralobj<-spectrolab::as.spectra(Spectralobj1[-1:-9])
meta(Spectralobj)<-data.frame(Spectralobj1[,c(1:9)], stringsAsFactors = FALSE)

#plot_interactive(Spectralobj)
# 
## Lets convert our new spectral library to a spectral object to be used later 
#SpecLib_reduced_spectra <-spectrolab::as.spectra(SpecLib_reduced_df[-1:-9])
#
## adds metadata
#meta(SpecLib_reduced_spectra)<-data.frame(SpecLib_reduced_df[1:9], stringsAsFactors = FALSE)
#saveRDS(SpecLib_reduced_spectra,"Output/C_004_SpecLib_FunctionalGroupsEqual.rds")

# ------------------------------------------ Plots ---------------------------------------------------

# Creates a vector with the name of all the categories of interest
names_of_classes<-c(as.character(unique(SpecLib_reduced_df[,"Class3"])))

# Creates an empty list
FunctionalGroupDf<-list()

for(i in 1:length(names_of_classes)){
  
  # Subset a functional group
  FunctionalGroupDf[[i]]<-subset(SpecLib_reduced_df,Class3 == names_of_classes[i])
  
  
  # change the dtaframe to a long dataframe 
  FunctionalGroupDf[[i]]<-gather(FunctionalGroupDf[[i]] ,Wavelength,Reflectance,-1:-9)
  
  
  # Make column name Wavelength numeric
  FunctionalGroupDf[[i]]$Wavelength    <-as.numeric(FunctionalGroupDf[[i]]$Wavelength)
  
  # Plot the output
  FunctionalGroupDf[[i]]<-FunctionalGroupDf[[i]]%>%
    group_by(Class2, Wavelength) %>%  
    dplyr::summarise(Median_Reflectance = median(Reflectance))%>%
    as.data.frame()
}

for(i in 1:length(FunctionalGroupDf)){
  
  # Creates a ggplot for each functional group with all species
  ggplot(FunctionalGroupDf[[i]],aes(Wavelength,Median_Reflectance))+geom_line(aes(color = Class2))+
    labs(color="Species")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
          legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=12))+
    labs(title = paste(names_of_classes[[i]]," Spectral Signatures", sep = ""))
  
  # Saves the plots
  ggsave(paste("Output/","C_005","_",names_of_classes[[i]],".jpg",sep =""))
  
}

# ---------------------------------------------------- Resample bands based on headwall bandpasses -------------------------------------------

# Creates a vector of the bandpasses for the headwall sensor that will be used
# Noisey band were omitted (only bands 1:272 below)
Headwall_bandpasses<-c(397.593,399.444, 401.296, 403.148, 405.000, 406.851, 408.703, 410.555, 412.407,
                       414.258,416.110, 417.962, 419.814, 421.666, 423.517, 425.369, 427.221, 429.073,
                       430.924,432.776, 434.628, 436.480, 438.332, 440.183, 442.035, 443.887, 445.739,
                       447.590,449.442, 451.294, 453.146, 454.998, 456.849, 458.701, 460.553, 462.405,
                       464.256,466.108, 467.960, 469.812, 471.664, 473.515, 475.367, 477.219, 479.071,
                       480.922,482.774, 484.626, 486.478, 488.330, 490.181, 492.033, 493.885, 495.737,
                       497.588,499.440, 501.292, 503.144, 504.996, 506.847, 508.699, 510.551, 512.403,
                       514.254,516.106, 517.958, 519.810, 521.662, 523.513, 525.365, 527.217, 529.069,
                       530.920,532.772, 534.624, 536.476, 538.328, 540.179, 542.031, 543.883, 545.735,
                       547.586,549.438, 551.290, 553.142, 554.994, 556.845, 558.697, 560.549, 562.401,
                       564.252,566.104, 567.956, 569.808, 571.659, 573.511, 575.363, 577.215, 579.067,
                       580.918,582.770, 584.622, 586.474, 588.325, 590.177, 592.029, 593.881, 595.733,
                       597.584,599.436, 601.288, 603.140, 604.991, 606.843, 608.695, 610.547, 612.399,
                       614.250,616.102, 617.954, 619.806, 621.657, 623.509, 625.361, 627.213, 629.065,
                       630.916,632.768, 634.620, 636.472, 638.323, 640.175, 642.027, 643.879, 645.731,
                       647.582,649.434, 651.286, 653.138, 654.989, 656.841, 658.693, 660.545, 662.397,
                       664.248,666.100, 667.952, 669.804, 671.655, 673.507, 675.359, 677.211, 679.063,
                       680.914,682.766, 684.618, 686.470, 688.321, 690.173, 692.025, 693.877, 695.729,
                       697.580,699.432, 701.284, 703.136, 704.987, 706.839, 708.691, 710.543, 712.395,
                       714.246,716.098, 717.950, 719.802, 721.653, 723.505, 725.357, 727.209, 729.061,
                       730.912,732.764, 734.616, 736.468, 738.319, 740.171, 742.023, 743.875, 745.726,
                       747.578,749.430, 751.282, 753.134, 754.985, 756.837, 758.689, 760.541, 762.392,
                       764.244,766.096, 767.948, 769.800, 771.651, 773.503, 775.355, 777.207, 779.058,
                       780.910,782.762, 784.614, 786.466, 788.317, 790.169, 792.021, 793.873, 795.724,
                       797.576,799.428, 801.280, 803.132, 804.983, 806.835, 808.687, 810.539, 812.390,
                       814.242,816.094, 817.946, 819.798, 821.649, 823.501, 825.353, 827.205, 829.056,
                       830.908,832.760, 834.612, 836.464, 838.315, 840.167, 842.019, 843.871, 845.722,
                       847.574,849.426, 851.278, 853.130, 854.981, 856.833, 858.685, 860.537, 862.388,
                       864.240,866.092, 867.944, 869.796, 871.647, 873.499, 875.351, 877.203, 879.054,
                       880.906,882.758, 884.610, 886.462, 888.313, 890.165, 892.017, 893.869, 895.720,
                       897.572,899.424)
# ---------------------------------------------- PSR band Resampling using Sensor Bandpasses ------------------------------
# Turn this into a function, It will be used  with the MSGC data
# Reads in spectral library as a spectral object
# This is the spectral library that had all uncalibrated scans removed
# Even distribution of species within each functional group applied
Speclib_spec<-readRDS("Output/C_004_SpecLib_FunctionalGroupsEqual.rds")

# Function resamples the PSR band passes to match the sensor
ResampBands<-function(x){
  
  # Resamples alsakSpeclib based on Headwall Bandpasses
  # Resamples alsakSpeclib based on the bandpasses
  Resamp<-spectrolab::resample(x,Headwall_bandpasses)
  
  # Converts Spectral library to a dataframe
  Df_convert<-Resamp%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  
  # Removes bad scans (Scans with reflectance values >2 or <0)
  goodscans<-Df_convert%>%
    filter_at(vars(-(ScanID:Class4_Freq)), all_vars(. <2))%>%
    filter_at(vars(-(ScanID:Class4_Freq)), all_vars(. >=0))
  
  # Writes out each dataframe as a .csv file
  write.csv(goodscans,
            file = paste("Output/","D_001_",
                         "Headwall_SpecLibDF.csv",sep=""), row.names = F)
  
  return(goodscans)
}

# Apply function
Speclib_resamp<-ResampBands(Speclib_spec)

# Cleans up R memeory
# rm(list=ls())

# --------------------------------------------- Creating and Saving Derivatives ------------------------------------------------------
# Source the function that will calculate derivatives of our new spectral library
# The function will do the same for a datacube
source("Functions/SpectralLibrayCreator.R")

# Calculate Derivative for spectral libaray
Spectral_Library<-SpectralLibrayCreator("Output/D_001_Headwall_SpecLibDF.csv",
                                        out_file= "Output/",datatype = "csv", 
                                        extension = FALSE)




