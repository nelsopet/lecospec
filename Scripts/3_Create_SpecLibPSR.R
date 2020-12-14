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

# Reads in scans from ecosis
Spectra_Ecosis<-read.csv("Data/leaf_spectra_barrow_2013_20180824_ecosis.csv", check.names = F)
Metadata_Ecosis<-read.csv("Data/ngeearctic_bnl_2013_leaf_spectra_traits_metadata.csv", check.names = F)

# Meges meta and spectra
Ecosis_data<-cbind(Metadata_Ecosis[,-3],Spectra_Ecosis[,-1])

# Change name of column and add an area column
names(Ecosis_data)[2]<-"Class1"
names(Ecosis_data)[1]<-"ScanID"
Ecosis_data$Area<- "NOT RECORDED"

# Creates final library with all the data
Ecosis_data<-Ecosis_data%>%inner_join(Species_groups,by="Class1")%>%
  dplyr::select(ScanID,Class1,Class2,Class3,Class4,Area,everything())

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

# Combines Ecosis data and spectral library
SpecLib<-rbind(SpecLib,Ecosis_data)

# Please note these are the number of samples we have for each functional group
#table(SpecLib$Class3)%>%as.data.frame() #The Dwarf shrub category is high because we scanned a high number of Dryas sp. in summer 2019
#Var1 Freq
#1              Dwarf_Shrub_Decid  968
#2                           Forb   80
#3                Graminoid_Grass   24
#4                Graminoid_Sedge   44
#5           Lichen_Crustose_Dark   32
#6          Lichen_Crustose_Light   28
#7           Lichen_Epiphyte_Dark   23
#8         Lichen_Epiphyte_Yellow   56
#9            Lichen_Foliose_Dark   46
#10 Lichen_Foliose_Dark_Peltigera   29
#11          Lichen_Foliose_Light   16
#12         Lichen_Foliose_Yellow   53
#13         Lichen_Fruticose_Dark   43
#14        Lichen_Fruticose_Light   46
#15       Lichen_Fruticose_Yellow   97
#16                        Litter    8
#17              Moss_Aulacomnium   12
#18                Moss_Ceratadon    5
#19                 Moss_Dicranum    5
#20              Moss_Hylocomnium   13
#21              Moss_Plagiomnium    4
#22               Moss_Pleurozium    4
#23              Moss_Polytrichum   23
#24              Moss_Racomitrium    4
#25                Moss_Rhytidium    6
#26          Moss_Sphagnum_fuscum    4
#27           Moss_Sphagnum_other    8
#28             Moss_Tomenthypnum    2
#29                          Rock   32
#30                   Shrub_Alder   44
#31                  Shrub_Betula   56
#32               Shrub_Evergreen   65
#33                    Shrub_Rosa   20
#34                   Shrub_Salix  111
#35                          Soil   13
#36                    Tree_Decid   12
#37                Tree_Evergreen   17
#38                   Wood_Coarse    5

# Lets remove all the rows with negative values or Values >2
SpecLib_new<-SpecLib%>% 
  dplyr::filter_at(vars(-(ScanID:Area)), all_vars((.) < 1)) %>% # Removes rows with values greater than 2
  dplyr::filter_at(vars(-(ScanID:Area)), all_vars((.) >=0)) # Removes row with values less than 0, # dim (nrows = 1984 ncol = 2157)

# Lets check the number of scans we have for each functional group because we removed all the bad scans
 table(SpecLib_new$Class3)%>%as.data.frame()

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
#table(SpecLib_new_All$Class2)%>%as.data.frame() # There are 99 targets in our spectral library

#Var1 Freq
#1       Alectoria ochroleuca    6
#2                  Alnus sp.   44
#3     Arctagrostis latifolia    5
#4   Arctocetraria centrifuga    4
#5           Arctophila fulva    5
#6            Arctostaphyllos   19
#7        Asahinea chrysantha   19
#8       Aulacomnium palustre    6
#9       Aulacomnium turgidum    6
#10                 Bare Rock    7
#11                 Bare Soil   13
#12               Betula nana   56
#13        Betula neoalaskana    4
#14               Bryoria sp.   10
#15         Calamogrostis sp.    4
#16           Carex aquatilis    8
#17                 Carex sp.   16
#18        Cassiope tetragona    9
#19       Ceratadon purpureus    5
#20        Cetraria islandica   14
#21        Cetraria laevigata    3
#22      Cladonia amaurocraea    6
#23          Cladonia cornuta    3
#24         Cladonia gracilis   18
#25            Cladonia mitis   17
#26      Cladonia rangiferina   20
#27       Cladonia steallaris   20
#28           Cladonia stygia   18
#29       Cladonia sulphurina    3
#30         Cladonia uncialis   10
#31         Dactylina arctica    7
#32                Dead Salix    8
#33              Dicranum sp.    5
#34         Dryas alleghenies  272
#35          Dryas octopetala  610
#36                 Dryas sp.   43
#37          Dupontia fisheri   10
#38           Empetrum nigrum   12
#39         Equisetum arvense    4
#40      Equisetum sylvaticum    4
#41  Eriophorum angustifolium   10
#42      Eriophorum vaginatum    2
#43        Evernia mesomorpha   20
#44   Flavocetraria cucculata   14
#45     Flavocetraria nivalis   19
#46         Heracleum lanatum    8
#47      Hylocomium splendens   13
#48     Hypogymnia austerodes   13
#49    Icmadophila ericetorum    8
#50                  Iris sp.    4
#51           Ledum decumbens   19
#52     Loisleuria procumbens    3
#53               Lupinus sp.   12
#54   Masonhalea richardsonii   14
#55             Melanelia sp.   13
#56         Nephroma arcticum   17
#57       Parmelia omphalodes    4
#58      Parmeliopsis ambigua    4
#59          Parmelis sulcata   12
#60      Pedicularis racemosa   11
#61      Pedicularis sudetica    4
#62         Peltigera apthosa   14
#63         Peltigera malacea    4
#64        Peltigera scabrata    7
#65    Peltigers leucophlebia    4
#66       Pestasites frigidus   10
#67         Petasites frigida    8
#68             Picea mariana   17
#69              Pices (bark)    5
#70     Pilophorus acicularis   15
#71           Plagiomnium sp.    4
#72      Pleurozium schreberi    4
#73   Polytrichum juniperinum   10
#74           Polytrichum sp.   13
#75       Populus balsamifera    8
#76              Porpidia sp.   11
#77                    Quartz   25
#78  Racomitrium lanoiginosum    4
#79  Rhizocarpon geographicum   18
#80           Rhizocarpon sp.    3
#81          Rhytidum rugosum    6
#82           Rosa acicularis   20
#83                 Rubus sp.   12
#84             Salix (wooly)   10
#85           Salix alaxensis   47
#86       Salix arbusculoides    4
#87              Salix glauca   16
#88              Salix lanata    4
#89          Salix ovalifolia    6
#90             Salix pulchra   20
#91        Salix richardsonii    4
#92        Saxifraga punctata   10
#93           Sphagnum fuscum    4
#94              Sphagnum sp.    8
#95          Stereocaulon sp.    8
#96             Toefeldia sp.    5
#97       Tomenthypnum nitens    2
#98     Trapelopsis granulosa    5
#99       Umbilicaria arctica    4
#100   Umbilicaria hyperborea   15
#101          Usnea lapponica   12
#102           Usnea scabrata   12
#103     Vaccinium uliginosum   10
#104     Vaccinium vitis-idea   22
#105       Vulpicida pinastri   12
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
# plot_interactive(each_target[["Alectoria ochroleuca"    ]]) # Target 2 removed. 
# plot_interactive(each_target[["Alnus sp."               ]]) # Signatrues Good.         
# plot_interactive(each_target[["Arctocetraria centrifuga"]]) # Signatrues Good.        
# plot_interactive(each_target[["Arctostaphyllos"         ]]) # Target 19,13,10,11,16,17 removed. 
# plot_interactive(each_target[["Asahinea chrysantha"     ]]) # Target 3 removed. 
# plot_interactive(each_target[["Aulacomnium palustre"    ]]) # Signatrues Good. 
# plot_interactive(each_target[["Aulacomnium turgidum"    ]]) # Target 3,4 removed,
# plot_interactive(each_target[["Bare Rock"               ]]) # New class Rock.
# plot_interactive(each_target[["Bare Soil"               ]]) # New class soil.
# plot_interactive(each_target[["Betula nana"             ]]) # Target 5,18,26,27,32,56 removed. 
# plot_interactive(each_target[["Betula neoalaskana"      ]]) # Need more of this from Ecosis. Signatrues Good. 
# plot_interactive(each_target[["Bryoria sp."             ]]) # remove. Epiphyte: Don't use in spec lib but scans Good.
# plot_interactive(each_target[["Calamogrostis sp."       ]]) # Signatrues Good. 
# plot_interactive(each_target[["Carex sp."               ]]) # Target 10,12,13,14,15 removed.
# plot_interactive(each_target[["Cassiope tetragona"      ]]) # Evergreen Dwarf shrubs. Signatrues Good.
# plot_interactive(each_target[["Ceratadon purpureus"     ]]) # Signatrues Good.
# plot_interactive(each_target[["Cetraria islandica"      ]]) # Target 1,2,3,4,8 removed. 
# plot_interactive(each_target[["Cetraria laevigata"      ]]) # Signatrues Good.
# plot_interactive(each_target[["Cladonia amaurocraea"    ]]) # Signatrues Good.
# plot_interactive(each_target[["Cladonia cornuta"        ]]) # Signatrues Good.
# plot_interactive(each_target[["Cladonia gracilis"       ]]) # Target 13,14,15,16,17,18 removed. 
# plot_interactive(each_target[["Cladonia mitis"          ]]) # Target 8,12,13,14,15,16,17 removed. 
# plot_interactive(each_target[["Cladonia rangiferina"    ]]) # Target 1,2,3,4,17 removed. 
# plot_interactive(each_target[["Cladonia steallaris"     ]]) # Target 13,14,16,20 removed. 
# plot_interactive(each_target[["Cladonia stygia"         ]]) # Target 13 removed. 
# plot_interactive(each_target[["Cladonia sulphurina"     ]]) # Rare target remove
# plot_interactive(each_target[["Cladonia uncialis"       ]]) # Target 6 removed. 
# plot_interactive(each_target[["Dactylina arctica"       ]]) # Rare target remove Target 3,7 removed. 
# plot_interactive(each_target[["Dead Salix"              ]]) # Target 3,4,8 removed. 
# plot_interactive(each_target[["Dicranum sp."            ]]) # Signatures Good.
# plot_interactive(each_target[["Dryas alleghenies"       ]]) # Kept up to 50 scans Target 15,22,29,37,38,46,47,48,55 removed. 
# plot_interactive(each_target[["Dryas octopetala"        ]]) # Kept up to 50 scans Target 10,29,33,32,38,41,42,48 removed. 
# plot_interactive(each_target[["Dryas sp."               ]]) # Signatures good.
# plot_interactive(each_target[["Empetrum nigrum"         ]]) # Target 1,3 removed. 
# plot_interactive(each_target[["Equisetum arvense"       ]]) # Signatures good.Make equisetum it's own plant functional type
# plot_interactive(each_target[["Equisetum sylvaticum"    ]]) # Signatures good.Make equisetum it's own plant functional type
# plot_interactive(each_target[["Eriophorum vaginatum"    ]]) # Signatures good for our application (Bands 800:2500 will be removed)
# plot_interactive(each_target[["Evernia mesomorpha"      ]]) # Target 6,7,8,19,20 removed. Epiphyte and Too variable: Don't use this species in spectral library 
# plot_interactive(each_target[["Flavocetraria cucculata" ]]) # Target 1,12,14 removed. 
# plot_interactive(each_target[["Flavocetraria nivalis"   ]]) # Clean and remove. Target 11 removed. 
# plot_interactive(each_target[["Gravel"                  ]]) # Target 5 removed.
# plot_interactive(each_target[["Heracleum lanatum"       ]]) # Clean and remove 
# plot_interactive(each_target[["Hylocomium splendens"    ]]) # Target 5,6,7 removed. e
# plot_interactive(each_target[["Hypogymnia austerodes"   ]]) # Clean and remove. Target 1,2,3,4 removed Epiphyte
# plot_interactive(each_target[["Icmadophila ericetorum"  ]]) # Clean and remove  Target 4,7 removed. 
# plot_interactive(each_target[["Iris sp."                ]]) # Clean and remove  Target 2 removed. 
# plot_interactive(each_target[["Ledum decumbens"         ]]) # Signatures Good Target 2 removed.
# plot_interactive(each_target[["Loisleuria procumbens"   ]]) # Signatures good. Clean and remove Target 2 removed
# plot_interactive(each_target[["Lupinus sp."             ]]) # Target 12 removed. 
# plot_interactive(each_target[["Masonhalea richardsonii" ]]) # Target 3,6 removed. Clean and remove Target 2 removed. 
# plot_interactive(each_target[["Melanelia sp."           ]]) # Clean and remove. Signatures good
# plot_interactive(each_target[["Nephroma arcticum"       ]]) # Clean and remove. Target 10,11,12,13. 
# plot_interactive(each_target[["Parmelia omphalodes"     ]]) # Clean and remove Signatures good.
# plot_interactive(each_target[["Parmeliopsis ambigua"    ]]) # Clean and remove. Target 4 removed. 
# plot_interactive(each_target[["Parmelis sulcata"        ]]) # Clean and remove. Target 1,11 removed. Epiphyte 
# plot_interactive(each_target[["Pedicularis racemosa"    ]]) # Target 1,8 removed. 
# plot_interactive(each_target[["Pedicularis sudetica"    ]]) # Clean and remove. Signatures good.
# plot_interactive(each_target[["Peltigera apthosa"       ]]) # New class peltigera green. Target 3,4,5 removed. 
# plot_interactive(each_target[["Peltigera malacea"       ]]) # New class peltigera cyano. Signatures good
# plot_interactive(each_target[["Peltigera scabrata"      ]]) # New class peltigera cyano. Signatures good. 
# plot_interactive(each_target[["Peltigers leucophlebia"  ]]) # New class peltigera green. Signatures good 
# plot_interactive(each_target[["Petasites frigida"       ]]) # remove Target 1 removed. 
# plot_interactive(each_target[["Picea mariana"           ]]) # Signatures good.
# plot_interactive(each_target[["Pices (bark)"            ]]) # Signatures good. 
# plot_interactive(each_target[["Pilophorus acicularis"   ]]) # remove. Signatures good.
# plot_interactive(each_target[["Plagiomnium sp."         ]]) # remove. Signatures good
# plot_interactive(each_target[["Pleurozium schreberi"    ]]) # Signatures good. 
# plot_interactive(each_target[["Polytrichum juniperinum" ]]) # Target 1,2,3,4 removed. 
# plot_interactive(each_target[["Polytrichum sp."         ]]) # Target 1,4 removed. 
# plot_interactive(each_target[["Populus balsamifera"     ]]) # Signatures good.
# plot_interactive(each_target[["Porpidia sp."            ]]) # remove. Signatures good.
# plot_interactive(each_target[["Quartz"                  ]]) # Target 13 removed. 
# plot_interactive(each_target[["Racomitrium lanoiginosum"]]) # Signatures good. 
# plot_interactive(each_target[["Rhizocarpon geographicum"]]) # New class yellow crust lichen. Target 8,18 removed. 
# plot_interactive(each_target[["Rhizocarpon sp."         ]]) # remove. Signatures good.
# plot_interactive(each_target[["Rhytidum rugosum"        ]]) # Signatures good.
# plot_interactive(each_target[["Rosa acicularis"         ]]) # Target 19 removed. 
# plot_interactive(each_target[["Rubus sp."               ]]) # Target 8,6 removed.
# plot_interactive(each_target[["Salix (wooly)"           ]]) # remove. Signatures good.
# plot_interactive(each_target[["Salix alaxensis"         ]]) # Signatures good. 
# plot_interactive(each_target[["Salix arbusculoides"     ]]) # Signatures good.
# plot_interactive(each_target[["Salix glauca"            ]]) # Signatures good.
# plot_interactive(each_target[["Salix lanata"            ]]) # Signatures good.
# plot_interactive(each_target[["Salix ovalifolia"        ]]) # Signatures good.
# plot_interactive(each_target[["Salix pulchra"           ]]) # Signatures good.
# plot_interactive(each_target[["Salix richardsonii"      ]]) # Signatures good.
# plot_interactive(each_target[["Sphagnum fuscum"         ]]) # Signatures good.
# plot_interactive(each_target[["Sphagnum sp."            ]]) # Signatures good.
# plot_interactive(each_target[["Stereocaulon sp."        ]]) # Signatures good.
# plot_interactive(each_target[["Toefeldia sp."           ]]) # remove  Target 5 removed. 
# plot_interactive(each_target[["Tomenthypnum nitens"     ]]) # remove. Signatures good.
# plot_interactive(each_target[["Trapelopsis granulosa"   ]]) # dark crust lichen.
# plot_interactive(each_target[["Umbilicaria arctica"     ]]) # Signatures good. 
# plot_interactive(each_target[["Umbilicaria hyperborea"  ]]) # Signatures good.
# plot_interactive(each_target[["Usnea lapponica"         ]]) # remove  Epiphyte: Good but Don't use in spec lib.
# plot_interactive(each_target[["Usnea scabrata"          ]]) # remove  Epiphyte: Good but don't use in spec lib.
# plot_interactive(each_target[["Vaccinium uliginosum"    ]]) # Signatures good.
# plot_interactive(each_target[["Vaccinium vitis-idea"    ]]) # Target 1,2,3,4,5,6,7,8 removed.
# plot_interactive(each_target[["Pestasites frigidus"     ]]) # Signatures good.   
# plot_interactive(each_target[["Saxifraga punctata"      ]]) # Signatures good. 
# plot_interactive(each_target[["Arctophila fulva"        ]]) # Signatures good. 
# plot_interactive(each_target[["Dupontia fisheri"        ]]) # Signatures good. 
# plot_interactive(each_target[["Carex aquatilis"         ]]) # Signatures good. 
# plot_interactive(each_target[["Arctagrostis latifolia"  ]]) # Signatures good. 
# plot_interactive(each_target[["Eriophorum angustifolium"]]) # Signatures good. 

# Hard code removes bad spectra taken for each species
each_target[["Alectoria ochroleuca"    ]]<-each_target[["Alectoria ochroleuca"    ]][-c(2),                                ]
each_target[["Arctostaphyllos"         ]]<-each_target[["Arctostaphyllos"         ]][-c(19,13,10,11,16,17)                 ]
each_target[["Asahinea chrysantha"     ]]<-each_target[["Asahinea chrysantha"     ]][-c(3),                                ]
each_target[["Aulacomnium turgidum"    ]]<-each_target[["Aulacomnium turgidum"    ]][-c(3,4),                              ]
each_target[["Bare Soil"               ]]<-each_target[["Bare Soil"               ]][-c(1,3,7),                            ]
each_target[["Betula nana"             ]]<-each_target[["Betula nana"             ]][-c(5,18,26,27,32,43,56),              ]
each_target[["Carex sp."               ]]<-each_target[["Carex sp."               ]][-c(10,12,13,14,15),                   ]
each_target[["Cetraria islandica"      ]]<-each_target[["Cetraria islandica"      ]][-c(1,2,3,4,8),                        ]
each_target[["Cladonia gracilis"       ]]<-each_target[["Cladonia gracilis"       ]][-c(13,14,15,16,17,18),                ]
each_target[["Cladonia mitis"          ]]<-each_target[["Cladonia mitis"          ]][-c(8,12,13,14,15,16,17),              ]
each_target[["Cladonia rangiferina"    ]]<-each_target[["Cladonia rangiferina"    ]][-c(1,2,3,4,17,18),                    ]
each_target[["Cladonia steallaris"     ]]<-each_target[["Cladonia steallaris"     ]][-c(13,14,16,2),                       ]
each_target[["Cladonia stygia"         ]]<-each_target[["Cladonia stygia"         ]][-c(13),                               ]
each_target[["Cladonia uncialis"       ]]<-each_target[["Cladonia uncialis"       ]][-c(6),                                ]
each_target[["Dactylina arctica"       ]]<-each_target[["Dactylina arctica"       ]][-c(3,7),                              ]
each_target[["Dead Salix"              ]]<-each_target[["Dead Salix"              ]][-c(3,4,8),                            ]
each_target[["Dryas alleghenies"       ]]<-each_target[["Dryas alleghenies"       ]][-c(15,22,29,37,38,46,47,48,55,60:272),]
each_target[["Dryas octopetala"        ]]<-each_target[["Dryas octopetala"        ]][-c(10,11,29,33,32,38,41,42,48,61:610),]
each_target[["Empetrum nigrum"         ]]<-each_target[["Empetrum nigrum"         ]][-c(1,3),                              ]
each_target[["Evernia mesomorpha"      ]]<-each_target[["Evernia mesomorpha"      ]][-c(1,5,6,7,8,19,20),                  ]
each_target[["Flavocetraria cucculata" ]]<-each_target[["Flavocetraria cucculata" ]][-c(1,12,14),                          ]
each_target[["Flavocetraria nivalis"   ]]<-each_target[["Flavocetraria nivalis"   ]][-c(11),                               ]
each_target[["Gravel"                  ]]<-each_target[["Gravel"                  ]][-c(5),                                ]
each_target[["Heracleum lanatum"       ]]<-each_target[["Heracleum lanatum"       ]][-c(2),                                ]
each_target[["Hylocomium splendens"    ]]<-each_target[["Hylocomium splendens"    ]][-c(5,6,7),                            ]
each_target[["Hypogymnia austerodes"   ]]<-each_target[["Hypogymnia austerodes"   ]][-c(1,2,3,4),                          ]
each_target[["Icmadophila ericetorum"  ]]<-each_target[["Icmadophila ericetorum"  ]][-c(4,7),                              ]
each_target[["Iris sp."                ]]<-each_target[["Iris sp."                ]][-c(2),                                ]
each_target[["Lupinus sp."             ]]<-each_target[["Lupinus sp."             ]][-c(1,12),                             ]
each_target[["Masonhalea richardsonii" ]]<-each_target[["Masonhalea richardsonii" ]][-c(2),                                ]
each_target[["Nephroma arcticum"       ]]<-each_target[["Nephroma arcticum"       ]][-c(10,11,12,13),                      ]
each_target[["Parmeliopsis ambigua"    ]]<-each_target[["Parmeliopsis ambigua"    ]][-c(4),                                ]
each_target[["Parmelis sulcata"        ]]<-each_target[["Parmelis sulcata"        ]][-c(1,11),                             ]
each_target[["Pedicularis racemosa"    ]]<-each_target[["Pedicularis racemosa"    ]][-c(1,8),                              ]
each_target[["Peltigera apthosa"       ]]<-each_target[["Peltigera apthosa"       ]][-c(3,4,5),                            ]
each_target[["Petasites frigida"       ]]<-each_target[["Petasites frigida"       ]][-c(1),                                ]
each_target[["Polytrichum juniperinum" ]]<-each_target[["Polytrichum juniperinum" ]][-c(1,2,3,4),                          ]
each_target[["Polytrichum sp."         ]]<-each_target[["Polytrichum sp."         ]][-c(1,4),                              ]
each_target[["Quartz"                  ]]<-each_target[["Quartz"                  ]][-c(13),                               ]
each_target[["Rhizocarpon geographicum"]]<-each_target[["Rhizocarpon geographicum"]][-c(8,18),                             ]
each_target[["Rosa acicularis"         ]]<-each_target[["Rosa acicularis"         ]][-c(19),                               ]
each_target[["Rubus sp."               ]]<-each_target[["Rubus sp."               ]][-c(8,6),                              ]
each_target[["Toefeldia sp."           ]]<-each_target[["Toefeldia sp."           ]][-c(5),                                ]
each_target[["Trapelopsis granulosa"   ]]<-each_target[["Trapelopsis granulosa"   ]][-c(1),                                ]
each_target[["Vaccinium vitis+--idea"  ]]<-each_target[["Vaccinium vitis-idea"    ]][-c(1,2,3,4,5,6,7,8),                  ]
each_target[["Vulpicida pinastri"      ]]<-each_target[["Vulpicida pinastri"      ]][-c(1,2,11),                           ]

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
 
 # Saves each target as a spectral object (.rds)
 for(i in 1:length(New_targets)){
   
   # Writes out each target and their spectral profiles
   saveRDS(New_targets[[i]],paste0("Output/Spectral_profiles/",names(New_targets[i]),".rds"))
   
 }

# Lets combine into one spectral library if we are satisfied with our results
Cleaned_Speclib<-Reduce(spectrolab::combine,New_targets)%>% # dim(n_samples=1989, n_wavelegths=2151)
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)

Cleaned_Speclib_rds<-Reduce(spectrolab::combine,New_targets)

# table(Cleaned_Speclib$Class3)%>%as.data.frame()
# Var1 Freq
# 1              Dwarf_Shrub_Decid  177
# 2                           Forb   72
# 3                Graminoid_Grass   24
# 4                Graminoid_Sedge   31
# 5           Lichen_Crustose_Dark   30
# 6          Lichen_Crustose_Light   25
# 7            Lichen_Foliose_Dark   45
# 8  Lichen_Foliose_Dark_Peltigera   26
# 9           Lichen_Foliose_Light    4
# 10         Lichen_Foliose_Yellow   38
# 11         Lichen_Fruticose_Dark   27
# 12        Lichen_Fruticose_Light   39
# 13       Lichen_Fruticose_Yellow   72
# 14                        Litter    5
# 15              Moss_Aulacomnium   10
# 16                Moss_Ceratadon    5
# 17                 Moss_Dicranum    5
# 18              Moss_Hylocomnium   10
# 19              Moss_Plagiomnium    4
# 20               Moss_Pleurozium    4
# 21              Moss_Polytrichum   17
# 22              Moss_Racomitrium    4
# 23                Moss_Rhytidium    6
# 24          Moss_Sphagnum_fuscum    4
# 25           Moss_Sphagnum_other    8
# 26             Moss_Tomenthypnum    2
# 27                          Rock   31
# 28                   Shrub_Alder   44
# 29                  Shrub_Betula   49
# 30               Shrub_Evergreen   77
# 31                    Shrub_Rosa   19
# 32                   Shrub_Salix  111
# 33                          Soil   10
# 34                    Tree_Decid   12
# 35                Tree_Evergreen   17
# 36                   Wood_Coarse    5
write.csv(Cleaned_Speclib, "Output/Cleaned_SpectralLib.csv")
saveRDS(Cleaned_Speclib_rds,"Output/C_004_Cleaned_Speclib.rds")

# Class3 == "Moss_Acrocarp"|Class3 == " Moss_Pleurocarp"|


# Subset a functional group
Comparison_lot<-subset(Cleaned_Speclib,Class3 == "Soil"| Class3 == "Gravel")


# change the dtaframe to a long dataframe 
Comparison_lot<-gather(Comparison_lot ,Wavelength,Reflectance,-1:-9)


# Make column name Wavelength numeric
Comparison_lot$Wavelength    <-as.numeric(Comparison_lot$Wavelength)

# Plot the output
Comparison_lot<-Comparison_lot%>%
  group_by(Class2, Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance))%>%
  as.data.frame()

# Creates a ggplot for each functional group with all species
ggplot(Comparison_lot,aes(Wavelength,Median_Reflectance))+geom_point(aes(color = Class2))+
  labs(color="Species")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=12))+
  labs(title = paste(names_of_classes[[i]]," Spectral Signatures", sep = ""))


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
# Species_table_df<-SpecLib_new_All%>%
#   group_by(Class3,Class2)%>%
#   tally()
# # View(Species_table)
# 
# # Writes out species per functional group dataframe
# Species_table_df%>%
#   write.csv("Output/C_002_SpeciesPer_FunctionalGroup.csv", row.names = F)
# 
# 
# # Function allows the number of scans per species (Class2) to be equal within each functinal group (Class3)
# reduce_SpeciesPerGroup<-function(x){
#   
#   #dataframe that shows scans per species within each functional group
#   Species_table<-x%>%
#     group_by(Class3,Class2)%>%
#     tally()
#   
#   # Creates a list that contains a dataframe for each functional group
#   Func_groups<-split(x,x$Class3)
#   
#   # count per species within each functional group
#   countperspecies<-split(Species_table,Species_table$Class3)
#   
#   # Lets make the Class2 groups equal within each functinal group (Class3)
#   Func_groups_reduced<-Map(x=Func_groups, y = countperspecies, function(x,y){
#     x%>%
#       group_by(Class2)%>%
#       sample_n(pmin(n(), median(y$n)))%>%
#       as.data.frame()
#   })
#   return(Func_groups_reduced)
# }
# 
# # Applies function to spectral library
# SpecLib_reduced<- reduce_SpeciesPerGroup(SpecLib_new_All)
# 
# # Combines the list above into one dataframe
# SpecLib_reduced_df<-do.call("rbind", SpecLib_reduced)
# 
# # str(SpecLib_reduced_df)
# 
# # Writes out dataframe
# SpecLib_reduced_df%>%
#   write.csv("Output/C_003_SpecLib_FunctionalGroupsEqual_DF.csv",row.names = F)
# 
# # Lets check the number of scans we have for each functional category now that we reduced the distribution
# # These will be used in models
# SpecLib_reduced_df %>%
#   group_by(Class3) %>%
#   tally()
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

## Lets convert our new spectral library to a spectral object to be used later 
#SpecLib_reduced_spectra <-spectrolab::as.spectra(SpecLib_reduced_df[-1:-9])
#
## adds metadata
#meta(SpecLib_reduced_spectra)<-data.frame(SpecLib_reduced_df[1:9], stringsAsFactors = FALSE)
#saveRDS(SpecLib_reduced_spectra,"Output/C_004_SpecLib_FunctionalGroupsEqual.rds")

# ------------------------------------------ Plots ---------------------------------------------------

# Creates a vector with the name of all the categories of interest
names_of_classes<-c(as.character(unique(Cleaned_Speclib[,"Class3"])))

# Creates an empty list
FunctionalGroupDf<-list()

for(i in 1:length(names_of_classes)){
  
  # Subset a functional group
  FunctionalGroupDf[[i]]<-subset(Cleaned_Speclib,Class3 == names_of_classes[i])
  
  
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
Speclib_spec<-readRDS("Output/C_004_Cleaned_Speclib.rds")

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
                                        out_file= "Output/",
                                        datatype = "csv", 
                                        extension = FALSE)




