# ----Creates a spectral library object, combining all the scans collected in Alaksa during 2018 and 2019----

library(spectrolab)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(doParallel)
library(parallel)

# ------------------------------- Data Munging -----------------------------------------------

# Creates a file path to where our spectral libraries for each site is loacated
# Data collected in 2018 and 2019
mypath_atkin = "Output/"

# Reads in species and functional level groups dataframe
Species_groups<-read.csv("Output/B_001_Species_Table.csv")

# Import names of .rds files into character list
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
# table(SpecLib$Class3)%>%as.data.frame()
# The Dwarf shrub category is high because we scanned a high number of Dryas sp. in summer 2019

# Lets remove all the rows with negative values or Values >2
SpecLib_new<-SpecLib%>% 
  dplyr::filter_at(vars(-(ScanID:Area)), all_vars((.) < 2)) %>% # Removes rows with values greater than 2
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
# Remove bad scans
SpecLib_new_All<-SpecLib_new_All[!(SpecLib_new_All$Class2=="Unknown"),]

# ------------------------------------------------- Spectral Library Clean Up --------------------------------------------------
# Script removes bad scans from each species in the library
# First lets check how many species are available and the amount of scans we have for each species
table(SpecLib_new_All$Class2)%>%as.data.frame() # There are 100 targets in our spectral library

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
#         Dryas octopetala  612
#                Dryas sp.   43
#          Empetrum nigrum   12
#        Equisetum arvense    4
#     Equisetum sylvaticum    4
#     Eriophorum vaginatum    5
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
#        Nephroma arcticum   21
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

# First lets create an object with all the names 
target_names<-unique(SpecLib_new_All$Class2)

# Creates an empty list
each_target<-list()

# Function splits the spectrallibrary into spectral objects based on each target (99 Spectral Objects)
for(i in 1:length(target_names)){
  
  # Subset a functional group
  each_target[[i]]<-subset(SpecLib_new_All,Class2 == target_names[i])
  
  # saves metadata
  metadata<-each_target[[i]][,c(1:9)]%>%as.data.frame()
  
  # Convert to a spectral object
  each_target[[i]]<-spectrolab::as.spectra(each_target[[i]][-1:-9])
  
  # Add metadata
  meta(each_target[[i]])<-data.frame(metadata[,c(1:9)], stringsAsFactors = FALSE)
  
}

# Renames each target in list 
each_target<-each_target%>%setNames(target_names)

# Plots spectral profile for each target (You'll have to gor through all 99 targets)
# plot_interactive(each_target[[1]])

# Remove bad scans after looking at spectral profiles
# each_target[[100]]<-each_target[[100]][-c(1,3),]

# Create a new object with clenaed spectral library
# New_targets<-each_target

# for(i in 1:length(New_targets)){
#   
#   # Writes out each target and their spectral profiles
#   saveRDS(New_targets[[i]],paste0("Output/Spectral_profiles/",names(New_targets[i]),".rds"))
#   
# }

# You could read in all these targets using the code below
# listofnames<-list.files("Output/Spectral_profiles",full.names = T) 
#  Targets_speclib<-lapply(listofnames,readRDS)


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




