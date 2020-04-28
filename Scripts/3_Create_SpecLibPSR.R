## ----Creates a spectral library object, combining all the scans collected in Alaksa during 2018 and 2019----

library(spectrolab)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)

## ------------------------------- Data Munging -----------------------------------------------

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
  filter_at(vars(-(ScanID:Area)), all_vars((.) < 2)) %>% # Removes rows with values greater than 2
  filter_at(vars(-(ScanID:Area)), all_vars((.) >=0)) # Removes row with values less than 0, # dim (nrows = 1984 ncol = 2157)

# Lets check the number of scans we have for each functional group because we removed all the bad scans
# table(SpecLib_new$Class3)%>%as.data.frame()

# Lets add more details to our spectral library by adding frequency columns
# Frequency values represent the number of scans per species and the number of scans per functional group
SpecLib_new<-SpecLib_new%>%
  ddply( .(Class2), mutate, Class2_Freq = length(Class2))%>% # Add column to data frame that shows frequency of species
  ddply( .(Class3), mutate, Class3_Freq = length(Class3))%>% # Add column to data frame that shows frequency of functional group
  ddply( .(Class4), mutate, Class4_Freq = length(Class4))%>% # Add column to data frame that shows frequency of courser functional groups
  dplyr::select(ScanID,Class1,Class2,Class3,Class4,Area,Class2_Freq,Class3_Freq,Class4_Freq,everything()) # Rearrange columns 

# Writes out our spectral library as a .csv
write.csv(SpecLib_new,"Output/C_001_Spectral_Library_all.csv", row.names = F)

# Convers our new spectral library back to a spectral object to be saved
# Spectral object with 1984 samples with spectral range of 350-2500nm
# SpecLib_spectra<-spectrolab::as.spectra(SpecLib_new[,c(-1:-9)])
# str(SpecLib_spectra)

# Adds metadata to new Spectral Library 
# Final product is a spectral object with 1984 samples with spectral range of 350-2500nm and  9 variables being metadata
# meta(SpecLib_spectra)<-data.frame(SpecLib_new[,c(1:9)], stringsAsFactors = FALSE)
# saveRDS(SpecLib_spectra,"Outputs/alaskaSpecLib.rds")

# ------------------------------------------------- Even distribution among species --------------------------------------------

# Creates a dataframe that shows scans per species within each functional group
Species_table_df<-SpecLib_new%>%
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
SpecLib_reduced<- reduce_SpeciesPerGroup(SpecLib_new)
  
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

# Lets convert our new spectral library to a spectral object to be used later 
SpecLib_reduced_spectra <-spectrolab::as.spectra(SpecLib_reduced_df[-1:-9])

# adds metadata
meta(SpecLib_reduced_spectra)<-data.frame(SpecLib_reduced_df[1:9], stringsAsFactors = FALSE)
saveRDS(SpecLib_reduced_spectra,"Output/C_004_SpecLib_FunctionalGroupsEqual.rds")

# ------------------------------------------ Plots ---------------------------------------------------
# Plot_Func<-function(x, Class){
#   
#   # Creates a vector with the name of all the categories of interest
#   names_of_classes<-c(as.character(unique(x[,Class])))
#   
#   # Creates an empty list
#   FunctionalGroupDf<-list()
#   
#   for(i in 1:length(names_of_classes)){
#     
#     # Subset a functional group
#     FunctionalGroupDf[[i]]<-subset(x,Class == names_of_classes[i])
#     
#     
#     # change the dtaframe to a long dataframe 
#     FunctionalGroupDf[[i]]<-gather(x ,Wavelength,Reflectance,-1:-9)
#     
#     
#     # Make column name Wavelength numeric
#     FunctionalGroupDf[[i]]$Wavelength    <-as.numeric(FunctionalGroupDf[[i]]$Wavelength)
#     
#     # Create an empt jpeg
#     # jpeg(paste("Output/","C_005","_",names_of_classes[[i]],".jpg",sep =""), units="px",height = 1400, width=2400, res=350)
#     
#     # Plot the output
#     FunctionalGroupDf[[i]]%>%
#       group_by(Class2, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
#       ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color = Class3.))+ 
#       theme(panel.background = element_rect(fill = "white", colour = "grey50"), legend.key.size = unit(0.3, "cm"),legend.text = element_text(size=3))+
#       labs(title = paste(names_of_classes[[i]],"Spectral Signatures", sep = ""))
#     
#     ggsave(paste("Output/","C_005","_",names_of_classes[[i]],".png",sep =""))
#     
#     }
#   
# }
# 
# # Create Outputs
# Plot_Func(SpecLib_reduced_df, Class = "Class3")







