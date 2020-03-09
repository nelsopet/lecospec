## Creates a spectral library object, combining all the scans collected in Alaksa during 2018 and 2019
## Need to run all scripts in folder "1_FieldSpec/1_By_site_PSR" prior this script if the data is not present in the output folder below
## " OutputsPSR/Processing/PSR/"
## Most of the script includes data munging
## At the end of the script memeroy will be cleaned (to stop this put a # infront the last line of code)
library(spectrolab)
library(tidyverse)
library(plyr)
library(dplyr)


# Reads in species and functional level groups dataframe
Species_groups<-read.csv("OutputsPSR/Processing/PSR/SpecLib_groups.csv")

# Creates a file path to where our spectral libraries for each site is loacated
# Data collected in 2018 and 2019
mypath_atkin = "OutputsPSR/Processing/PSR/"

# Import names of .rds files into character list
SpecLib_by_location = list.files(mypath_atkin, pattern="spectra.rds",full.names = T) 

# Reads in the spectral libraries for each location in a list...List of 13 spectral objects
list_of_SpecLib<-lapply(SpecLib_by_location,readRDS)%>% # Reads in the spectral library for each site 
  setNames(gsub("OutputsPSR/Processing/PSR/","",SpecLib_by_location)) # Removes dir path from the name

# Combines specral libraries from all locations
SpecLib<-Reduce(spectrolab::combine,list_of_SpecLib)%>% # dim(n_samples=1989, n_wavelegths=2151)
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)%>% # Removes unwanted column 
  inner_join(Species_groups,by="PFT")%>% #Joins dataframe with all the species info to our spectral library
  dplyr::select(ScanID,PFT,PFT_2,PFT_3,PFT_4,Area,everything()) #Reorders columns 

# Please note these are the number of samples we have for each functional group
# table(SpecLib$PFT_3)%>%as.data.frame()
# The Dwarf shrub category is high because we scanned a high number of Dryas sp. in summer 2019

# Lets remove all the rows with negative values or Values >2
SpecLib_new<-SpecLib%>% 
  filter_at(vars(-(ScanID:Area)), all_vars(. <2))%>% # Removes rows with values greater than 2
  filter_at(vars(-(ScanID:Area)), all_vars(. >=0)) # Removes row with values less than 0, # dim (nrows = 1984 ncol = 2157)

# Lets check the number of scans we have for each functional group because we removed all the bad scans
# table(SpecLib_new$PFT_3)%>%as.data.frame()

# Lets add more details to our spectral library by adding frequency columns
# Frequency values represent the number of scans per species and the number of scans per functional group
SpecLib_new<-SpecLib_new%>%
  ddply( .(PFT_2), mutate, PFT2_Freq = length(PFT_2))%>% # Add column to data frame that shows frequency of species
  ddply( .(PFT_3), mutate, PFT3_Freq = length(PFT_3))%>% # Add column to data frame that shows frequency of functional group
  ddply( .(PFT_4), mutate, PFT4_Freq = length(PFT_4))%>% # Add column to data frame that shows frequency of courser functional groups
  dplyr::select(ScanID,PFT,PFT_2,PFT_3,PFT_4,Area,PFT2_Freq,PFT3_Freq,PFT4_Freq,everything()) # Rearrange columns 

# Convers our new spectral library back to a spectral object to be saved
# Spectral object with 1984 samples with spectral range of 350-2500nm
SpecLib_spectra<-spectrolab::as.spectra(SpecLib_new[,c(-1:-9)])
# str(SpecLib_spectra)

# Adds metadata to new Spectral Library 
# Final product is a spectral object with 1984 samples with spectral range of 350-2500nm and  9 variables being metadata
meta(SpecLib_spectra)<-data.frame(SpecLib_new[,c(1:9)], stringsAsFactors = FALSE)
saveRDS(SpecLib_spectra,"OutputsPSR/Processing/PSR/alaskaSpecLib.rds")

# Creates a dataframe that shows scans per species within each functional group
Species_table_df<-SpecLib_new%>%
  group_by(PFT_3,PFT_2)%>%
    tally()
# View(Species_table)

# Writes out species per functional group dataframe
Species_table_df%>%
  write.csv("OutputsPSR/Processing/PSR/alaskaSpecies_Pergroup.csv")


# Function allows the number of scans per species (PFT_2) to be equal within each functinal group (PFT_3)
reduce_SpeciesPerGroup<-function(x){
  
  #dataframe that shows scans per species within each functional group
  Species_table<-x%>%
    group_by(PFT_3,PFT_2)%>%
    tally()
  
  # Creates a list that contains a dataframe for each functional group
  Func_groups<-split(x,x$PFT_3)
  
  # count per species within each functional group
  countperspecies<-split(Species_table,Species_table$PFT_3)
  
  # Lets make the PFT_2 groups equal within each functinal group (PFT_3)
  Func_groups_reduced<-Map(x=Func_groups, y = countperspecies, function(x,y){
    x%>%
      group_by(PFT_2)%>%
      sample_n(pmin(n(), median(y$n)))%>%
      as.data.frame()
  })
  return(Func_groups_reduced)
}

# Applies function to spectral library
SpecLib_reduced<- reduce_SpeciesPerGroup(SpecLib_new)
  
# Combines the list above into one dataframe
SpecLib_reduced_df<-do.call("rbind", SpecLib_reduced)

#Writes out dataframe
SpecLib_reduced_df%>%
  write.csv("OutputsPSR/Processing/PSR/alaskaSpecLib_reduced_df.csv",row.names = F)

# Lets check the number of scans we have for each functional category now that we reduced the distribution
# SpecLib_reduced_df %>%group_by(PFT_3) %>%tally()

# Lets convert our new spectral library to a spectral object to be used later 
SpecLib_reduced_spectra <-spectrolab::as.spectra(SpecLib_reduced_df[-1:-9])

# adds metadata
meta(SpecLib_reduced_spectra)<-data.frame(SpecLib_reduced_df[1:9], stringsAsFactors = FALSE)
saveRDS(SpecLib_reduced_spectra,"OutputsPSR/Processing/PSR/alaskaSpecLib_reduced.rds")

# Function creates a plot by functional groups
# Need to fix this
#ggplot_data<-lapply(Func_groups,function(x){
#  x%>%
#    gather(Wavelength,Reflectance,-1:-9) # coverts each functional group dataframe to a long dataframe
#})%>%
#  lapply(function(y) {y[10] <- lapply(y[10], as.numeric);y
#  })%>%
#  lapply(function(z) {z[3]  <- lapply(z[3], as.factor);z
#  }) 
#
#
#
#ggplot_data[["Moss_Sphagnum"]]%>%
#  group_by(PFT_2, Wavelength)%>%
#  mutate(Median_reflectance = median(Reflectance))%>%
#  ggplot(aes(Wavelength,Median_Reflectance)) + geom_line(aes(color = PFT_2)) +
#  theme(panel.background = element_rect(fill = "White", colour = "grey50"), 
#        legend.key.size = unit(0.3, "cm"),legend.text = element_text(size=3))+
#  labs(title=paste("Moss_Sphagnum","Spectral Signature", sep = " "))
#
## Lets Make plots of each functional group
##Need to correct this function
#my_plots = lapply(ggplot_data, function(plot_create1) {
#    plot_create1%>%
#    group_by(PFT_2, Wavelength)%>%
#    mutate(Median_Reflectance = median(Reflectance))
#})%>%
#  lapply(function(plot_create2){
#    plot_create2%>%
#      ggplot(aes(Wavelength,Median_Reflectance)) + geom_line(aes(color = PFT_2)) +
#      theme(panel.background = element_rect(fill = "White", colour = "grey50"), 
#            legend.key.size = unit(0.3, "cm"),legend.text = element_text(size=3))+
#      labs(title=paste(plot_create2,"Spectral Signature", sep = " "))
#  })
#  
#
#lapply(names(my_plots), 
#       function(x) ggsave(filename=paste("Outputs/1_Field_spec/1_Processing/PSR_data/PSR_ProcessedSpec/",x,".jpeg",sep=""), plot=my_plots[[x]])) # Save Plots

# Cleans up R memeory
rm(list=ls())






