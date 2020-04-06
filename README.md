# Processing Headwall Hyperspectral Rasters using Tiling and Parallelization:  R (tutorial)
Nelson, P.  
Smith, K.  
Pettit, J.  

## Contents
- [Introduction](#Introduction)
- [Preparing Spectral Libraries](#Introduction)
- [Building classification model](#Introduction)
- [Classifying Hyperspectral Data](#Introduction)
    
### Introduction
This tutorial demonstrates how to use functions developed to read, process and create large spatial (Headwall raster) data sets. In principle, both examples follow the same systematic approach:
1. Preparing your spectral library
2. Building classification model 
3. Classifying Headwall Hyperspectral Raster

JANE'S ERDS (IMAGE HERE)

### Preparing Spectral Libraries
SAMPLE CODE
...# Combines specral libraries from all locations
SpecLib<-Reduce(spectrolab::combine,list_of_SpecLib)%>% # dim(n_samples=1989, n_wavelegths=2151)
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)%>% # Removes unwanted column 
  inner_join(Species_groups,by="PFT")%>% #Joins dataframe with all the species info to our spectral library
  dplyr::select(ScanID,PFT,PFT_2,PFT_3,PFT_4,Area,everything()) #Reorders columns 
FACET PLOT SHOWING SPECTRAL FEATURES OF EACH CLASS...

### Building classification model
SAMPLE CODE
MODEL RESULTS
CONFUSION MATRIX

### Classifying Hyperspectral Data
SAMPLE CODE

IMAGE OF DATACUBE















