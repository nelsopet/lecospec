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
  ddply( .(PFT_2), mutate, PFT2_Freq = length(PFT_2))%>% # Add column to data frame that shows frequency of species
  ddply( .(PFT_3), mutate, PFT3_Freq = length(PFT_3))%>% # Add column to data frame that shows frequency of functional group
  ddply( .(PFT_4), mutate, PFT4_Freq = length(PFT_4))%>% # Add column to data frame that shows frequency of courser functional groups
  dplyr::select(ScanID,PFT,PFT_2,PFT_3,PFT_4,Area,PFT2_Freq,PFT3_Freq,PFT4_Freq,everything()) # Rearrange columns 
FACET PLOT SHOWING SPECTRAL FEATURES OF EACH CLASS 

### Building classification model
SAMPLE CODE
MODEL RESULTS
CONFUSION MATRIX

### Classifying Hyperspectral Data
SAMPLE CODE

IMAGE OF DATACUBE















