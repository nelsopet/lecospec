# Processing Headwall Hyperspectral Rasters using Tiling and Parallelization:  R (tutorial)

## Author & Contributor List
Dr. Peter Nelson      
Kevaughan Smith   
Jane Petite  
Catherine Chan  

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

### Preparing Spectral Libraries using PSR Data
SAMPLE CODE  
``` 
SpecLib<-Reduce(spectrolab::combine,list_of_SpecLib)%>% # dim(n_samples=1989, n_wavelegths=2151)
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)%>% # Removes unwanted column 
  inner_join(Species_groups,by="PFT")%>% #Joins dataframe with all the species info to our spectral library
  dplyr::select(ScanID,PFT,PFT_2,PFT_3,PFT_4,Area,everything()) #Reorders columns  
  ```
  
FACET PLOT SHOWING SPECTRAL FEATURES OF EACH CLASS

![](/Cladonia.jpg)

### Preparing Spectral Libraries using didgitized Raster Pixels
SAMPLE CODE  
``` 
SpecLib<-Reduce(spectrolab::combine,list_of_SpecLib)%>% # dim(n_samples=1989, n_wavelegths=2151)
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)%>% # Removes unwanted column 
  inner_join(Species_groups,by="PFT")%>% #Joins dataframe with all the species info to our spectral library
  dplyr::select(ScanID,PFT,PFT_2,PFT_3,PFT_4,Area,everything()) #Reorders columns  
  ```

### Building classification model
SAMPLE CODE  
``` 
SpecLib<-Reduce(spectrolab::combine,list_of_SpecLib)%>% # dim(n_samples=1989, n_wavelegths=2151)
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)%>% # Removes unwanted column 
  inner_join(Species_groups,by="PFT")%>% #Joins dataframe with all the species info to our spectral library
  dplyr::select(ScanID,PFT,PFT_2,PFT_3,PFT_4,Area,everything()) #Reorders columns  
  ```
MODEL RESULTS
```
Call:
 randomForest(formula = Classes ~ ., data = newdf, mtry = sqrt(ncol(newdf)),      ntree = 1001, localImp = TRUE) 
               Type of random forest: classification
                     Number of trees: 1001
No. of variables tried at each split: 7

        OOB estimate of  error rate: 25.69%
CONFUSION MATRIX  
```  

### Classifying Hyperspectral Data
SAMPLE CODE
```
SpecLib<-Reduce(spectrolab::combine,list_of_SpecLib)%>% # dim(n_samples=1989, n_wavelegths=2151)
  as.data.frame()%>% # Converts Spectral Object to a dataframe
  dplyr::select(-sample_name)%>% # Removes unwanted column 
  inner_join(Species_groups,by="PFT")%>% #Joins dataframe with all the species info to our spectral library
  dplyr::select(ScanID,PFT,PFT_2,PFT_3,PFT_4,Area,everything()) #Reorders columns  
  ```

IMAGE OF DATACUBE  

![](/EightMileTest_Plot_Prediction.jpg)
















