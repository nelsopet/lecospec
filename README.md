# Processing Fine-scale Hyperspectral Rasters using Tiling and Parallelization:  R (tutorial)

## Authors & Contributors 
*1. Dr. Peter Nelson*      
*2. Kevaughan Smith*  
*3. Jane Pettit*  
*4. Catherine Chan*  

## Contents

- [Introduction](#Introduction)
- [Approaches to fine-scale image classification](#Approaches-to-fine-scale-image-classification)
- [Building classification model](#Building-classification-model)
- [Classifying Hyperspectral Data](#Classifying-Hyperspectral-Data)
- [References](#References)
    
### Introduction
The electromagnetic spectrum is composed of thousands of bands representing different types of light energy. Imaging spectrometers (instruments that collect hyperspectral data) break the electromagnetic spectrum into groups of bands that support classification of objects by their spectral properties on the Earth's surface. Hyperspectral data consists of many bands - up to hundreds of bands - that cover the electromagnetic spectrum.  

The Headwall Micro A-series VINIR imaging spectrometer collects data within the 400 nm to 1000 nm portions of the electromagnetic spectrum within bands that are approximately 2 nm in width. This results in a hyperspectral data cube that contains approximately 326 bands. However (say something about noisey bands in the 900-100nm wavelength region)

This tutorial demonstrates how to use functions created by our team to read, process and create large spatial (Headwall raster) data sets. In principle, both examples follow the same systematic approach:
1. Preparing your spectral library from field spectroradiometers
2. Preparing your spectral library from digitized pixels
2. Building classification model 
3. Classifying Headwall Hyperspectral Images 

*Click [here](https://drive.google.com/drive/u/2/folders/1HIgyxhXO0kYDXYohvymxGupc2yR0k1-4) to download the data and output folder to the repository after cloning:*

Packages used in this tutorial include:
```
library(spectrolab)
library(tidyverse)
library(raster)
library(SpaDES)
library(doParallel)
library(hsdar)
library(caret)
library(randomForest)
library(randomcoloR)
```

### Approaches to fine-scale image classification
Ground truthed data for image classification can be collected using:
1. Portable Field spectrometers
2. Digitizing pixels of known classes in your image

#### The Portable Field Spectrometer Approach
The one used here was a (Spectral Evolution PSR+) which covers the full spectrum (350-2500 nm) and have 1nm wide bands. Note that the spectrtal range and width of this field spec exceeds that of the imaging sensor (400nm - 1000nm, 2nm wide bands). Hence, resampling these bandpasses to match the sensor bandpasses is essential.  

Lets take a look at our spectral library before we resample the bands to match our sensor (user would have already done some data munging).

```
# Note that this is a spectral object
SpectralLibrary<-readRDS("Output/C_004_SpecLib_FunctionalGroupsEqual_DF.rds")
str(SpectralLibrary)

spectra object 
number of samples: 748 
wavelengths: 350 to 2500 (2151 bands)
metadata (3 of 9): ScanID, Class1, Class2, ...
```  
Note that we have a total of 748 samples. We can convert to a dataframe and look at its features.
```
SpectralLibrary<-SpectralLibrary%>%
                    as.data.frame()%>%
                    dplyr::select(-sample_name)
str(SpectralLibrary)
    
'data.frame':	748 obs. of  2160 variables:
 $ ScanID     : chr  "salsic_AK2018_07" "salsic_AK2018_05" "salsic_AK2018_03" "salsic_AK2018_06" ...
 $ Class1     : chr  "dead salix" "dead salix" "dead salix" "dead salix" ...
 $ Class2     : Factor w/ 119 levels "Abies balsamea",..: 36 36 36 36 36 36 75 75 75 75 ...
 $ Class3     : Factor w/ 19 levels "Abiotic_Litter",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Class4     : Factor w/ 9 levels "Abiotic","Dwarf Shrub",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Area       : chr  "AK2018" "AK2018" "AK2018" "AK2018" ...
 $ Class2_Freq: int  8 8 8 8 8 8 5 5 5 5 ...
 $ Class3_Freq: int  13 13 13 13 13 13 13 13 13 13 ...
 $ Class4_Freq: int  58 58 58 58 58 58 58 58 58 58 ...
 $ 350        : num  0.159 0.178 0.15 0.178 0.176 ...
 $ 351        : num  0.156 0.174 0.147 0.174 0.172 ...
 $ 352        : num  0.154 0.171 0.144 0.17 0.169 ...
 $ 353        : num  0.152 0.168 0.14 0.166 0.165 ...
 $ 354        : num  0.15 0.165 0.138 0.163 0.163 ...
 $ 355        : num  0.148 0.163 0.136 0.162 0.161 ...
 $ 356        : num  0.147 0.162 0.134 0.162 0.161 ...
 $ 357        : num  0.146 0.161 0.133 0.162 0.16 ...
 $ 358        : num  0.144 0.16 0.133 0.16 0.157 ...
 $ 359        : num  0.143 0.158 0.132 0.158 0.155 ...
 $ 360        : num  0.14 0.156 0.129 0.154 0.152 ...
....................................................
 $ 2500    : num  0.14 0.156 0.129 0.154 0.152 ...
```
Note that there are different classes types (class 1, class 2, class 3, class 4) and wavelengths across the full spectrum (350-2500 nm. However, we're intrested in one class (Class 3 = functional groups of plant species).  
Lets take a look at the categories in  Class 3
```
 SpectralLibrary %>%
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
```
Note that there are 19 different functional groups that will be used for classification. Lets resample the banpasses to match the sensor. 

First we'll need the bandpasses from headwall. You can  find those [here](https://github.com/nelsopet/lecospec/blob/master/Scripts/3_Classify_Image.R#L17-L47)
``` 
# Creates a vector of the bandpasses for the headwall sensor that will be used
# Noisey band were omitted (only bands 1:272 below)
Headwall_bandpasses<-c() 

```

Now we can use the function below to resample
```
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

 return(goodscans)
 }

# Apply function
SpecLib_resampled<-ResampBands(SpectralLibrary)               
  ```
Now we can take a look at our new spectral library
```
str(SpecLib_resampled)

'data.frame':	747 obs. of  281 variables:
 $ ScanID     : chr  "salsic_AK2018_05" "salsic_AK2018_07" "salsic_AK2018_03" "salsic_AK2018_08" ...
 $ Class1     : chr  "dead salix" "dead salix" "dead salix" "dead salix" ...
 $ Class2     : Factor w/ 119 levels "Abies balsamea",..: 36 36 36 36 36 36 75 75 75 75 ...
 $ Class3     : Factor w/ 19 levels "Abiotic_Litter",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Class4     : Factor w/ 9 levels "Abiotic","Dwarf Shrub",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Area       : chr  "AK2018" "AK2018" "AK2018" "AK2018" ...
 $ Class2_Freq: int  8 8 8 8 8 8 5 5 5 5 ...
 $ Class3_Freq: int  13 13 13 13 13 13 13 13 13 13 ...
 $ Class4_Freq: int  58 58 58 58 58 58 58 58 58 58 ...
 $ 397.593    : num  0.0769 0.0706 0.0545 0.0598 0.0758 ...
 $ 399.444    : num  0.0756 0.0696 0.0533 0.0589 0.0744 ...
 $ 401.296    : num  0.0739 0.0683 0.0517 0.0575 0.0727 ...
 $ 403.148    : num  0.0718 0.0667 0.0498 0.0557 0.0706 ...
 $ 405        : num  0.0695 0.0649 0.0477 0.0536 0.0683 ...
 $ 406.851    : num  0.067 0.063 0.0455 0.0514 0.0658 ...
 $ 408.703    : num  0.0646 0.0611 0.0434 0.0492 0.0634 ...
 $ 410.555    : num  0.0623 0.0593 0.0414 0.047 0.0611 ...
 $ 412.407    : num  0.0603 0.0576 0.0398 0.0452 0.059 ...
 $ 414.258    : num  0.0586 0.0562 0.0385 0.0436 0.0573 ...
 $ 416.11     : num  0.0571 0.0552 0.0374 0.0423 0.0559 ...
 $ 417.962    : num  0.056 0.0545 0.0367 0.0414 0.0549 ...
...........................................................
 $ 899.424    : num  0.0508 0.0607 0.0378 0.0381 0.0535 ...
 
 # Writes out dataframe as a .csv file
 write.csv(SpecLib_resampled,
           file = paste(outputs_folder,"D_001_",
                        "SpecLib_resampled.csv",sep=""), row.names = F)
```
Notice the change in the bandpasses and the number of variables in your new dataframe. We can now use the [HyperSpec_DerivGenerator](https://github.com/nelsopet/lecospec/blob/111338ecb754bbdc3861a86ecf68eb2757204315/Functions/Spectral_classifier.R#L3-L26) function to calculate the derivatives. These are saved on disk because of R's memory limit.
```
# Source the function that will calculate derivatives of our new spectral library
source("Functions/HyperspecGenFunction.R")

# Calculate Derivative for spectral libaray
# Note that the output file will be save to the "Output/" folder
HyperSpec_DerivGenerator("Output/D_001_SpecLib_resampled.csv", out_file= "Output/")

```
Once that's done we can go ahead and build our model with the [spectral classifier](https://github.com/nelsopet/lecospec/blob/111338ecb754bbdc3861a86ecf68eb2757204315/Functions/Spectral_classifier.R#L3-L26) function.
```
# Source Function That will Build randomForest model
source("Functions/Spectral_classifier.R")

# Build Model
RF_MOD<-Spectral_classifier("Output/SpecLib_resampled_Derivs.csv")

```
Lets take a look at our model (Need to edit this with more model info and updated approach using the ranger package)
```
Call:
 randomForest(formula = Classes ~ ., data = Spectral_Library,      mtry = sqrt(ncol(Spectral_Library)), ntree = 1001, localImp = TRUE) 
               Type of random forest: classification
                     Number of trees: 1001
No. of variables tried at each split: 16

        OOB estimate of  error rate: 30.12%
```

#### The didgitized pixel Approach


### Building classification model
We can also visualize a confusion matrix CONFUSION MATRIX (Place Holder)
```
 RF_MOD$confusion
 
                 Abiotic_Litter Abiotic_Rock Abiotic_Soil Dwarf_Shrub_Broad Dwarf_Shrub_Needle Forb Graminoid_Grass
Abiotic_Litter                  4            0            0                 0                  1    0               0
Abiotic_Rock                    1           13            5                 0                  0    0               0
Abiotic_Soil                    0            5            2                 0                  0    0               0
Dwarf_Shrub_Broad               0            0            0                79                  0    3               0
Dwarf_Shrub_Needle              0            0            0                 4                 13    0               0
Forb                            0            0            0                 1                  0   28               0
Graminoid_Grass                 0            0            0                 0                  0    0               3
Graminoid_Sedge                 0            0            0                 1                  0    1               0
Lichen_Dark                     0            0            0                 4                  0    2               0
Lichen_Light                    0            0            0                 0                  0    2               0
Lichen_Yellow                   0            0            0                 1                  0    0               0
Moss_Acrocarp                   0            0            0                 0                  2    0               0
Moss_Pleurocarp                 0            0            0                 0                  0    0               0
Moss_Sphagnum                   0            0            0                 0                  0    0               0
```

### Classifying Hyperspectral Data
SAMPLE CODE (use a function to created a predicted layer of a datacube)
```
PredictedRaster_Layer<-HyperspecImageClassifier(filename = Path/to/datacube.envi,
                                                spectral_Library = path/to/SpectralLibrary.csv,
                                                Out_file = path/to/outputfile)
  ```

### HypIMGPredictor_generator Entity relationship diagram
JANE'S ERDS (IMAGE HERE)
![](HypIMGPredictor_generator_v.2.jpeg)

### References














