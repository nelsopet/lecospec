# Processing Headwall Hyperspectral Rasters using Tiling and Parallelization:  R (tutorial)
Nelson, P. Smith, K. Pettit,J.

- [Introduction]
- [Preparing Spectral Libraries](#Introduction)
- [Building classification model](#Introduction)
- [Classifying Hyperspectral Data](#Introduction)
    
  

### Getting Started
Install Rstudio on your device 
https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/

Go to google drive and dowlodad output folder into the repository:https://drive.google.com/open?id=1YTQfNiGCWVxQdEk2KoPCxZDTyLhh79ZV

## Folders
There are three main folders in this repository. The folders contain data that is either raw, processed or output data. 

### Original_data
The folder contains all the original, unprocessed datasets collected in the field.

##### Feld_spec
These are all the scans collecetd by the PSR+ 3500 in Alaksa and Maine during the years 2018 and 2019. Scans were grouped into subfolders based on the respective locations we visited in each state. 

##### Test_imagery
A subset of a hyperspectral image captured with our Micro A-series VINIR imaginging spectroradiometer. 

### Outputs
Folder contains all the  outputs generated after processing and model building. Both hyperspectral image outputs and Field spec output scan be found here.

##### 1_Field_spec
These are all the outputs of the processed data from the field spectroradiometer. Data such as the spectral library with alluncalibrarted scans removed can be found here. Model outputs for the species or functional groups prediction based on field spectroradiometry can also be found here. 

##### 2_HDW_Imagery
These are all the outputs from processing the subset of our hyperspectral image. Processed datasets such as vegitation indices calculations and principal components based on the reflectance values within each pixel of the image can be found here. 

### Scripts
This folder contains detailed scripts for all the processes carried out in the epository. There are 4 subfolders with scripts for image processing, image modeling, field spec processing and field spec modeling. There is an aditional folder with all the old scrits that were used in the past to analyze the data collected. 

The folders are numbered in the order in which they should be ran. However, all the scritpts can be ran independently because all output datasets are saved within the repository.












