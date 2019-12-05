library(randomForest)
library(tidyverse)

##Creates objcet that stores file paths to be used (input and output folders)
#Input
Raw_spec   <-"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019"
Smooth_spec<-"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019"

#Output
output_raw   <-"Processed_spec/Error_rates/Raw"
output_smooth<-"Processed_spec/Error_rates/Smooth"

##Creates a list of all filenames for the different sets of predictors that are PCAs
pca_plants_files_allba <-list.files(path = Raw_spec, pattern = "pca_plants"      , full.names = T)
pca_plants_files_005nm <-list.files(path = Raw_spec, pattern = "pca_005nm_plants", full.names = T)
pca_plants_files_010nm <-list.files(path = Raw_spec, pattern = "pca_010nm_plants", full.names = T)
pca_plants_files_100nm <-list.files(path = Raw_spec, pattern = "pca_050nm_plants", full.names = T)

####Reads in all dataframes for the different sets of predictors that are raw spectra (allbands,5nm,10nm,50nm,100nm)
alaskaspeclib_raw_allba<-lapply(alaskaspeclib_raw_files_allba, read.csv)
alaskaspeclib_raw_005nm<-lapply(alaskaspeclib_raw_files_005nm, read.csv)
alaskaspeclib_raw_010nm<-lapply(alaskaspeclib_raw_files_010nm, read.csv)
alaskaspeclib_raw_050nm<-lapply(alaskaspeclib_raw_files_050nm, read.csv)
alaskaspeclib_raw_100nm<-lapply(alaskaspeclib_raw_files_100nm, read.csv)

##Creates a list of all the dataframes (list within a list)
Raw_spectra<-list(alaskaspeclib_raw_allba
                  ,alaskaspeclib_raw_005nm
                  ,alaskaspeclib_raw_010nm
                  ,alaskaspeclib_raw_050nm
                  ,alaskaspeclib_raw_100nm)