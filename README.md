Lecospec
================

## Laboratory of Ecological Spectroscopy (lecospec)

The purpose lecospec code is to take ground or image based reflectance
spectra, build a classifier or regression, apply that model to an
imaging spectrometer data cube (aka hyperspectral image).

1)  First, install dependencies

    source(“./Scripts/Utilites/install_dependencies.R”)

2)  Then load the package of lecospectR

    source(“./Functions/lecospectR.R”)

The functions in lecospectR are divided into sections, Some functions
are found in lecospectR.R but all of which are loaded by sourcing
/Functions/lecospectR.R.

    /Functions/dataframe_operations.R  
    /Functions/model_support.R 
    /Functions/pfts.R  
    /Functions/pipeline.R  
    /Functions/raster_operations.R 
    /Functions/site_specific_processing.R   
    /Functions/spectral_operations.R
    /Functions/training_utilities.R    
    /Functions/type_conversion.R   
    /Functions/utilities.R 
    /Functions/validation.R
    /Functions/visualization.R    

## How to run lecospec

## Build a spectral library from ground data

1)  Our workflow assumes a list of species with associated levels of
    taxonomic information (eg. functional group membership)
    /Data/SpeciesTable_202230417.csv. This table is used for several
    steps, including aggregating validation to the same taxonomic level
    as training data. This table will be specific to whatever
    classification targets one is working with. To use lecospec,
    associated files are needed that are generated based on this species
    table are created by running the following script:

    /Scripts/utilities/install_dependencies.R

The outputs of this script are written to assets/.

2)  Build a spectral library from a range of field scans collected with
    Tungsten halogen illumination using a leaf clip or contact probe.
    These scripts delete bad scans and standardize the associated
    information into a single metadata format.

    /Scripts/TrainingDataCreation/Ground/2_DataMunging.R  
    /Scripts/TrainingDataCreation/Ground/2B_DataMunging_missing_spectra.R  
    /Scripts/TrainingDataCreation/Ground/3_Create_SpecLibPSR.R

The output of running these scripts is spectral library reflectance and
the metadata.

    /Output/C_001_SC3_Cleaned_SpectralLib.csv   

## Build a spectral library from images

1)  Collect spectra from pixels in images from UAV in quadrats and
    patches of plant functional types visible in ground reference or
    higher resolution RGB imagery. Reflectance spectra are extracted
    from hand digitized patches of plant functional types provided as a
    shapefile to the first script (101_Crop_Training_PFT_vector). The
    same operation is performed in the second script
    (101_Crop_Training_Quads) for hand digitzed quadrats of validation
    ground cover data. Script 102_Parse_training_PFT_vector_spectra adds
    metadata to each pixel by plant patch.

    /Scripts/TrainingDataCreation/Image/101_Crop_Training_PFT_vector.R
    /Scripts/TrainingDataCreation/Image/101_Crop_Training_Quads.R
    /Scripts/TrainingDataCreation/Image/102_Parse_training_PFT_vector_spectra.R
    /Scripts/TrainingDataCreation/Image/103_Clean_training_PFT_vector_spectra.R

2)  After running these scripts, the output is relectance for each pixel
    from the patches of plants digitized from images.

    /Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv

## Compare ground and image spectra

1)  Visually compare the reflectance profiles of plant functional types
    between ground spectra and image spectra. We generated the figure
    below by calculating median reflectance and other quantiles by band,
    source and plant functional type. We then plotted the quantiles (red
    or blue = median, 75% black & 95% grey). Sample sizes (pixels for
    image data, scans for ground data), number of unique taxa and number
    of patches (for image data only) are also calculated and written to
    the header of each panel. These steps are done in the following
    script.

    /Scripts/ExploratoryDataAnalysis/106_Compare_ground_image_spectra.R

Other routines are also in this script, including principle components
analysis and associated visualizations, which would need to be modified
for a new application.

<img src="./Output/Fnc_grp1_spectral_profiles_PFT_GRD_IMG_SPECTRA_ALL.jpg" width="1000" height="1000">

## Study Area and Data locations

The centers of all UAV flights and points where ground scans were
collected are shown in the map. Collecting these locations from the
metadata and image centers for data collected as the Arctic boreal plant
mapping use case for lecospectR using the
/Scripts/ExploratoryDataAnalysis/7_Visualizations_Ground_and_UAS_Spectra_locations.R
which produces the two .kml files plotted in the map below. The image
below it shows one site (Bison Gulch near Denali National Park) with
ground validation quadrat locations (squares) along 100 m long transect
with a white calibration tarp on one end.

<img src="Output/StudyAreaGround_Airborne_Spectra_Locs.jpg" width="480" /><img src="Data/Ground_Validation/Imagery/snaphsots/BisonGulch.jpg" width="1222" />

## Building patch or site balanced test/train splits for the data across different bandpasses and sample sizes

Since each plant functional type has different numbers of species, scans
and sites/patches, balancing the data used in building classification
models requires selecting a train and testing split that accounts for
the differences. Also, the width of each band may change the ability to
separate the different plant functional types. The script below builds
training and testing partitions with different maximum samples sizes
(125, 300, 500, 750, 1000 and 2000 pixels) per plant functional type and
across different bandpasses (5,10,25 and 50nm). Some plant functional
types have fewere than 300 pixels so as the max sample size increases,
so does the bias towards plant functional types with more pixels. The
maximum sample size only applies to the training dataset. The testing
dataset is set at 20 pixels per plant functional type.

    ./build_balanced_ground_data 
    ./build_balanced_training_data

The outputs of these scripts are written to ./Data/v2/ as .csv files,
once for each combination of test/train, wavelength and max sample size
per plant functional type.

## Model training and validation

1)  Set all the input, output and needed associated files for building
    models and predicting images in the following file.

    /Scripts/validation_defs.R

Variable set in this script include: test_paths, which are rasters only
with the pixels by quadrat by site shape_paths, which are the vectors of
the edges of each quadrat by site validation_paths, which are tabular
data of ocular estimated cover of plants by a human based on ground
photos lists of names of quadrat vectors by site (eg. bison_gulch_names
\<-
c(“Bisongulch0”,“Bisongulch60”,“Bisongulch70”,“Bisongulch10”,“Bisongulch80”,“Bisongulch20”,“Bisongulch50”,“Bisongulch30”,“Bisongulch40”))

In this script, The test_paths are set to the output of
/Scripts/TrainingDataCreation/101_Crop_Training_PFT_vector, which are a
set of images with 326 bands from 400-1000nm covering only the square
quadrats 1m x 1m that were hand digitized in different study areas.
Model paths are set for different types to be evaluated. The vector
layers of each hand digitized quadrats are set in tihe shape_paths. The
names of each quadrat are listed manually to standardize across all
since each vector layer of quadrats follow different order and have
slightly different names. The validation_path are the ground cover
estimates by quadrat derived from ground photos by a single expert
observer.

2)  Several types of models are built in lecospec, including two bagged
    regression trees (ranger and adaboost) and partial least squares
    regression (PLS). To build these models, use the following scripts
    at the root of the directory.

    ./gs3_adaboost.R ./gs3_rf.R ./gs3_pls.R

Other models have been tested but not fully explored include support
vector machines (SVM), CART, boosted regression trees (Xgboost).

    ./gs3_svm.R 
    ./gs3_cart.R

Each model script has a similar set up, with the parameters for the
model to be searched described at the beginning of the script.

Each model script has similar settings but some are model
site_specific_processing# model-independent search parameters. \#Max
number of samples per class, in this case PFT max_per_pft \<- c(75, 150)
\#Bandwidths in increments of 5nm bandwidths \<- c(5, 25, 50) \#Cutoff
for variable intercorrelation based on a variable importance run of
randome forest, which identifiied the most important variables
correlation_thresholds \<- c(0.96, 0.97,0.98,0.99) \#Apply filter to
reatures filter_features \<- c(TRUE, FALSE) \#Types of transformations
to apply transform_names \<- c(“Nothing”)

model hyperparameters num_components \<- 2^seq(0, 10) alpha \<- seq(0,
1, 0.1)

4)  Each model output is written to a folder with the same name as the
    model, such as the example below. Each model is given a unique
    identifier when it is built.

    ./mle/experiments/gs/“MODEL UUID HERE”

Model outputs include predictions for each ground validation sample
(quadrat) as a raster as well as histogram comparing the observed
proportion of pixels in each quadrat per plant functional type against
the predicted proportion from lecospec. These are all aggregated into a
single figure called aggergate.html, which shows the observes vs
predicted cover proportion by quadrat, plant funnctional type and site.
All models for a particular algorithm type (eg. pls) have summary
statistics written to a log file at the root with the same name as the
model script (eg. ./gs3_adaboost.R makes a log file at
./gs3_adaboost.csv). The log file makes it easy to sort models by
accuracy (percent pixels correctly identified) and fit (sum of squared
residuals of observed vs predicted values)

<img src="figures/Adaboost_obs_vs_pred.png" width="594" />

5)  After exploring model outputs based on different input data, Pick
    and model and explore results with lecospectR::validate_model.R ,
    whicih calls the input data, models and settings from validate_def.R

    ./gs/models/gs/“MODEL UUID HERE”

6)  Generate predidctions for plant functional type occurence for whole
    datacubes by running the parallelized estimate_landcover function
    from lecospectR. Set the number of tiles carefully based on RAM and
    image size. To run the function lecospectR::estimate_landcover,
    change the settings at the following file.

    ./config.json

The setting included in this config control which model is used, tiling
settings, parallelization, etc..

    automatic_tiling: false   
    max_size: 200    
    x_tiles: 2 # Set to make about 10% of RAM size on machine     
    y_tiles: 2    
    tile_path: "./tiles/" #Intermediate products go here, like /temp. Will need to be cleaned out every so often    
    model_path: "./mle/"INSERT MOD NAME".rda" #Models built in /modelbuilding.ipynb can be pasted here  
    clusterCores: (NUM CORES ON MACHINE - 1) #Speeds up the processing on larger images to have more cores but tradeoff between handling tiles and creating tiles exists    
    parallelize_by_tiles: false 
    key_path: "./fg2key.json"   
    external_bands: "./bands.csv" #Bands used to rename spectral objects consistently along the way 
    output_format: "grd"    
    aggregation: 1 #Depends on levels within data and only relevant for taxonomic-like structured response categories   

7)  Once the settings match what is needed, the following script tests
    the model smaller on a small image. If that succeeds, the script
    shows using the model on large images, converting them to different
    image formats for plotting and then saving a low resolution image of
    the output map of plant functional types (with a legend)

    ./Scripts/ExploratoryDataAnalysis/run_prn.R

Example predicted plant functional type map from one site (Bison Gulch
near Denali National Park)
<img src="./figures/bg_1511_fncgrp1_PREDICTIONS_adaboost.png" width="4200" />
