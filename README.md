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
    as training data.

2)  Build a spectral library from a range of field scans collected with
    Tungsten halogen illumination using a leaf clip or contact probe.
    These scripts delete bad scans and standardize the associated
    information into a single metadata format. Around 90 vegetation
    indices are also calculated and the narrow band reflectance is
    resampled and smoothed to 5 nm bands.

    /Scripts/TrainingDataCreation/Ground/2_DataMunging.R  
    /Scripts/TrainingDataCreation/Ground/2B_DataMunging_missing_spectra.R  
    /Scripts/TrainingDataCreation/Ground/3_Create_SpecLibPSR.R

The output of running these scripts is spectral library reflectance and
the metadata.

    /Output/C_001_SC3_Cleaned_SpectralLib.csv   

## Build a spectral library from images

Collect spectra from pixels in images from UAV in quadrats and patches
of plant functional types visible in ground reference or higher
resolution RGB imagery. Calculate vegetation indices, smooth and
resample spectra to 5 nm wide bands. Reflectance spectra are extracted
from hand digitized patches of plant functional types provided as a
shapefile to the first script (101_Crop_Training_PFT_vector). The same
operation is performed in the second script (101_Crop_Training_Quads)
for hand digitzed quadrats of validation ground cover data. Script
102_Parse_training_PFT_vector_spectra adds metadata to each pixel by
plant patch.

    /Scripts/TrainingDataCreation/Image/101_Crop_Training_PFT_vector.R
    /Scripts/TrainingDataCreation/Image/101_Crop_Training_Quads.R
    /Scripts/TrainingDataCreation/Image/102_Parse_training_PFT_vector_spectra.R
    /Scripts/TrainingDataCreation/Image/103_Clean_training_PFT_vector_spectra.R

After running these scripts, the output is relectance for each pixel
from the patches of plants digitized from images.

    /Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv   

After cleaning scans, the reflectance data can be summarized and
visualized in various ways. For example, running elements of the
/Scripts/ExploratoryDataAnalysis/7_Visualizations_JGR_Bio_Tundra_Refl.R
produce the the figure below, which shows the median and interquartile
ranges of reflectance (75% black & 95% grey) with the sample size in
number of scans distributed across a number of scans (ground measured)
or pixels (airborne).

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

## Model training and validation

1)  Set all the input, output and needed associated files for building
    models and predicting images

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

2)  Proprocessing spectral libraries Ground and image based spectral
    libraries created in earlier steps are resampled at different
    bandpasses and balanced samples are created based on target classes
    (eg. PFTs) and sample units (eg. patches for image spectra or sites
    for ground spectra). It creates a training and test split for each
    dataset by randomly selecting pixels up to a specified number per
    class and sample unit. These train and test splits are written to
    disk in /Data/gs in x_train or x_test (independent variables) and
    y_train and y_test (dependent variables).

/Scripts/build_balanced_ground_data.R
/Scripts/build_balanced_training_data.R

3)  Builds and visualizes model accuracy

Several scripts build, validate and visualize accuracy of different
kinds of models; adaboost, CART, partial least squares regression linear
discriminant analysis (PLS-LDA), random forests implemented in the
ranger package and support vector machines.

    /Scripts/gs3_adaboost.R   
    /Scripts/gs3_cart.R   
    /Scripts/gs3_pls.R
    /Scripts/gs3_rf.R   
    /Scripts/gs3_svm.R

Each model script has similar settings but some are model
site_specific_processing# model-independent search parameters. \#Max
number of samples per class, in this case PFT max_per_pft \<- c(75, 150)
\#Bandwidths in increments of 5nm bandwidths \<- c(5, 25, 50) \#Cutoff
for variable intercorrelation based on a variable importance run of
randome forest, which identifiied the most important variables
correlation_thresholds \<- c(0.96, 0.97,0.98,0.99) \#Apply filter to
reatures filter_features \<- c(TRUE, FALSE) \#Types of transformations
to apply transform_names \<- c(“Nothing”)

# model hyperparameters

num_components \<- 2^seq(0, 10) alpha \<- seq(0, 1, 0.1)

Here is an example confusion matrix from a model showing
misclassification between plant funcational types.

4)  After exploring models based on different input data, Pick and model
    and explore results with lecospectR::validate_model.R , whicih calls
    the input data, models and settings from validate_def.R

    /mle/“MODEL UUID HERE”

5)  Generate predidctions for plant functional type occurence for whole
    datacubes by running the parallelized estimate_landcover function
    from lecospectR. Set the number of tiles carefully based on RAM and
    image size. To run the function lecospectR::estimate_landcover,
    check the settings in the /config.json. The settings include

    automatic_tiling: false  
    max_size: 200  
    x_tiles: 2 \# Set to make about 10% of RAM size on machine  
    y_tiles: 2  
    tile_path: “./tiles/” \#Intermediate products go here, like /temp.
    Will need to be cleaned out every so often  
    model_path: “./mle/”INSERT MOD NAME”.rda” \#Models built in
    /modelbuilding.ipynb can be pasted here  
    clusterCores: (NUM CORES ON MACHINE - 1) \#Speeds up the processing
    on larger images to have more cores but tradeoff between handling
    tiles and creating tiles exists  
    parallelize_by_tiles: false key_path: “./fg2key.json”  
    external_bands: “./bands.csv” \#Bands used to rename spectral
    objects consistently along the way output_format: “grd”  
    aggregation: 1 \#Depends on levels within data and only relevant for
    taxonomic-like structured response categories

Once the /config.json is set to match what is needed, the following
script shows specifying a single large image and smaller images used in
estimate_landcover.

    /Scripts/run.R   

6)  Visualize maps of full image output showing plant functional types

    /Scripts/visualizeRasters.R

Example predicted plant functional type map from one site (Bison Gulch
near Denali National Park)
<img src="Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree_FIGURE.jpeg" width="645" />
