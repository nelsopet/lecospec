# LECOSPEC ML Pipeline

## Overview

This document describes the overall pipeline (preprocessing, logging, models, etc.).  It is broken down into the following sections:
* Data Loading and input requirements
* Preprocessing
    * Type conversion
    * Data cleaning
    * resampling
    * derivative calculation
    * parallel computing
    * logging
* Machine Learning processing
    * model specifications and requirements
    * application of models
    * logging
* Post-processing
    * Data aggregation
    * logging
* Other notes


## Data loading

Data can be passed to the pipeline in any of the following formats:
* *.tif images
* *.csv files
* A data frame loaded from an external source
* a RasterBrick object loaded from another source

# Current Pipeline

1. Load Data
    * Load data from file
    * initialize the band pass filters
    * initialize base vegetation indices.
    * save X and Y Coordinates from Input to CSV
    * load the classifier (ML Model)
2. Whole dataset pre-processing
    * Convert Data Type to data frame
    * data compression (WIP - currently just select important columns)
    * Fill in missing data with Random Forest regressor (MissForest Algorithm)
    * Split the data into tiles
3. initialize parallel infrastructure
4. In parallel: for each tile do:
    * calculate spectral derivatives
    * calculate specific vegetation indices
    * run the model on the tile (pixel-wise classifier)
5. Merge results
    * Create a data.frame with the same X and Y coordinates as the input (from CSV)
    * Use Model outputs to assign a label (z) to each input point
    * create a column of numeric codes for class labels
    * convert to a raster object as needed
6. Use this (DF or raster) to create images of the output / overlay the input image
7. Evaluate accuracy and performance
8. Clean up as needed

## Publishing

The Lecospec project is currently under development and will be released as an R package soon.  Currently we are targeting a mid-2021 launch.