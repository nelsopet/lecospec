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
    * Convert Data Type to ______
    * initialize the bandpass filters
    * initialize base vegetation indices.
2. Split the data into tiles
3. initialize parallel infrastructure
    * broadcast variables as needed
4. In parallel: `for tile in tiles do:`
    * fill in missing values
    * calculate derivatives
    * calculate specific vegetation indices
    * run the model on the tile
5. Merge results
6. Evaluate accuracy, etc. 
7. Clean up

## To deploy as API