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