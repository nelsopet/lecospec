# Testing

## Some known inputs/outputs are needed

These can be separate files or in test_defs.R

#### Validation

* input raster tile with known values
* dummy model (done)
* example prediction for the known tile & dummy model
* example aggregated result for the above prediction
  * required shapefile for the tile, prediction, validation 
* known (fake) validation data
* input data with some NAs
* example output for aggregating multiple output files

#### Raster/df/spectral Operations

* ideally, known inputs/outputs for each stage of the pipeline

## Other tests

* Check data types for outputs of type conversion
* Check there are no NAs after imputing (count NAs)
* Check `isinstance(object, CLASS)` would be great

## End to End test

* Tests both pipeline and validation

### Testing order:

1. raster/df/spectral
2. pipeline stages
   * imputer
   * type conversion
3. validation
4. End-to-end test
