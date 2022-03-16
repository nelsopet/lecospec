# Notes on the LecoSpectR package

## Priorities
1. Integration Test
2. Validation / Accuracy Assessment
3. 

## API Changes
1. Update `process_tile` to `process_file`
2. Update the `estimate_land_cover` to use traditional functional arguments
3. create a function that runs `estimate_land_cover` from a config file
4. Others?  ASk about Code review
Revisit after integration test and accuracy assessment.


## Student Projects
* Data labeling app for validation ()
* GPU processing of pipeline
* Image segmentation: segment images into plants (from spectra or prediction)

## Code Review


## Bug Fixes

## Refactor
1. Add documentation
2. remove old code

## Testing
1. Integration Test



`RhpcBLASctl::blas_set_num_threads(1L) RhpcBLASctl::omp_set_num_threads(1L) sink("C:/Users/kenne/Documents/GitHub/lecospec/lecospec.log", append = TRUE)`


use chi-squared test for validation


Use this for the merges:
template<- projectRaster(from = r2, to= r1, alignOnly=TRUE)
#template is an empty raster that has the projected extent of r2 but is aligned with r1 (i.e. same resolution, origin, and crs of r1)
r2_aligned<- projectRaster(from = r2, to= template)
r_merged<- merge(r1,r2) 
r_merged2<- mosaic(r1,r2, fun=mean, na.rm=TRUE)