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


## See raster::approxNA()


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


preprocess_raster_to_df <- function(raster_obj, model) {
  if(is.na(mean(values(raster_obj),na.rm=TRUE)))
  {return (data.frame())}
   
  if(mean(values(raster_obj), na.rm=TRUE)==0){
    return(data.frame())
  }
    df <- raster::rasterToPoints(raster_obj) %>% as.data.frame()
    if(nrow(df) < 1){
        #return df here as filtering fails later
        return (df)
    }



## Timing
* 7 minutes for BG Quads with filter step
* 4 minutes for BG Quads without filter step
* less than 1 minute as a block (process_tile) with sampling instead of raster::values()


## PLots, Paper, Science, etc.

Eight Mile & Bison Gulch are in south of Alaska, Twelve Mile is farther North.  There are several sites where there is no

NULL: No difference between Human dist and ML dist
ALT: There is a difference in the distributions

### Plots:
  Within a quadrat: what is
  * Bar graph of human vs ML prediction counts
  * Map of Output - Leaflet? base::plot?

## Stats
* Kolmogorov-Smirnov - make test work in R (done)
* ADDN: Should probably be Chi Square - (done)
* Think about monthly working group

Link
Hyperspectral ML jobs in SF; Orbital Sidekick

Variable Importance
Confusion Matrix - from train/test split on speclib, or similar
Bar plots of Human vs ML distributions (DONE)
Big Maps of predictions 

Figure with quadrat image, the ML prediction, and the UAV RGB image.

Tables: KS Test for each Quadrat (DATA MADE)
PFT/Species/etc. Class counts (show class imbalance). 

MAKE ALL THE FIGs!



15:26:20 From  Peter Nelson  to  Everyone:
	https://www.linkedin.com/jobs/view/2919090475/?refId=bdf6e33b-18fb-4262-a3b5-161d7fc7c8c7
15:31:05 From  Peter Nelson  to  Everyone:
	https://docs.google.com/document/d/1-59adFWLILCFU98shE1v192DMQrfw_2DXGmtV68LfJY/edit?usp=sharing



Figures to make: 
    Variable Importance - Peter has code for this I think
    Confusion Matrix - from train/test split on speclib, or similar (existing code?)
    Bar plots of Human vs ML distributions (ggplot function, match key to maps)
    Big Maps - get this working
    Figure with quadrat image, the ML prediction, and the UAV RGB image. -see example from slideshow

Confusion matrix for training and test set (Speclib data from 80/20 split)

Table of KS Statistics by Quadrat (Bold ones that are good for us, hopefully all)

## PAPER TO DOS
* ML Model with oversampling
* Build prediction maps, 
* validation quadrat maps,
* bar graphs for each quadrat (human vs ML)
* run predictions at all levels, convert to each higher level
* Compile chi-squared test results for base and oversampled model
* confusion matrix and training statistics (ROC?)

### Graphics
* white background
* Title: will be removed anyway
* Legend:
  * set the legend to "Validation" and "Prediction"
* Axis Labels:
  * 