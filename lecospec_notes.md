# Lecospec Notes

This document is a list of useful things that we have found in the development of Lecospec.  

## Solving Spatial Issues

### Projection Woes
There is a correct way to make sure that the spatial extents of objects match.  Using manually types / copy pasted wkt strings can be problematic due to truncation and rounding.  It's better to use an EPSG code and the following:

`target_wkt <- sf::st_crs(epsg)[[2]]`<br>
`target_crs <- sp::CRS(target_wkt)`

Then the raster can be projected as

`projected_ras <- raster::projectRaster(`<br>
`raster_obj,`<br>
`crs = target_crs)`

use `method = "bilinear` for numerical variables and `method = "ngb"` for categorical variables.

### Memory
A peculiar thing about R is the way garbage collection works: there appears to be a lot of carry-overs from one iteration of a `for` loop to another (which I find suprising; perhaps a later version will address this: currently on 4.1.0).  This problem can be manually addressed by calling `gc()` at the end of a for loop iteration.  This has enable long, nested `for` loops to run without eventually exhausting the system memory.  

This likely has to do with the way R handles second order (and higher) data types; there is always a copy operation involved, even when computing in place.  Since Raster and other packages automatically cache to disk as well, there may be some memory leak also.  It's not clear the exact cause, but tests for image segmentation show that a `gc()` call at the end of the for loop keep the system memory in check.  The exact reason remains unclear.

# Segmentation Pipeline
1. Get base image and shapefile. Load them
2. project to same EPSG code
3. Run ITC Segment (`itcSegment::itcIMG()`)
4. Filter list of centroids to manual ones (`filter_segmentation` function) 
5. Crop the lecospec PredLayer to each individual shapefile (`crop_raster_to_shapes`)
6. Calculate the accuracy via `raster_uniform_acc()` for each raster in the list (e.g. via `lapply()`).