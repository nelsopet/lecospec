Lecospec
================

## Laboratory of Ecological Spectroscopy (lecospec)

The purpose lecospec code is to take ground or image based reflectance
spectra, build a classifier or regression, apply that model to an
imaging spectrometer data cube (aka hyperspectral image).

## How to run lecospec

### NOTE: Your data will need to be a spectral library with all files per site in a directory

### File naming structure etc. maybe different and therefore cleaning of scans below should be

### for demonstration only.

1)  First, install dependencies /Scripts/install\_dependencies.R
2)  Then source the package of lecospectR /Functions/lecospectR.R
3)  Our workflow assumes a list of species with associated levels of
    taxonomic information (eg. functional group membership)
    /Data/SpeciesTable\_20220125.csv
4)  Build a spectral library from a range of field scans collected with
    Tungsten halogen illumination using a leaf clip or contact probe.
    These scripts delete bad scans and standardize the associated
    information into a single metadata format.
    /Scripts/2\_DataMunging.R  
    /Scripts/2B\_DataMunging\_missing\_spectra.R  
    /Scripts/3\_Create\_SpecLibPSR.R

After cleaning scans, the reflectance data can be summarized and
visualized in various ways. Below is accuracy summary of the median and
interquartile ranges of reflectance (75% black & 95% grey) with the
sample size in number of scans distributed across a number of scans
(ground measured) or pixels (airborne).

<img src="./Output/Fnc_grp1_spectral_profiles_PFT_IMG_SPECTRA_ALL.jpg" width="1000" height="1000">

## Study StudyArea

The centers of all UAV flights and points where ground scans were
collected are shown in the map.

    ## OGR data source with driver: KML 
    ## Source: "/Users/peternelson 1/Documents/Schoodic/lecospec_at_schoodic/Git/lecospec/Output/Ground_Spetra_AK_points.kml", layer: "Ground_Spetra_AK_points"
    ## with 739 features
    ## It has 2 fields

    ## OGR data source with driver: KML 
    ## Source: "/Users/peternelson 1/Documents/Schoodic/lecospec_at_schoodic/Git/lecospec/Output/UAV_VNIR_AK_centroids.kml", layer: "UAV_VNIR_AK_centroids"
    ## with 997 features
    ## It has 2 fields

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
<img src="./Output/StudyAreaGround_Airborne_Spectra_Locs.jpg" width="250" height="250">

1)  validation\_def.R sets all the input, output and needed associated
    files for building models and predicting images
2)  Scripts/modelbuilding.ipynb Builds and visualizes model accuracy
3)  Pick and model and explore results with
    lecospectR::validate\_model.R , whicih calls the input data, models
    and settings from validate\_def.R
