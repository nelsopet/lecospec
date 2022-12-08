# Testing

## Variables

* normalization - 3 levels
* Noise / no Noise
* weights - none, posterior (validation), prior (frequency)
* scaling (min-max scale or not)
* ground or UAV data 

MRPP and PermuNOVA per PFT or across PFTs
Do PFTs look the same in different SpecLibs?


## To Dos moving forward
* adjust posterior weights to remove bias based on the number of scans
* create tests for the data munging
    * Visualizes Spectra by PFT
    * Min, Max, median, mean, variance for the bands / indices
    * 
* Model tests
    * Bar graph of outputs per PFT
* Create single model training and validation script.
* plot medians of the PFTs for each spectrum
* trim spectra to above 420nm (up to 1k)