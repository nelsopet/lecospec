lecospec_depenencies <- c(
    "rgdal",
    "gdalUtils",
    "rgeos",
    "sf",
    "sp",
    "hsdar",
    "mapview", 
    "maptools",
    "doParallel",
    "randomForest",
    "ranger",
    "missForest",
    "caret",
    "e1071",
    "raster",
    "spectrolab",
    "tidyverse",
    "useful",
    "SpaDES", 
    "SpaDES.tools",
    "Polychrome",
    "gplots",
    "rasterVis",
    "RColorBrewer",
    # should remove the below dependencies:
    #"leaflet",
    #"leaflet.opacity",
    #"leaflegend",
    "naniar",
    "rasterVis",
    "doSNOW",
    "snow",
    "gdalUtils",
    "rjson",
    "doParallel",
    "arrow",
    "RhpcBLASctl",
    "plotly",
    "readr",
    "terra",
    "pivottabler",
    "webshot",
    "xgboost",
    "permute",
    "pls",
    "landscapemetrics",
    "landscapetools",
    "pls", # added this one, will need to add PLS packages as well
    "stars", #Plotting maps with layers from the web
    "maptiles"  #Plotting maps with layers from the web
)

install.packages(lecospec_depenencies, repos = "http://cran.rstudio.com/")
update.packages(ask=FALSE)

#webshot::install_phantomjs()

# NOTE: on debian linux, it is recommended to use
# r2u to aid in the installation of the above packages.
#
# Time-out issues are also possible when installing terra (it's big!)