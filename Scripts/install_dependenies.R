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
    "leaflet",
    "leaflet.opacity",
    "leaflegend",
    "naniar",
    "rasterVis"
)

install.packages(lecospec_depenencies, repos = "http://cran.rstudio.com/")
update.packages(repos = "http://cran.rstudio.com/", ask=FALSE)

webshot::install_phantomjs()