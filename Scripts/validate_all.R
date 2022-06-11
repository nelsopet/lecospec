source("./Functions/lecospectR.R")
require(sf)

validation_df <- read.csv(validation_data_path_2, na.strings=c("NA", "n/a"))
