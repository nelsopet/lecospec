####################################################
#       Global Defs
####################################################
source("./Functions/lecospectR.R")


####################################################
#       Bison Gulch 
####################################################

plot_options <- define_plot_options(
    title = "Bison Gulch Predictions",
    xLabel = "Longitude",
    yLabel = "Latitude"
)

#bg_filepath <- "E:/Predictions/bg_01_07_1511_fncgrp1_PREDICTIONS.tif"
bg_filepath <- "Output/dev_FullCube/bg_1511_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif"

bg_predictions <- raster::raster(bg_filepath)

bg_map <- plot_categorical_raster(bg_predictions, plot_options)

windows();bg_map

ggsave("./bg_map.png")


####################################################
#       Eight Mile
####################################################
plot_options <- define_plot_options(
    title = "Eight Mile Predictions",
    xLabel = "Longitude",
    yLabel = "Latitude"
)

em_filepath <- "./Output/dev_FullCube/em_5968_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif"
em_predictions <- raster::raster(em_filepath)

em_map <- plot_categorical_raster(em_predictions, plot_options)

windows();em_map

ggsave("./em_map.png")

####################################################
#       Twelve Mile
####################################################

# Part A
plot_options <- define_plot_options(
    title = "Twelve Mile Predictions",
    xLabel = "Longitude",
    yLabel = "Latitude"
)

tm_filepath <- "./Output/dev_FullCube/tm_0_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif"

tm_predictions <- raster::raster(tm_filepath)

tm_map <- plot_categorical_raster(tm_predictions, plot_options)

windows();tm_map

ggsave("./tm_map.png")

# Part B

tm_filepath_b <- "E:/Predictions/tm_21_28_2000_fngrp1_PREDICTIONS.tif"
tm_predictions_b <- raster::raster(tm_filepath_b)

tm_map_b <- plot_categorical_raster(tm_predictions_b, plot_options)

windows();tm_map_b

ggsave("./tm_map_pt2.png")

####################################################
#       Chatanika
####################################################
plot_options <- define_plot_options(
    title = "Chatanika Predictions",
    xLabel = "Longitude",
    yLabel = "Latitude"
)

ca_filepath <- "./Output/dev_FullCube/ch_0_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif"
ca_predictions <- raster::raster(ca_filepath)

ca_map <- plot_categorical_raster(ca_predictions, plot_options)

windows();ca_map

ggsave("./ca_map.png")


####################################################
#       Murphy Dome
####################################################
plot_options <- define_plot_options(
    title = "Murphy Dome Predictions",
    xLabel = "Longitude",
    yLabel = "Latitude"
)

md_filepath_1 <- "E:/Predictions/md_10350_fncgrp1_PREDICTIONS.tif"
md_predictions_1 <- raster::raster(md_filepath_1)

md_map_1 <- plot_categorical_raster(md_predictions_1, plot_options)

windows();md_map_1

ggsave("./md_map_1.png")

md_filepath_2 <- "E:/Predictions/md_24967_fncgrp1_PREDICTIONS.tif"
md_predictions_2 <- raster::raster(md_filepath_2)

md_map_2 <- plot_categorical_raster(md_predictions_2, plot_options)

windows();md_map_2

ggsave("./md_map_2.png")

####################################################
#       Bonanza Dome
####################################################
plot_options <- define_plot_options(
    title = "Bonanza Predictions",
    xLabel = "Longitude",
    yLabel = "Latitude"
)

bz_filepath_1 <- "./Output/dev_FullCube/bz_6425_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif"
bz_predictions_1 <- raster::raster(bz_filepath_1)

bz_map_1 <- plot_categorical_raster(bz_predictions_1, plot_options)

windows();bz_map_1

ggsave("./bz_map_1.png")





