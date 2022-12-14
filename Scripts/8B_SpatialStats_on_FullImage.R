require(landscapetools)
require(landscapemetrics)
require(sf)
source("Functions/lecospectR.R")

img_tst_rst<-terra::rast("Output/bg_01_07_1511_fncgrp1_PREDICTIONS.tif")
img_tst_rst_proj<-terra::project(img_tst_rst, "epsg:6393")
unique(values(img_tst_rst_proj))
img_tst_rst_proj_int<- setValues(img_tst_rst_proj, as.integer(values(img_tst_rst_proj)))
check_landscape(img_tst_rst_proj_int)
unique(values(img_tst_rst_proj_int)) #Values are cast to int but NAs are introduced
img_rst_tst_area<-landscapemetrics::lsm_p_area(img_tst_rst_proj_int) #How are NAs handled?

img_rst_tst_area %>% group_by(class) %>% summarize(Min_Area = min(value)*10000,
                                                    Lowest_5pct_Area = quantile(value, probs = 0.05)*10000,
                                                   Median_Patch_Area = quantile(value, probs = 0.5)*10000, 
                                                    Upper_95pct_Area = quantile(value, probs = 0.95)*10000,
                                                    Max_Area = max(value)*10000)

class_dist<-lst_img_rst_tst %>% dplyr::filter(class==10) %>% dplyr::select(value)
class_area<-img_rst_tst_area %>% dplyr::filter(class==10) %>% dplyr::select(value)

hist(log10(class_val$value))
hist(log10(class_area$value))

values(img_tst_rst)

range(img_rst_tst_area$value)
quantile(class_dist$value, probs = c(0.05, 0.5, 0.95))
quantile(class_area$value, probs = c(0.05, 0.5, 0.95))
range(class_area$value)

jpeg("figures/PatchSizebyImage.jpg")
ggplot((img_rst_tst_area %>% filter(class != 0)), aes(x = log10(value*10000), colour=class)) + 
    geom_histogram(aes(colour=as.factor(class))) + 
      facet_wrap(vars(class), scales = "free", ncol = 3) 
dev.off()
