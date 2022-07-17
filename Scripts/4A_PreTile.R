
big_test <- "E:/ORNL_DAAC_DATA_ARCHIVE/BisonGulch/BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or"
big_test_brick<-brick(big_test)
big_test_rast<-raster(big_test)
SpaDES.tools::splitRaster(
  big_test_rast, 
  nx=3,
  ny = 3, 
  path = "./tiles/gig")

gig_files<-list.files("./tiles/gig/")

gig_files_use<- gig_files %>% 
  as.data.frame() %>% 
  dplyr::filter(grepl(".grd",.)) %>% 
  rename(file_name=".") %>%
  mutate(path = paste("./tiles/gig/",file_name, sep=""))

lapply(3:length(gig_files_use$file_name), function(x) {

assign(paste(gsub(".grd","",gig_files_use[x,1])),terra::crop(brick(big_test), raster(gig_files_use[x,2])))

writeRaster(eval(parse(text=gsub(".grd","",gig_files_use[x,1]))),paste("./tiles/gig/full/",gsub(".grd","",gig_files_use[x,1]),".grd",sep=""))
})


