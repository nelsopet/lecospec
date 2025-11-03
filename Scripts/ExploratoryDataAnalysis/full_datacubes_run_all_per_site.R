#source("./Functions/lecospectR.R")
source("./Functions/lecospectR_debug.R")


#Read in directories of images to predict PFT cover
dirs_2018<-read.csv("./Output/Dirs/2018_AK_directories_keep.csv") %>% dplyr::select(x)
dirs_2019<-read.csv("./Output/Dirs/2019_AK_directories_keep.csv")%>% dplyr::select(x)

dirs_2018_l<-dirs_2018$x
dirs_2019_l<-dirs_2019$x

#Make lists of images per directory


dir_out_2018_all<-lapply(1:length(dirs_2018_l), function(x){
dir <- dirs_2018_l[x]
dir_files<-list.files(dirs_2018_l[x])
dir_file_names<-dir_files[grep("_rd_rf_or$",dir_files)]
dir_file_paths<-rep(dir,length(dir_file_names))
dir_info_out<-cbind(dir_file_paths,dir_file_names)
})
dir_out_2018_all_df<-Reduce(rbind,dir_out_2018_all)

dir_out_2019_all<-lapply(1:length(dirs_2019_l), function(x){
dir <- dirs_2019_l[x]
dir_files<-list.files(dirs_2019_l[x])
dir_file_names<-dir_files[grep("_rd_rf_or$",dir_files)]
dir_file_paths<-rep(dir,length(dir_file_names))
dir_info_out<-cbind(dir_file_paths,dir_file_names)
})
dir_out_2019_all_df<-Reduce(rbind,dir_out_2019_all)

dir_out_all<-rbind(dir_out_2018_all_df,dir_out_2019_all_df)
#write.csv(dir_out_all,"Output/Dirs/datacubes_keep_all.csv")

img_num = 29
test_path = paste(dir_out_all[img_num,1],dir_out_all[img_num,2],sep="/")
test_path
#Get file size
file_size<-file.info(test_path)$size
file_size/1E9
tile_size = function(x) {solve(file_size/x <= 25000000)}
#Divide file into tiles <250 Mb
tile_size(file_size)

windows()
raster::raster(test_path) %>% plot()

print(date())
quad_results <- estimate_land_cover(
  test_path, 
  output_filepath = "./test/test_pred_adaboost_all_datacube_run_100152_9315.grd",
  use_external_bands = TRUE)
closeAllConnections()
print(date())

as_tiff<-function(path) {raster::raster(paste0("./test/",path,".grd")) %>% raster::writeRaster(paste0("./test/",path,".tif"), overwrite=TRUE)}

test_tif<-as_tiff("test_pred_adaboost_all_datacube_run_100152_9315")
plot(test_tif)

#Remove and  recreate tiles directory for next run
unlink("./tiles", recursive = TRUE)
dir.create("./tiles")
