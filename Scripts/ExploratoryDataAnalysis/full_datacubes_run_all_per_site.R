source("./Functions/lecospectR.R")


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

test_path = paste(dir_out_all[1,1],dir_out_all[1,2],sep="/")

print(date())
quad_results <- estimate_land_cover(
  test_path, 
  output_filepath = "./test/test_pred_adaboost.grd",
  use_external_bands = TRUE,
  overwrite = TRUE)
closeAllConnections()
print(date())