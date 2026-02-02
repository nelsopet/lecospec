source("Functions/lecospectR.R")
dir = "./test"
preds_out<-list.files(dir)[grep(".grd$",list.files(dir))]
preds_out<-preds_out[grep("test_pred_adaboost_all_datacube_run_",preds_out)]

files<-gsub(".grd","",preds_out)

df<-t(as.data.frame(strsplit(gsub(".grd","",preds_out),"_"))) #%>% head
rownames(df)<-NULL
head(df[,7:8])

as_tiff<-function(x) {terra::rast(paste0(dir,"/",files[x],".grd")) %>% terra::writeRaster(paste0("./Output/PFT_predictions_final/","UAS_PFT_map_flight_",df[x,7],"_image_",df[x,8],".tif"), overwrite=TRUE)}

#x=3
#test_tif<-as_tiff(x)
#plot(test_tif)

#Write all PFT maps to disk as .tif with new naming convention
lapply(1:length(files), function(x) {terra::rast(paste0(dir,"/",files[x],".grd")) %>% terra::writeRaster(paste0("./Output/PFT_predictions_final/","UAS_PFT_map_flight_",df[x,7],"_image_",df[x,8],".tif"), overwrite=TRUE)})

#Remove and  recreate tiles directory for next run
unlink("./tiles", recursive = TRUE)
dir.create("./tiles")