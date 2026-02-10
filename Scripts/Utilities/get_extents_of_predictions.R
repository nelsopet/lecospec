source("Functions/lecospectR.R")
dir = "./Output/PFT_predictions_final/"
preds_out<-list.files(dir)[grep(".tif$",list.files(dir))]
#preds_out<-preds_out[grep(".tif",preds_out)]

files<-gsub(".tif","",preds_out)

df<-t(as.data.frame(strsplit(gsub(".tif","",preds_out),"_"))) #%>% head
rownames(df)<-NULL
head(df[,7:8])

#Write all PFT maps to disk as .tif with new naming convention
lapply(1:length(files), function(x) {
img<-terra::rast(paste0(dir,"/",preds_out[x]))
img_extent<-sf::st_bbox(img)
img_poly<-sf::st_as_sfc(img_extent)
sf::write_sf(img_poly,paste0("./Output/PFT_predictions_final/Extents/",files[x],".kml"))
    })
