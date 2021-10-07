require(raster)

TileAssembler<-function(dir,out){
tiles = list.files(dir)
tiles = grep("Pred",tiles, value = TRUE)
chunks<-lapply(1:length(tiles),function(x) {raster(paste(dir,"/",tiles[x], sep=""))})
pred_merged<-Reduce(merge,chunks)
#writeRaster(pred_merged, filename = "Output/Predictions/")
return(pred_merged)
}
