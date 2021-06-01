require(raster)

TileAssembler<-function(dir){
tiles = list.files(dir)
chunks<-lapply(1:length(tiles),function(x) {raster(paste(dir,"/",tiles[x], sep=""))})
pred_merged<-Reduce(merge,chunks)
return(pred_merged)
}
