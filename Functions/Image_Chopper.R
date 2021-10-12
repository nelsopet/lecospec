
ImgChopper<-
function(img,quad) {
tst_img<-raster(img)
tst_quads<-readOGR(dsn=quad)
tst_crop<-raster::crop(tst_img, tst_quads)
tst_mask<-raster::mask(tst_img, tst_quads)
tst_out<-c(tst_crop,tst_mask)
return(tst_out)
}


