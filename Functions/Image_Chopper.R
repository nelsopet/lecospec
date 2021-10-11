tst_path = "F:/BisonGulch/raw_1511_rd_rf_or"
tst_seg_path = "F:/BisonGulch/Bisoon_Quadrats.shp"

ImgChopper<-
function(img,quad) {
tst_img<-raster(img)
tst_quads<-readOGR(dsn=quad)
tst_crop<-raster::crop(tst_img, tst_quads)
tst_mask<-raster::mask(tst_img, tst_quads)
tst_out<-c(tst_crop,tst_mask)
return(tst_out)
}

tst_out<-ImgChopper(tst_path, tst_seg_path)

plot(tst_out[[2]])
writeRaster(tst_out[[2]], "./Output/BisonGulchQuads")
