library(parallel)





df<-("Original_data/Sensors/Tiles/EightMile_TSTIMG2_envi_R1C1.dat")



system.time(test_2<-HyperSpecImages(df))
system.time(test_3<-newby(llb))
system.time(test_1<-ImagePredictor_generator(df))

llb<-metaRemove(test_2)

bad<-lapply(test_3,add_suffix)

#newby<-function(x){
#  aaa<-as.spectra(x)
#  
#  bandpass<-list(
#    wl_5nm  <-spectrolab::resample(aaa,seq(397.593,899.424,5))
#    ,wl_10nm <-spectrolab::resample(aaa,seq(397.593,899.424,10))
#    ,wl_50nm <-spectrolab::resample(aaa,seq(397.593,899.424,50))
#    ,wl_100nm<-spectrolab::resample(aaa,seq(397.593,899.424,100))
#  )
#  final<-lapply(bandpass,function(x){
#    as.data.frame(x)%>%
#      dplyr::select(-sample_name)})
#  names(final)=c("wl_5nm","wl_10nm","wl_50nm","wl_100nm")
#  
#  lapply(final,add_suffix)}
#
#return(do.call(cbind(bandsRemove(Resamp),final)))
#
#}
#
#}
#
#df<-newby(DF)#