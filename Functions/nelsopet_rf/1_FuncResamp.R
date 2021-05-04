Func_Resamp<-function(Resamp){
  
  # Removes metadata before function can be applied
  df<-metaRemove(Resamp)
  
  # Converts the dataframe to a spectral object
  SpeclibObj<-spectrolab::as.spectra(df)
  
  print("Resampling spectra every 5nm")
  
  # Creates functions that will do the resampling every 5nm
  final<-spectrolab::resample(SpeclibObj,seq(397.593,899.424,5))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  
  # Rename columns
  colnames(final)<-paste(colnames(final),"5nm",sep = "_")
  
  # Combines all the dataframes created into one df
  ResampledDF<-cbind(bandsRemove(Resamp),final)
  
  print("Resampling sucessful")
  
  return(ResampledDF)} # Func_Resamp ends