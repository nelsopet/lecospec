Make_Speclib_Derivs<- function(filename)  {  
  # Reads in spectral libray as .csv
  # Right now your spectral library would have already have weird values removed/replaced
  Spectral_lib<-read.csv(filename, check.names = F)
  
  Spectral_lib<-Deriv_combine(Spectral_lib)
  
  write.csv(Spectral_lib,paste("Output/D_002_SpecLib_Derivs",".csv", sep=""),row.names = F)
  
  # Normalize Values here
  return(Spectral_lib)
}