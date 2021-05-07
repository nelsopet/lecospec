#Load all the dependent precursor info/functions
source("Functions/nelsopet_rf/1_Bandpass_Def.R"      )
source("Functions/nelsopet_rf/1_FuncResamp.R"        )
#source("Functions/nelsopet_rf/1_LandCoverEstimator.R")
source("Functions/nelsopet_rf/1_MetaBand_Remove.R"   )
source("Functions/nelsopet_rf/1_VI_CALC.R")

# Function Combines both Derivitive that are calculated
Deriv_combine<- function(x){
  
  # Resampling Dataset
  Resampled_data<-Func_Resamp(x)
  
  # Calculating VIs for Dataset
  VegIndex_data<-Func_VI(x)
  
  DF<-cbind(VegIndex_data,metaRemove(Resampled_data))
  
  return(DF)
} # Deriv_combine ends
