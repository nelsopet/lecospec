library(tidyverse)

##############################################################MOD_Errors#####################################################
#Output
Raw_errors   <-"Processed_spec/Error_rates/Raw"
Smooth_errors<-"Processed_spec/Error_rates/Smooth"

##Creates a list of all filenames for the different sets of error rates for models
MOD_error_raw   <-list.files(path = Raw_errors   , pattern = "ModelOOB"      , full.names = T)
MOD_error_Smooth<-list.files(path = Smooth_errors, pattern = "ModelOOB"      , full.names = T)

##Reads in all dataframes for MOD erros as a list
MOD_error_raw   <-lapply(MOD_error_raw   , read.csv)
MOD_error_Smooth<-lapply(MOD_error_Smooth, read.csv)

##Combines the lists of mod errors
MOD_errors<-append(MOD_error_raw, MOD_error_Smooth)

##
MOD_error<-do.call(cbind, l)
cbind(obj.list)
do.call(cbind, list(A,B,C))

Spec_raw_more05<-list.files(path = Raw_errors   , pattern = "Species"      , full.names = T)
Spec_raw_more10<-list.files(path = Raw_errors   , pattern = "Species"      , full.names = T)
Spec_raw_more15<-list.files(path = Raw_errors   , pattern = "Species"      , full.names = T)
Spec_raw_more20<-list.files(path = Raw_errors   , pattern = "Species"      , full.names = T)

Spec_smooth_more05<-list.files(path = Smooth_errors   , pattern = "Species"      , full.names = T)
Spec_smooth_more10<-list.files(path = Smooth_errors   , pattern = "Species"      , full.names = T)
Spec_smooth_more15<-list.files(path = Smooth_errors   , pattern = "Species"      , full.names = T)
Spec_smooth_more20<-list.files(path = Smooth_errors   , pattern = "Species"      , full.names = T)


Spec_raw_more05_error   <-lapply(Spec_raw_more05,read.csv)
Spec_raw_more10_error   <-lapply(Spec_raw_more10,read.csv)
Spec_raw_more15_error   <-lapply(Spec_raw_more15,read.csv)
Spec_raw_more20_error   <-lapply(Spec_raw_more20,read.csv)

Spec_smooth_more05_error<-lapply(Spec_smooth_more05,read.csv)
Spec_smooth_more10_error<-lapply(Spec_smooth_more05,read.csv)
Spec_smooth_more15_error<-lapply(Spec_smooth_more05,read.csv)
Spec_smooth_more20_error<-lapply(Spec_smooth_more05,read.csv)
  
  
  
  