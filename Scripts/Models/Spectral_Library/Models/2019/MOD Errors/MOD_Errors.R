library(tidyverse)

##############################################################MOD_Errors#####################################################
#Output
Raw_errors   <-"Processed_spec/Error_rates/Raw"
Smooth_errors<-"Processed_spec/Error_rates/Smooth"

##Creates a list of all filenames for the different sets of error rates for models
MOD_error_raw   <-list.files(path = Raw_errors   , pattern = "ModelOOB"      , full.names = T)
MOD_error_smooth<-list.files(path = Smooth_errors, pattern = "ModelOOB"      , full.names = T)

##Reads in all dataframes for MOD erros as a list
MOD_error_raw   <-lapply(MOD_error_raw   , read.csv)
MOD_error_smooth<-lapply(MOD_error_smooth, read.csv)

MOD_error_raw   <-do.call(rbind,MOD_error_raw)
MOD_error_smooth<-do.call(rbind,MOD_error_smooth)
  
##Combines the lists of mod errors
MOD_errors<-merge(MOD_error_raw,MOD_error_smooth,by="Category")

###Writes out dataframe as a excel file that shows overall error rate for each rf_MOD

write.csv(MOD_errors,"Processed_spec/Error_rates/MOD_errors.csv",row.names = F)
  
  
  
  