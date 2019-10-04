###Percent Error For models

#############################################################Smooth PCA#######################################################
error_rate_PCA_smooth_bryo<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_smooth_bryo.csv",check.names = F)
error_rate_PCA_smooth_lichen<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_smooth_lichen.csv",check.names = F)
error_rate_PCA_smooth_plants<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_smooth_plants.csv",check.names = F)
error_rate_PCA_smooth_vascular<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_smooth_vascular.csv",check.names = F)

###Combines all error rates for smooth pca
smooth_PCA_error_rates<-rbind(error_rate_PCA_smooth_bryo,
                              error_rate_PCA_smooth_lichen,
                              error_rate_PCA_smooth_plants,
                              error_rate_PCA_smooth_vascular)

##############################################################Raw PCA########################################################
error_rate_PCA_bryo<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_bryo.csv",check.names = F)
error_rate_PCA_lichen<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_lichen.csv",check.names = F)
error_rate_PCA_plants<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_plants.csv",check.names = F)
error_rate_PCA_vascular<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_vascular.csv",check.names = F)

###Combines all error rates for raw pca
PCA_error_rates<-rbind(error_rate_PCA_bryo,
                              error_rate_PCA_lichen,
                              error_rate_PCA_plants,
                              error_rate_PCA_vascular)

###Combines all error rates for both smooth and raw spectra for PCA values
PCA_error_rates<-merge(PCA_error_rates,smooth_PCA_error_rates,by="category")


#############################################################Smooth regular########################################################
error_rate_smooth_bryo<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_smooth_bryo.csv",check.names = F)
error_rate_smooth_lichen<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_smooth_lichen.csv",check.names = F)
error_rate_smooth_plants<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_smooth_plants.csv",check.names = F)
error_rate_smooth_vascular<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_smooth_vascular.csv",check.names = F)

###Combines all error rates for smooth (Regular values)
smooth_reg_error_rate<-rbind(error_rate_smooth_bryo,
                             error_rate_smooth_lichen,
                             error_rate_smooth_plants,
                             error_rate_smooth_vascular)

##############################################################Raw regular########################################################
error_rate_bryo<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_bryo.csv",check.names = F)
error_rate_lichen<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_lichen.csv",check.names = F)
error_rate_plants<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_plants.csv",check.names = F)
error_rate_vascular<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_vascular.csv",check.names = F)

###Combines all error rates for raw(Regular values)
reg_error_rate<-rbind(error_rate_bryo,
                      error_rate_lichen,
                      error_rate_plants,
                      error_rate_vascular)

###Combines all error rates for both smooth and raw spectra for regular values
reg_error_rate<-merge(reg_error_rate,smooth_reg_error_rate,by="category")

##Combines all error rates for courser levels PCA values
error_rate_PCA_smooth_gen_life_form<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_smooth_gen_life_form.csv",check.names = F)
error_rate_PCA_gen_life_form<-read.csv("Model Scripts/Error Rates/PCA/Error/error_rate_PCA_gen_life_form.csv",check.names = F)
gen_life_PCA_error_rate<-merge(error_rate_PCA_gen_life_form,error_rate_PCA_smooth_gen_life_form,by="category")

##Combines all error rates for courser levels Regular values
error_rate_smooth_gen_life_form<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_smooth_gen_life_form.csv",check.names = F)
error_rate_gen_life_form<-read.csv("Model Scripts/Error Rates/Regular/Error/error_rate_gen_life_form.csv",check.names = F)
gen_life_reg_error_rate<-merge(error_rate_gen_life_form,error_rate_smooth_gen_life_form,by="category")

write_csv(reg_error_rate, "Model Scripts/Error Rates/PCA/Error/all_reg_error_rate.csv")
write_csv(PCA_error_rates, "Model Scripts/Error Rates/PCA/Error/all_PCA_error_rate.csv")
