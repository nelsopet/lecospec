library(caret)
library(ranger)
library(randomForest)
library(tidyverse)

#------------------Building Model without identifying important varibles --------------
# Spectral Library
# Output/C_001_SC3_Cleaned_SpectralLib.csv
#SpecLib_derivs<-read.csv("Output/C_001_SC3_Cleaned_SpectralLib.csv")
base_data_path <- "Output/D_002_SpecLib_Derivs.csv"
augmented_data_path <- "./mle/trainingData_noNoise.csv"
SpecLib_derivs<-read.csv(augmented_data_path)
#SpecLib_derivs<-read.csv("Output/resampled/D_002_SpecLib_Derivs.csv")
#SpecLib_derivs<-read.csv("Output/resampled/FncGrp2/D_002_SpecLib_Derivs.csv")
summary(SpecLib_derivs)

#BadAlder<- SpecLib_derivs %>% colnames()
#  dplyr::filter(Functional_group1 == "Shrub_Alder" & `1890`<0.15) %>% dim()

metadata_columns_dropped <- c(
  "ScanID", "Area", "Code_name", "Species_name", "Functional_group1",
  "Functional_group2", "Species_name_Freq", "Functional_group1_Freq",
  "Functional_group2_Freq", "Genus", "Version", "File.Name.1", "Instrument",
  "Detectors", "Measurement", "Date", "Time", "Battery.Voltage.1", "Averages",
  "Integration1", "Integration2", "Integration3", "Dark.Mode.1", "Foreoptic",
  "Radiometric.Calibration.1", "Units", "Latitude", "Longitude", "Altitude",
  "GPS.Time.1", "Satellites", "Calibrated.Reference.Correction.File.1",
  "Channels", "File.Name", "Battery.Voltage", "Dark.Mode",
  "Radiometric.Calibration", "GPS.Time",
  "Calibrated.Reference.Correction.File"
)

print(colnames(SpecLib_derivs)[1:40])

#Reorder columns, delete unneeded for species, FNC grp 1 and 2
SpecLib_derivs_species <-
  SpecLib_derivs %>%
  dplyr::select(Species_name, everything()) %>% #colnames()
  dplyr::select(-ScanID:-Calibrated.Reference.Correction.File) %>% #colnames()
  dplyr::rename(Classes = Species_name) %>%
  mutate(Classes = as.factor(Classes)) %>%
  as.data.frame()

SpecLib_derivs_Fnc1<-
  SpecLib_derivs %>%
  dplyr::select(Functional_group1, everything()) %>% #colnames()
  dplyr::select(-ScanID:-Calibrated.Reference.Correction.File) %>% #colnames()
  dplyr::rename(Classes = Functional_group1) %>%
  mutate(Classes = as.factor(Classes)) %>%
  as.data.frame()


SpecLib_derivs_Fnc2<-
  SpecLib_derivs %>%
  dplyr::select(Functional_group2, everything()) %>% #colnames()
  dplyr::select(-ScanID:-Calibrated.Reference.Correction.File) %>% #colnames()
  dplyr::rename(Classes = Functional_group2) %>%
  mutate(Classes = as.factor(Classes)) %>%
  as.data.frame()

#Set seed for stable output
set.seed(123)

# Build Models
rf_mod_ranger_species_pred<-ranger::ranger(
  Classes ~ .,
  data = SpecLib_derivs_species,
  num.trees = 128)#,local.importance = "impurity_corrected" ) 
  # OOB prediction error:             25.93 %

rf_mod_ranger_FncGrp1_pred<-ranger::ranger(
  Classes ~ .,
  data = SpecLib_derivs_Fnc1,
  num.trees = 128)#,local.importance = "impurity_corrected" ) 
  # OOB prediction error:             25.93 %


rf_mod_ranger_FncGrp2_pred<-ranger::ranger(
  Classes ~ .,
  data = SpecLib_derivs_Fnc2,
  num.trees = 128)#,local.importance = "impurity_corrected" ) 
  # OOB prediction error:             25.93 %

#rf_mod_ranger_species_pred
#rf_mod_ranger_FncGrp1_pred

write.csv(
  rf_mod_ranger_FncGrp1_pred$confusion.matrix,
  "./Output/E_003_Ranger_FncGrp1_Confusion_128trees.csv"
  )

#rf_mod_randomforest
 # Build models using 0.99 percent cutoff for corelated varibles




##Model refinement: Removing intercorrelated predictors. Revisit this .
# Creates corelation matrix by which to filter predictors
# 
#CorelationMatrix<-cor(SpecLib_derivs[-1])
# 
# # Select most correlated varibles 
# caret_findCorr<-findCorrelation(CorelationMatrix, cutoff = 0.99, names = T)
# 
# # Remove corelated vars
# predictor_df_reduced<-SpecLib_derivs %>%
#   dplyr::select(-caret_findCorr)
# 
# # Rebuild models after intercorelated vars are removed
## rf_mod_randomforest<-randomForest(Classes ~ .,data = predictor_df_reduced
##                                   ,ntree=1000,importance=TRUE) # OOB prediction error 23.28%
## # Saves confusion matrix rf
## RandomForest_confusionmatrix<-rf_mod_randomforest$confusion%>%as.data.frame()
## write.csv(RandomForest_confusionmatrix,"Output/E_001_RandomForest_confusionmatrix.csv")
## 
## # Saves confuison Matrix Ranger
#rf_mod_ranger_reduced<-ranger(Classes ~ .,data = predictor_df_reduced,
#                      num.trees = 1000,
#                      local.importance = TRUE) # OOB prediction error:             23.76 %
##
## rf_mod_ranger_IMP<-ranger(Classes ~ .,data = predictor_df_reduced,
##                       num.trees = 1000,
##                       importance = "impurity_corrected",
##                       local.importance = TRUE) # OOB prediction error:             23.76 %  
## 
# Ranger_confusionmatrix<-rf_mod_ranger_reduce$confusion.matrix%>%as.data.frame.matrix()
# write.csv(Ranger_confusionmatrix,"Output/E_002_Ranger_confusionmatrix.csv")


# saves the model with the lowest error
save(rf_mod_ranger_species_pred, file = "Output/E_003_Pred_Model_RandomForest_species_128trees.rda")
save(rf_mod_ranger_FncGrp1_pred, file = "Output/E_003_Pred_Model_RandomForest_FncGrp1_128trees.rda")
#save(rf_mod_ranger_FncGrp2_pred, file = "Output/E_003_Pred_Model_RandomForest_FncGrp2_resamp29_1000trees.rda")
save(rf_mod_ranger_FncGrp2_pred, file = "Output/E_003_Pred_Model_RandomForest_FncGrp2_128trees.rda")

#------------------------------ Select Important varibles -----------------------------------
# Creates a dataframe with all varibles and their imoportance
# ImportantVarsFrame<-enframe(rf_mod_ranger_reduced$variable.importance, 
#                             name="predictor", value="importance")
#   
# # Function Creates a plot of the 30 most important vars
# ImportantVarsFrame25<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:25,]
# 
# # Lets R respect the order in data.frame.
# ImportantVarsFrame25$predictor <- factor(ImportantVarsFrame25$predictor,
#                                        levels = ImportantVarsFrame25$predictor
#                                        [order(ImportantVarsFrame25$importance)])
# 
# # Creates a plot of the 30 most important varibles
# ImportantVarsFrame25%>%
#   ggplot(aes(x  = predictor, y = importance))+
#   theme_bw()+
#   geom_bar(stat = "identity")+
#   coord_flip()+
#   ggtitle("25 Most Important Varibles (Class_3)")
# 
# ggsave("Output/E_009_Class_3_VarImp.jpg")
 


