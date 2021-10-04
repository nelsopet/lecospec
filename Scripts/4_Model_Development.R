library(caret)
library(ranger)
library(randomForest)
library(tidyverse)

#------------------Building Model without identifying important varibles --------------
# Spectral Library
SpecLib_derivs<-read.csv("Output/D_002_SpecLib_Derivs.csv")

# Remove Unwanted columns
# Creates a string of possible names that will be removed
#remove_names<-c("ScanID","Class1","Class2","Class4","Area","Class2_Freq"
#                ,"Class3_Freq","Class4_Freq","Tree_numbe","x","y","Species_name",
#                "Functional_group1","Functional_group2","Species_name_Freq"  ,
#                "Functional_group1_Freq","Functional_group2_Freq")
# Remove Unwanted columns
#SpecLib_derivs[remove_names] = NULL

# Change column name with all the levels to "classes"
#names(SpecLib_derivs)[1]<-"Classes"

# Converts column to a fctor
#SpecLib_derivs$Classes<-SpecLib_derivs$Classes%>%as.factor()
SpecLib_derivs<-
SpecLib_derivs %>%   
  dplyr::select(Functional_group1, everything()) %>% #colnames()
  dplyr::select(-ScanID:-Functional_group2_Freq) %>% #colnames()
  rename(Classes = Functional_group1) %>%
  mutate(Classes = as.factor(Classes)) %>% as.data.frame()



set.seed(123)
# Build Model
rf_mod_ranger<-ranger::ranger(Classes ~ .,data = SpecLib_derivs, num.trees = 1000,local.importance = "impurity_corrected" ) # OOB prediction error:             25.93 %

rf_mod_ranger

#rf_mod_randomforest<-randomForest(Classes ~ .,data = SpecLib_derivs,ntree=1000,importance=TRUE) # OOB prediction error 26.18%

#Make models with all predictors for prediction
rf_mod_ranger_pred<- ranger::ranger(Classes ~ .,data = SpecLib_derivs, num.trees = 1000) # OOB prediction error:10.42 %   

rf_mod_ranger_pred

#rf_mod_randomforest_pred<-randomForest(Classes ~ .,data = SpecLib_derivs, ntree=1000) # OOB prediction error 10.42 % 

#rf_mod_randomforest
 # Build models using 0.99 percent cutoff for corelated varibles
 # Creates corelation matrix
 
CorelationMatrix<-cor(SpecLib_derivs[-1])
 
 # Select most correlated varibles 
 caret_findCorr<-findCorrelation(CorelationMatrix, cutoff = 0.99, names = T)
 
 # Remove corelated vars
 predictor_df_reduced<-SpecLib_derivs %>%
   dplyr::select(-caret_findCorr)
 
 # Rebuild models after intercorelated vars are removed
# rf_mod_randomforest<-randomForest(Classes ~ .,data = predictor_df_reduced
#                                   ,ntree=1000,importance=TRUE) # OOB prediction error 23.28%
# # Saves confusion matrix rf
# RandomForest_confusionmatrix<-rf_mod_randomforest$confusion%>%as.data.frame()
# write.csv(RandomForest_confusionmatrix,"Output/E_001_RandomForest_confusionmatrix.csv")
# 
# # Saves confuison Matrix Ranger
rf_mod_ranger_reduced<-ranger(Classes ~ .,data = predictor_df_reduced,
                      num.trees = 1000,
                      local.importance = TRUE) # OOB prediction error:             23.76 %
#
# rf_mod_ranger_IMP<-ranger(Classes ~ .,data = predictor_df_reduced,
#                       num.trees = 1000,
#                       importance = "impurity_corrected",
#                       local.importance = TRUE) # OOB prediction error:             23.76 %  
# 
 Ranger_confusionmatrix<-rf_mod_ranger_reduce$confusion.matrix%>%as.data.frame.matrix()
 write.csv(Ranger_confusionmatrix,"Output/E_002_Ranger_confusionmatrix.csv")


# saves the model with the lowest error
#save(rf_mod_randomforest, file = "Output/E_003_Best_Model_RandomForest.rda")

# saves the model with the lowest error
save(rf_mod_ranger      , file = "Output/E_004_Best_Model_Ranger.rda")
save(rf_mod_ranger_pred      , file = "Output/E_004_Best_Model_Ranger_pred.rda")
save(rf_mod_ranger_reduced      , file = "Output/E_004_Best_Model_Ranger_reduced.rda")

#------------------------------ Select Important varibles -----------------------------------
# Creates a dataframe with all varibles and their imoportance
 ImportantVarsFrame<-enframe(rf_mod_ranger_reduced$variable.importance, 
                             name="predictor", value="importance")
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
 









