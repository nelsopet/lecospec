library(caret)
library(ranger)
library(randomForest)
library(tidyverse)

#------------------Building Model without identifying important varibles --------------
# Spectral Library
SpecLib_derivs<-read.csv("Output/D_002_SpecLib_Derivs.csv")

# Remove Unwanted columns
# Creates a string of possible names that will be removed
remove_names<-c("ScanID","Class1","Class2","Class4","Area","Class2_Freq"
                ,"Class3_Freq","Class4_Freq","Tree_numbe","x","y")

# Remove Unwanted columns
SpecLib_derivs[remove_names] = NULL

# Change column name with all the levels to "classes"
names(SpecLib_derivs)[1]<-"Classes"

# Converts column to a fctor
SpecLib_derivs$Classes<-SpecLib_derivs$Classes%>%as.factor()

set.seed(123)
# Build Model
rf_mod_ranger<-ranger::ranger(Classes ~ .,data = SpecLib_derivs,
                      num.trees = 1000,
                      local.importance = TRUE) # OOB prediction error:             15.25 % 

rf_mod_randomforest<-randomForest(Classes ~ .,data = SpecLib_derivs
                                  ,ntree=1000,importance=TRUE) # OOB prediction error 15.53%

# Build models using 0.99 percent cutoff for corelated varibles
# Creates corelation matrix
CorelationMatrix<-cor(SpecLib_derivs[-1])

# Select most correlated varibles 
caret_findCorr<-findCorrelation(CorelationMatrix, cutoff = 0.99, names = T)

# Remove corelated vars
predictor_df_reduced<-SpecLib_derivs %>%
  dplyr::select(-caret_findCorr)

# Rebuild models after intercorelated vars are removed
rf_mod_randomforest<-randomForest(Classes ~ .,data = predictor_df_reduced
                                  ,ntree=1000,importance=TRUE) # OOB prediction error 13.66%
# Saves confusion matrix rf
RandomForest_confusionmatrix<-rf_mod_randomforest$confusion%>%as.data.frame()
write.csv(RandomForest_confusionmatrix,"Output/E_001_RandomForest_confusionmatrix.csv")

# Saves confuison Matrix Ranger
rf_mod_ranger<-ranger(Classes ~ .,data = predictor_df_reduced,
                      num.trees = 1000,
                      local.importance = TRUE) # OOB prediction error:             14.03 %  

Ranger_confusionmatrix<-rf_mod_ranger$confusion.matrix%>%as.data.frame.matrix()
write.csv(Ranger_confusionmatrix,"Output/E_002_Ranger_confusionmatrix.csv")


# saves the model with the lowest error
save(rf_mod_randomforest, file = "Output/E_003_Best_Model_RandomForest_86vars.rda")

# saves the model with the lowest error
save(rf_mod_ranger      , file = "Output/E_004_Best_Model_Ranger_86vars.rda")









