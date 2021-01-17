library(caret)
library(ranger)
library(randomForest)
library(tidyverse)

#------------------Building Model without identifying important varibles --------------
# Spectral Library
SpecLib_derivs<-read.csv("Output/D_002_SC4_SpecLib_Derivs.csv")

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
                              num.trees = 10000,
                              importance = "impurity_corrected",
                              local.importance = TRUE)

rf_mod_randomforest<-randomForest(Classes ~ .,data = SpecLib_derivs
                                  ,ntree=1000,importance=TRUE) # OOB prediction error 22.54%

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
                                  ,ntree=1000,importance=TRUE) # OOB prediction error 20.86%
# Saves confusion matrix rf
RandomForest_confusionmatrix<-rf_mod_randomforest$confusion%>%as.data.frame()
write.csv(RandomForest_confusionmatrix,"Output/E_001_RandomForest_confusionmatrix.csv")

# Saves confuison Matrix Ranger
rf_mod_ranger<-ranger(Classes ~ .,data = predictor_df_reduced,
                      num.trees = 10000,
                      importance = "impurity_corrected",
                      local.importance = TRUE) # OOB prediction error:             24.60 %  

rf_mod_ranger

Ranger_confusionmatrix<-rf_mod_ranger$confusion.matrix%>%as.data.frame.matrix()
write.csv(Ranger_confusionmatrix,"Output/E_002_Ranger_confusionmatrix.csv")

# saves the model with the lowest error
save(rf_mod_randomforest, file = "Output/E_003_Best_Model_RandomForest_86vars.rda")

# saves the model with the lowest error
save(rf_mod_ranger      , file = "Output/E_004_Best_Model_Ranger_86vars.rda")

      # Selects the 50 most important variables
      ImportantVarsFrame<-enframe(rf_mod_ranger$variable.importance, name="predictor", value="importance")
          #ModStat<-enframe(rf_mod_rang$prediction.error, 
          #                           name="predictor", value="error")
          #Modstat$
          # Selects the 50 most important variables
          Imp_Vars50<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:50,]
          
          # Creates a plot of the 25 most important varibles
          #pdf(paste(out_file,"ImpVars",".pdf",sep =""))#, units="px",height = 1400, width=2400, res=350)
          
          # Creates plot
          # ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:25,]%>%
          ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:25,]%>%
            ggplot()+
              # geom_col(aes(x  = predictor, y = importance))+
              geom_col(aes(x  = reorder(predictor, +importance), y = importance))+
              coord_flip()+
              xlab("predictor")+
              theme_bw()
                # Saves the plot 
                ggsave("Output/E_005_Ranger25ImpVars_omitClass.jpg")
                dev.off
          
          # Grabs the names of the 50 most important variables from predictors dataframe
          ImpVars_names<-unique(Imp_Vars50$predictor)%>%as.character()
          
          # Creates a new model built on important variables
          New_Speclib<-SpecLib_derivs%>%
            dplyr::select(Classes,ImpVars_names)
          
          rfNew<-ranger(Classes ~ .,data = New_Speclib,
                        num.trees =10000,
                        #importance = "impurity_corrected",
                        local.importance = TRUE)
          
          
          # Lets save the confusion matrix from the model above
          Confusion_matrix<-rfNew$confusion.matrix%>%
            as.data.frame()
          
          # Writes out confusion matrix to output folder
          write.csv(Confusion_matrix,"Output/E_006_Ranger50_confusion_matrix_omitClass.csv",row.names = F)

          # saves the model with the lowest error
          save(rfNew, file = "Output/E_007_Best_Model_Ranger_50vars.rda")
          
          #ImportantVars_newFrame<-enframe(rfNew$, name="predictor", value="importance")
         # Selects the 50 most important variables
         #  Imp_Vars50_new<-ImportantVars_newFrame[order(ImportantVars_newFrame$importance,decreasing = TRUE),][1:50,]
          
           #ImportantVars_newFrame[order(ImportantVars_newFrame$importance,decreasing = TRUE),][1:25,]%>%
           # ggplot()+
           # # geom_col(aes(x  = predictor, y = importance))+
           # geom_col(aes(x  = reorder(predictor, +importance), y = importance))+
           # coord_flip()+
           # xlab("predictor")+
           # theme_bw()
          # Saves the plot 
          #ggsave("Output/_E_008_Ranger25ImpVars_50MostImpvars_omitClass.jpg")
          #dev.off()
          





