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
SpecLib_derivs$Functional_group1<- as.factor(SpecLib_derivs$Functional_group1)
SpecLib_derivs$Functional_group2<- as.factor(SpecLib_derivs$Functional_group2)


#Produce summaries of the counts of scans 

SpecLib_derivs_info<-SpecLib_derivs %>% dplyr::select(1:7)

FncGrp2_tally<-SpecLib_derivs_info %>% 
  ungroup() %>%
  group_by(Functional_group2) %>%
  tally
FncGrp1_tally<-SpecLib_derivs_info %>% 
  ungroup() %>%
  group_by(Functional_group1) %>%
  tally


colnames(SpecLib_derivs)

set.seed(123)
# Build Model
#Run once since it has 10,000 trees per model

##
input_1<-SpecLib_derivs %>% dplyr::select(-2:-7)
  dim(input_1)
rf_mod_ranger_species<-ranger::ranger(Classes ~ .,data = input_1, importance = 'impurity_corrected',
                      num.trees = 10000, local.importance = TRUE) # OOB prediction error:             25.93 %
###
input_2<-SpecLib_derivs %>% dplyr::select(-1:-2,-4:-7)
  dim(input_2)
rf_mod_ranger_fncgrp1<-ranger::ranger(Functional_group1 ~ .,data = input_2, importance = 'impurity_corrected',
                                      num.trees = 10000, local.importance = TRUE) # OOB prediction error:             25.93 %
##
input_3<-SpecLib_derivs %>% dplyr::select(-1:-3,-5:-7) 
  dim(input_3)
rf_mod_ranger_fncgrp2<-ranger::ranger(Functional_group2 ~ .,data = input_3, importance = 'impurity_corrected',
                                      num.trees = 10000, local.importance = TRUE) # OOB prediction error:             25.93 %


#rf_mod_randomforest<-randomForest(Classes ~ .,data = SpecLib_derivs
#                                  ,ntree=1000,importance=TRUE) # OOB prediction error 26.18%

# # Build models using 0.99 percent cutoff for corelated varibles
# # Creates corelation matrix
# CorelationMatrix<-cor(SpecLib_derivs[-1])
#
CorelationMatrix1<-SpecLib_derivs %>% dplyr::select(-1:-7) %>% cor()

# # Select most correlated varibles 
# caret_findCorr<-findCorrelation(CorelationMatrix, cutoff = 0.99, names = T)
# 
caret_findCorr1<-findCorrelation(CorelationMatrix1, cutoff = 0.99, names = T)
caret_findCorr2<-findCorrelation(CorelationMatrix1, cutoff = 0.98, names = T)
caret_findCorr3<-findCorrelation(CorelationMatrix1, cutoff = 0.97, names = T)

## Remove corelated vars
# predictor_df_reduced<-SpecLib_derivs %>%
#   dplyr::select(-caret_findCorr)
# 
predictor_df_reduced1<-input_1 %>% dplyr::select(-caret_findCorr1)
  dim(predictor_df_reduced1)
predictor_df_reduced2<-input_2 %>% dplyr::select(-caret_findCorr1)
  dim(predictor_df_reduced2)
predictor_df_reduced3<-input_3 %>% dplyr::select(-caret_findCorr1)
  dim(predictor_df_reduced3)
  
predictor_df_reduced1_2<-input_1 %>% dplyr::select(-caret_findCorr2)
dim(predictor_df_reduced1_2)
predictor_df_reduced2_2<-input_2 %>% dplyr::select(-caret_findCorr2)
dim(predictor_df_reduced2_2)
predictor_df_reduced3_2<-input_3 %>% dplyr::select(-caret_findCorr2)
dim(predictor_df_reduced3_1)
 

predictor_df_reduced1_3<-input_1 %>% dplyr::select(-caret_findCorr3)
dim(predictor_df_reduced1_3)
predictor_df_reduced2_3<-input_2 %>% dplyr::select(-caret_findCorr3)
dim(predictor_df_reduced2_3)
predictor_df_reduced3_3<-input_3 %>% dplyr::select(-caret_findCorr3)
dim(predictor_df_reduced3_3)

predictor_df_reduced3_1 %>% dplyr::select(-1) %>% cor() %>% hist()
## Rebuild models after intercorelated vars are removed
# rf_mod_randomforest<-randomForest(Classes ~ .,data = predictor_df_reduced
#                                   ,ntree=1000,importance=TRUE) # OOB prediction error 23.28%

rf_mod_ranger_species_reduced_1<-ranger::ranger(Classes ~ .,data = predictor_df_reduced1, importance = 'impurity_corrected',
                                      num.trees = 10000, local.importance = TRUE) # OOB prediction error:            
rf_mod_ranger_fncgrp1_reduced_1<-ranger::ranger(Functional_group1 ~ .,data = predictor_df_reduced2, importance = 'impurity_corrected',
                                      num.trees = 10000, local.importance = TRUE) # OOB prediction error:           
rf_mod_ranger_fncgrp2_reduced_1<-ranger::ranger(Functional_group2 ~ .,data = predictor_df_reduced3, importance = 'impurity_corrected',
                                              num.trees = 10000, local.importance = TRUE) # OOB prediction error:          

rf_mod_ranger_species_reduced_2<-ranger::ranger(Classes ~ .,data = predictor_df_reduced1_2, importance = 'impurity_corrected',
                                               num.trees = 10000, local.importance = TRUE) # OOB prediction error:            
rf_mod_ranger_fncgrp1_reduced_2<-ranger::ranger(Functional_group1 ~ .,data = predictor_df_reduced2_2, importance = 'impurity_corrected',
                                               num.trees = 10000, local.importance = TRUE) # OOB prediction error:           
rf_mod_ranger_fncgrp2_reduced_2<-ranger::ranger(Functional_group2 ~ .,data = predictor_df_reduced3_2, importance = 'impurity_corrected',
                                                num.trees = 10000, local.importance = TRUE) # OOB prediction error:          

rf_mod_ranger_species_reduced_3<-ranger::ranger(Classes ~ .,data = predictor_df_reduced1_3, importance = 'impurity_corrected',
                                                num.trees = 10000, local.importance = TRUE) # OOB prediction error:            
rf_mod_ranger_fncgrp1_reduced_3<-ranger::ranger(Functional_group1 ~ .,data = predictor_df_reduced2_3, importance = 'impurity_corrected',
                                                num.trees = 10000, local.importance = TRUE) # OOB prediction error:           
rf_mod_ranger_fncgrp2_reduced_3<-ranger::ranger(Functional_group2 ~ .,data = predictor_df_reduced3_3, importance = 'impurity_corrected',
                                                num.trees = 10000, local.importance = TRUE) # OOB prediction error:          

# # Saves confusion matrix rf
# RandomForest_confusionmatrix<-rf_mod_randomforest$confusion%>%as.data.frame()
# write.csv(RandomForest_confusionmatrix,"Output/E_001_RandomForest_confusionmatrix.csv")
# 

# # Saves confuison Matrix Ranger
# rf_mod_ranger<-ranger(Classes ~ .,data = predictor_df_reduced,
#                       num.trees = 1000,
#                       local.importance = TRUE) # OOB prediction error:             23.76 %
# 
# rf_mod_ranger_IMP<-ranger(Classes ~ .,data = predictor_df_reduced,
#                       num.trees = 1000,
#                       importance = "impurity_corrected",
#                       local.importance = TRUE) # OOB prediction error:             23.76 %  
# 
# Ranger_confusionmatrix<-rf_mod_ranger$confusion.matrix%>%as.data.frame.matrix()
 Ranger_species_confusionmatrix <-rf_mod_ranger_species$confusion.matrix %>% as.data.frame.matrix()
 Ranger_fncgrp1_confusionmatrix <-rf_mod_ranger_fncgrp1$confusion.matrix %>% as.data.frame.matrix()
 Ranger_fncgrp2_confusionmatrix <-rf_mod_ranger_fncgrp2$confusion.matrix %>% as.data.frame.matrix()
 
 Ranger_species_confusionmatrix1_reduced_1 <-rf_mod_ranger_species_reduced_1$confusion.matrix %>% as.data.frame.matrix()
 Ranger_fncgrp1_confusionmatrix2_reduced_1 <-rf_mod_ranger_fncgrp1_reduced_1$confusion.matrix %>% as.data.frame.matrix()
 Ranger_fncgrp2_confusionmatrix3_reduced_1 <-rf_mod_ranger_fncgrp2_reduced_1$confusion.matrix %>% as.data.frame.matrix()
 
 Ranger_species_confusionmatrix1_reduced_2 <-rf_mod_ranger_species_reduced_2$confusion.matrix %>% as.data.frame.matrix()
 Ranger_fncgrp1_confusionmatrix2_reduced_2 <-rf_mod_ranger_fncgrp1_reduced_2$confusion.matrix %>% as.data.frame.matrix()
 Ranger_fncgrp2_confusionmatrix3_reduced_2 <-rf_mod_ranger_fncgrp2_reduced_2$confusion.matrix %>% as.data.frame.matrix()
 
 Ranger_species_confusionmatrix1_reduced_3 <-rf_mod_ranger_species_reduced_3$confusion.matrix %>% as.data.frame.matrix()
 Ranger_fncgrp1_confusionmatrix2_reduced_3 <-rf_mod_ranger_fncgrp1_reduced_3$confusion.matrix %>% as.data.frame.matrix()
 Ranger_fncgrp2_confusionmatrix3_reduced_3 <-rf_mod_ranger_fncgrp2_reduced_3$confusion.matrix %>% as.data.frame.matrix()
 
 #write.csv(Ranger_confusionmatrix,"Output/E_002_Ranger_confusionmatrix.csv")
 write.csv(Ranger_species_confusionmatrix,"Output/E_002_Ranger_species_confusionmatrix.csv")
 write.csv(Ranger_fncgrp1_confusionmatrix,"Output/E_002_Ranger_fncgrp1_confusionmatrix.csv")
 write.csv(Ranger_fncgrp2_confusionmatrix,"Output/E_002_Ranger_fncgrp2_confusionmatrix.csv")
 
 write.csv(Ranger_species_confusionmatrix1_reduced,"Output/E_002_Ranger_species_confusionmatrix1_reduced.csv")
 write.csv(Ranger_fncgrp1_confusionmatrix2_reduced,"Output/E_002_Ranger_fncgrp1_confusionmatrix2_reduced.csv")
 write.csv(Ranger_fncgrp2_confusionmatrix3_reduced,"Output/E_002_Ranger_fncgrp2_confusionmatrix3_reduced.csv")
 
# saves the model with the lowest error
save(rf_mod_randomforest, file = "Output/E_003_Best_Model_RandomForest.rda")

# saves the model with the lowest error
#save(rf_mod_ranger      , file = "Output/E_004_Best_Model_Ranger.rda")

save(rf_mod_ranger_species      , file = "Output/E_004_Best_Model_Ranger_species.rda")
save(rf_mod_ranger_fncgrp1      , file = "Output/E_004_Best_Model_Ranger_fncgrp1.rda")
save(rf_mod_ranger_fncgrp2      , file = "Output/E_004_Best_Model_Ranger_fncgrp2.rda")
save(rf_mod_ranger_species_reduced      , file = "Output/E_004_Best_Model_Ranger_species_reduced.rda")
save(rf_mod_ranger_fncgrp1_reduced      , file = "Output/E_004_Best_Model_Ranger_fncgrp1_reduced.rda")
save(rf_mod_ranger_fncgrp2_reduced      , file = "Output/E_004_Best_Model_Ranger_fncgrp2_reduced.rda")
save(rf_mod_ranger_species_reduced_1      , file = "Output/E_004_Best_Model_Ranger_species_reduced_1.rda")
save(rf_mod_ranger_fncgrp1_reduced_1      , file = "Output/E_004_Best_Model_Ranger_fncgrp1_reduced_1.rda")
save(rf_mod_ranger_fncgrp2_reduced_1      , file = "Output/E_004_Best_Model_Ranger_fncgrp2_reduced_1.rda")
save(rf_mod_ranger_species_reduced_2      , file = "Output/E_004_Best_Model_Ranger_species_reduced_2.rda")
save(rf_mod_ranger_fncgrp1_reduced_2      , file = "Output/E_004_Best_Model_Ranger_fncgrp1_reduced_2.rda")
save(rf_mod_ranger_fncgrp2_reduced_2      , file = "Output/E_004_Best_Model_Ranger_fncgrp2_reduced_2.rda")
save(rf_mod_ranger_species_reduced_3      , file = "Output/E_004_Best_Model_Ranger_species_reduced_3.rda")
save(rf_mod_ranger_fncgrp1_reduced_3      , file = "Output/E_004_Best_Model_Ranger_fncgrp1_reduced_3.rda")
save(rf_mod_ranger_fncgrp2_reduced_3      , file = "Output/E_004_Best_Model_Ranger_fncgrp2_reduced_3.rda")

#Summarize model output

rf_error<-c(
 rf_mod_ranger_species$prediction.error
,rf_mod_ranger_fncgrp1$prediction.error
,rf_mod_ranger_fncgrp2$prediction.error
,rf_mod_ranger_species_reduced_1$prediction.error
,rf_mod_ranger_fncgrp1_reduced_1$prediction.error
,rf_mod_ranger_fncgrp2_reduced_1$prediction.error
,rf_mod_ranger_species_reduced_2$prediction.error
,rf_mod_ranger_fncgrp1_reduced_2$prediction.error
,rf_mod_ranger_fncgrp2_reduced_2$prediction.error
,rf_mod_ranger_species_reduced_3$prediction.error
,rf_mod_ranger_fncgrp1_reduced_3$prediction.error
,rf_mod_ranger_fncgrp2_reduced_3$prediction.error)

rf_num_vars<-c(
  rf_mod_ranger_species$num.independent.variables
  ,rf_mod_ranger_fncgrp1$num.independent.variables
  ,rf_mod_ranger_fncgrp2$num.independent.variables
  ,rf_mod_ranger_species_reduced_1$num.independent.variables
  ,rf_mod_ranger_fncgrp1_reduced_1$num.independent.variables
  ,rf_mod_ranger_fncgrp2_reduced_1$num.independent.variables
  ,rf_mod_ranger_species_reduced_2$num.independent.variables
  ,rf_mod_ranger_fncgrp1_reduced_2$num.independent.variables
  ,rf_mod_ranger_fncgrp2_reduced_2$num.independent.variables
  ,rf_mod_ranger_species_reduced_3$num.independent.variables
  ,rf_mod_ranger_fncgrp1_reduced_3$num.independent.variables
  ,rf_mod_ranger_fncgrp2_reduced_3$num.independent.variables)

rf_levels<-rep(c("1_Fine_Species"
,"2_Medium_Genera"
,"3_Coarse_Families"),4)


rf_cut_off<-c("full","full","full","0.99","0.99","0.99","0.98","0.98","0.98", "0.97","0.97","0.97")

tst<-cbind(rf_levels,rf_num_vars,rf_cut_off,rf_error) %>% 
  as.data.frame() %>%
  mutate(rf_error = round(as.numeric(rf_error),2))

ggplot(tst,aes(x=rf_cut_off, y=rf_error)) + 
         
  #geom_boxplot()+
  geom_point(group=rf_levels, aes(color=rf_levels, shape=rf_cut_off, size=12))+ # geom_line(aes(color=rf_num_vars))
  #ggplot(tst,aes(rf_error)) + geom_bar(group=rf_cut_off)#+
  theme(
  panel.background = element_rect(fill = "white", colour = "grey50"), 
  #legend.key.size = unit(0.5, "cm"),
  #legend.text = element_blank(),
  #legend.position = "none",
  #legend.text = element_text(size=20),
  title = element_text(size=20),
  axis.text.y = element_text(size=20),
  axis.text.x = element_text(size=10,angle = 90))+
  xlab("Correlation cut-off")+
  ylab("Model Error")
  

#------------------------------ Select Important varibles -----------------------------------
# Creates a dataframe with all varibles and their imoportance
# ImportantVarsFrame<-enframe(rf_mod_ranger_IMP$variable.importance, 
#                             name="predictor", value="importance")
ImportantVarsFrame_species<-enframe(rf_mod_ranger_species$variable.importance,name="predictor", value="importance")
ImportantVarsFrame_fncgrp1<-enframe(rf_mod_ranger_fncgrp1$variable.importance,name="predictor", value="importance")
ImportantVarsFrame_fncgrp2<-enframe(rf_mod_ranger_fncgrp2$variable.importance,name="predictor", value="importance")

ImportantVarsFrame_species_reduced_1<-enframe(rf_mod_ranger_species_reduced_1$variable.importance,name="predictor", value="importance")
ImportantVarsFrame_fncgrp1_reduced_1<-enframe(rf_mod_ranger_fncgrp1_reduced_1$variable.importance,name="predictor", value="importance")
ImportantVarsFrame_fncgrp2_reduced_1<-enframe(rf_mod_ranger_fncgrp2_reduced_1$variable.importance,name="predictor", value="importance")

ImportantVarsFrame_species_reduced_2<-enframe(rf_mod_ranger_species_reduced_2$variable.importance,name="predictor", value="importance")
ImportantVarsFrame_fncgrp1_reduced_2<-enframe(rf_mod_ranger_fncgrp1_reduced_2$variable.importance,name="predictor", value="importance")
ImportantVarsFrame_fncgrp2_reduced_2<-enframe(rf_mod_ranger_fncgrp2_reduced_2$variable.importance,name="predictor", value="importance")

ImportantVarsFrame_species_reduced_3<-enframe(rf_mod_ranger_species_reduced_3$variable.importance,name="predictor", value="importance")
ImportantVarsFrame_fncgrp1_reduced_3<-enframe(rf_mod_ranger_fncgrp1_reduced_3$variable.importance,name="predictor", value="importance")
ImportantVarsFrame_fncgrp2_reduced_3<-enframe(rf_mod_ranger_fncgrp2_reduced_3$variable.importance,name="predictor", value="importance")

# Function Creates a plot of the 30 most important vars
# ImportantVarsFrame25<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:25,]
ImportantVarsFrame_species<-ImportantVarsFrame_species[order(ImportantVarsFrame_species$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_species$importance),] %>% mutate(Taxonomic_Scale =  "1_Fine_Species")
ImportantVarsFrame_fncgrp1<-ImportantVarsFrame_fncgrp1[order(ImportantVarsFrame_fncgrp1$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_fncgrp1$importance),] %>% mutate(Taxonomic_Scale =  "2_Medium_Genera")
ImportantVarsFrame_fncgrp2<-ImportantVarsFrame_fncgrp2[order(ImportantVarsFrame_fncgrp2$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_fncgrp2$importance),]  %>% mutate(Taxonomic_Scale = "3_Coarse_Families")

ImportantVarsFrame_species_reduced_1<-ImportantVarsFrame_species_reduced_1[order(ImportantVarsFrame_species_reduced_1$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_species_reduced_1$importance),] %>% mutate(Taxonomic_Scale = "1_Fine_Species")
ImportantVarsFrame_fncgrp1_reduced_1<-ImportantVarsFrame_fncgrp1_reduced_1[order(ImportantVarsFrame_fncgrp1_reduced_1$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_fncgrp1_reduced_1$importance),] %>% mutate(Taxonomic_Scale = "2_Medium_Genera")
ImportantVarsFrame_fncgrp2_reduced_1<-ImportantVarsFrame_fncgrp2_reduced_1[order(ImportantVarsFrame_fncgrp2_reduced_1$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_fncgrp2_reduced_1$importance),]  %>% mutate(Taxonomic_Scale = "3_Coarse_Families")

ImportantVarsFrame_species_reduced_2<-ImportantVarsFrame_species_reduced_2[order(ImportantVarsFrame_species_reduced_2$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_species_reduced_2$importance),] %>% mutate(Taxonomic_Scale = "1_Fine_Species")
ImportantVarsFrame_fncgrp1_reduced_2<-ImportantVarsFrame_fncgrp1_reduced_2[order(ImportantVarsFrame_fncgrp1_reduced_2$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_fncgrp1_reduced_2$importance),] %>% mutate(Taxonomic_Scale = "2_Medium_Genera")
ImportantVarsFrame_fncgrp2_reduced_2<-ImportantVarsFrame_fncgrp2_reduced_2[order(ImportantVarsFrame_fncgrp2_reduced_2$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_fncgrp2_reduced_2$importance),]  %>% mutate(Taxonomic_Scale = "3_Coarse_Families")

ImportantVarsFrame_species_reduced_3<-ImportantVarsFrame_species_reduced_3[order(ImportantVarsFrame_species_reduced_3$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_species_reduced_3$importance),] %>% mutate(Taxonomic_Scale = "1_Fine_Species")
ImportantVarsFrame_fncgrp1_reduced_3<-ImportantVarsFrame_fncgrp1_reduced_3[order(ImportantVarsFrame_fncgrp1_reduced_3$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_fncgrp1_reduced_3$importance),] %>% mutate(Taxonomic_Scale = "2_Medium_Genera")
ImportantVarsFrame_fncgrp2_reduced_3<-ImportantVarsFrame_fncgrp2_reduced_3[order(ImportantVarsFrame_fncgrp2_reduced_3$importance,decreasing = TRUE),][1:length(ImportantVarsFrame_fncgrp2_reduced_3$importance),]  %>% mutate(Taxonomic_Scale = "3_Coarse_Families")

Importance_byPFT<-rbind(ImportantVarsFrame_species,
                        ImportantVarsFrame_fncgrp1,
                        ImportantVarsFrame_fncgrp2) %>% 
                          mutate(Cut_off = "full")
dim(Importance_byPFT)

Importance_byPFT_reduced_1<-rbind(
                        ImportantVarsFrame_species_reduced_1,
                        ImportantVarsFrame_fncgrp1_reduced_1,
                        ImportantVarsFrame_fncgrp2_reduced_1) %>% 
                        mutate(Cut_off = "0.99")


#Importance_byPFT_reduced_1 %>% group_by(Taxonomic_Scale, predictor) %>% summarize(mean_import = mean(importance)) %>% View()

#        ggplot(Importance_byPFT_reduced_1)+
#          #geom_point(aes(x =fct_inorder(as.factor(predictor)), y= importance, color=Taxonomic_Scale)) + 
#          #theme(axis.text.x = element_text(angle = 90)) +
#          #geom_boxplot(aes(y =importance, x= predictor, group=Taxonomic_Scale)) + 
#          geom_point(aes(y =importance, x= predictor, group=Taxonomic_Scale)) + 
#          theme(
#            panel.background = element_rect(fill = "white", colour = "grey50"), 
#            #legend.key.size = unit(0.5, "cm"),
#            #legend.text = element_blank(),
#            #legend.position = "none",
#            legend.text = element_text(size=20),
#            title = element_text(size=20),
#            axis.text.y = element_text(size=20),
#            axis.text.x = element_text(size=10,angle = 90))
#          


Importance_byPFT_reduced_2<-rbind(
  ImportantVarsFrame_species_reduced_2,
  ImportantVarsFrame_fncgrp1_reduced_2,
  ImportantVarsFrame_fncgrp2_reduced_2) %>% 
  mutate(Cut_off = "0.98")

Importance_byPFT_reduced_3<-rbind(
  ImportantVarsFrame_species_reduced_3,
  ImportantVarsFrame_fncgrp1_reduced_3,
  ImportantVarsFrame_fncgrp2_reduced_3) %>% 
  mutate(Cut_off = "0.97")


Importance<-rbind(Importance_byPFT,
                  Importance_byPFT_reduced_1,
                  Importance_byPFT_reduced_2,
                  Importance_byPFT_reduced_3)

Importance_filt<-Importance %>% 
  #Importance %>% subset(Cut_off = "0.99")
  dplyr::filter(importance>2)
dim(Importance)

#Taxonomic_Predictor_Scale<-paste(Importance$Taxonomic_Scale,Importance$Number_of_Variables, sep="_")

#Importance$Taxonomic_Predictor_Scale<-Taxonomic_Predictor_Scale

Importance_wide<- Importance %>% 
  pivot_wider(names_from = c(Taxonomic_Scale,Number_of_Variables), values_from = importance) 


ggplot(Importance_filt)+
  geom_point(aes(x =fct_inorder(as.factor(predictor)), y= importance, color=Taxonomic_Scale)) + 
  #theme(axis.text.x = element_text(angle = 90)) +
  #geom_point(aes(y =importance, x= predictor, color=Taxonomic_Scale)) + 
  theme(
        panel.background = element_rect(fill = "white", colour = "grey50"), 
        #panel.grid.major.x = element_line(colour = "grey50"),
        #legend.key.size = unit(0.5, "cm"),
        #legend.text = element_blank(),
        legend.position = "none",
        #legend.text = element_text(size=20),
        title = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.text.x = element_text(size=6,angle = 90, vjust = -.5))+
  facet_grid(vars(Cut_off)) +
  theme(strip.text.y = element_text(size=20))+
  xlab("Predictors")

ggsave("Output/Variable_Importance_Full_Reduced_Preds.jpg", dpi = 1000, width = 12, height =8)










