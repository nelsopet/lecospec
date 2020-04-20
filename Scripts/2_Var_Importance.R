# ------------------------------------ vARIBLE IMPORTANCE sCRIPT --------------------------
library(caret)
library(ranger)
library(randomForest)
library(tidyverse)

#------------------Building Model without identifying important varibles --------------
# Spectral Library
SpecLib<-read.csv("Output/D_002_SpecLib_Derivs.csv")

# Remove Unwanted columns
# Creates a string of possible names that will be removed
remove_names<-c("ScanID","Class1","Class2","Class4","Area","Class2_Freq"
     
                           ,"Class3_Freq","Class4_Freq","Tree_numbe","x","y")
# Remove Unwanted columns
SpecLib[remove_names] = NULL

# Change column name with all the levels to "classes"
names(SpecLib)[1]<-"Classes"

set.seed(123)
# Build Model
rf_mod_rang1<-ranger(Classes ~ .,data = SpecLib,
                    num.trees = 10000,
                    importance = "impurity_corrected",
                    local.importance = TRUE) # OOB prediction error:             33.07 % 

# ------------------------------- Remove correlated varibles ---------------------------
# Creates a sequence of numbers that, represents the number of varibles to choose to build models
NoofVars1<-seq(0.900,0.99,by = 0.01)

# List of models
Modslist_cor<-lapply(1:length(NoofVars1),function(x){
  
  # Creates corelation matrix
  CorelationMatrix<-cor(SpecLib[-1])
  
  # Select most correlated varibles
  caret_findCorr<-findCorrelation(CorelationMatrix, cutoff = NoofVars1[x], names = T)
  
  # Remove corelated vars
  predictor_df_reduced<-SpecLib %>%
    dplyr::select(-caret_findCorr)
  
  # Rebuild Model
  rf_mod_rang2<-ranger(Classes ~ .,data = predictor_df_reduced,
                       num.trees = 10000,
                       importance = "impurity_corrected",
                       local.importance = TRUE) # OOB prediction error:             29.32 % 
  return(rf_mod_rang2)
  
})%>%
  setNames(paste(NoofVars1,"Cor_Varibles",sep="_"))

# We can print the prediction error for each model, see below
listofmoderors_COR<-lapply(Modslist_cor,function(x)
  return(x$prediction.error))

#$`0.9_Cor_Varibles`
#[1] 0.291834
#
#$`0.91_Cor_Varibles`
#[1] 0.2784471
#
#$`0.92_Cor_Varibles`
#[1] 0.291834
#
#$`0.93_Cor_Varibles`
#[1] 0.2878179
#
#$`0.94_Cor_Varibles`
#[1] 0.2771084
#
#$`0.95_Cor_Varibles`
#[1] 0.2904953
#
#$`0.96_Cor_Varibles`
#[1] 0.2931727
#
#$`0.97_Cor_Varibles`
#[1] 0.2784471
#
#$`0.98_Cor_Varibles`
#[1] 0.3159304
#
#$`0.99_Cor_Varibles`
#[1] 0.3052209

# Creates a dataframe with errors for each model
error_COR_df<-do.call("rbind",listofmoderors_COR)%>%
  as.data.frame()

# Adds another column
error_COR_df$Vars<-rownames(error_COR_df)

# Changes column names
names(error_COR_df)<-c("error","Vars")

# Lets R respect the order in data.frame.
error_COR_df$Vars <- factor(error_COR_df$Var,
                        levels = error_COR_df$Var
                        [order(error_COR_df$error)])

# Creates a plot of the 30 most important varibles
error_COR_df%>%
  ggplot(aes(x  = Vars, y = error))+
  theme_bw()+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Model errors after pair-wise correlations are removed")

ggsave("Model_errors_Cors.jpg")

#------------------------------ Select Important varibles -----------------------------------
# Creates a dataframe with all varibles and their imoportance
ImportantVarsFrame<-enframe(Modslist_cor[[5]]$variable.importance, 
                            name="predictor", value="importance")
  
# Function Creates a plot of the 30 most important vars
ImportantVarsFrame30<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:30,]

# Lets R respect the order in data.frame.
ImportantVarsFrame30$predictor <- factor(ImportantVarsFrame30$predictor,
                                       levels = ImportantVarsFrame30$predictor
                                       [order(ImportantVarsFrame30$importance)])

# Creates a plot of the 30 most important varibles
ImportantVarsFrame30%>%
  ggplot(aes(x  = predictor, y = importance))+
  theme_bw()+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("30 Most Important Varibles")

ggsave("Important_Varibles_30.jpg")
  

# Creates corelation matrix
CorelationMatrix<-cor(SpecLib[-1])

# Select most correlated varibles
caret_findCorr<-findCorrelation(CorelationMatrix, cutoff = 0.94, names = T)

# Remove corelated vars
predictor_df_reduced<-SpecLib %>%
  dplyr::select(-caret_findCorr)

# Rebuild Model
rf_mod_rang2<-ranger(Classes ~ .,data = predictor_df_reduced,
                     num.trees = 10000,
                     importance = "impurity_corrected",
                     local.importance = TRUE) # OOB prediction error:             28.78 % 

# Creates a sequence of numbers that, represents the number of varibles to choose to build models
NoofVars<-seq(5,50,by = 5)

# List of models
Modslist<-lapply(1:length(NoofVars),function(x){
  
  # Creates a dataframe with all varibles and their imoportance
  ImportantVarsFrame<-enframe(rf_mod_rang2$variable.importance, 
                              name="predictor", value="importance")
    
  # Function selects the most important varibles
  Imp_Vars<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:NoofVars[x],]
  
  # Grabs the names of the 50 most important variables from predictors dataframe  
  ImpVars_names<-unique(Imp_Vars$predictor)%>%
    as.character()
  
  # Creates a new model built on important variables
  New_Speclib<-predictor_df_reduced%>%
    dplyr::select(Classes,ImpVars_names)
  
  # Build ne model
  rfNew<-ranger(Classes ~ .,data = New_Speclib,
                num.trees =1000,
                local.importance = TRUE)
  return(rfNew)
 
})%>%
  setNames(paste("A",NoofVars,"Most_ImpVaribles",sep="_"))

# We can print the prediction error for each model, see below
listofmoderors<-lapply(Modslist,function(x)
  return(x$prediction.error))

#$`5_Most_ImpVaribles`
#[1] 0.4605087
#
#$`10_Most_ImpVaribles`
#[1] 0.3547523
#
#$`15_Most_ImpVaribles`
#[1] 0.3413655
#
#$`20_Most_ImpVaribles`
#[1] 0.3145917
#
#$`25_Most_ImpVaribles`
#[1] 0.3038822
#
#$`30_Most_ImpVaribles`
#[1] 0.2811245
#
#$`35_Most_ImpVaribles`
#[1] 0.2958501
#
#$`40_Most_ImpVaribles`
#[1] 0.2904953
#
#$`45_Most_ImpVaribles`
#[1] 0.2851406
#
#$`50_Most_ImpVaribles`
#[1] 0.2891566

# Creates a dataframe with errors for each model
error_df<-do.call("rbind",listofmoderors)%>%
  as.data.frame()

# Adds another column
error_df$Vars<-rownames(error_df)

# Changes column names
names(error_df)<-c("error","Vars")

# Lets R respect the order in data.frame.
error_df$Vars <- factor(error_df$Var,
                        levels = error_df$Var
                        [order(error_df$error)])

# Creates a plot of the 30 most important varibles
error_df%>%
  ggplot(aes(x  = Vars, y = error))+
  theme_bw()+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Model errors after selecting the most important varibles")

ggsave("Model_errors_VarImp.jpg")

list2env(Modslist ,.GlobalEnv)

# saves the model with the lowest error
save(A_30_Most_ImpVaribles, file = "Output/Best_Model.rda")



















