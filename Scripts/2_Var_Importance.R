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
# Creates corelation matrix
CorelationMatrix<-cor(SpecLib[-1])

# Select most correlated varibles
caret_findCorr<-findCorrelation(CorelationMatrix, cutoff = 0.99, names = T)

# Remove corelated vars
predictor_df_reduced<-SpecLib %>%
  dplyr::select(-caret_findCorr)

# Rebuild Model
rf_mod_rang2<-ranger(Classes ~ .,data = predictor_df_reduced,
                    num.trees = 10000,
                    importance = "impurity_corrected",
                    local.importance = TRUE) # OOB prediction error:             29.32 % 

#------------------------------ Select Important varibles -----------------------------------
# Creates a dataframe with all varibles and their imoportance
ImportantVarsFrame<-enframe(rf_mod_rang2$variable.importance, 
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
  coord_flip()
  

# Creates a sequence of numbers that, represents the number of varibles to choose to build models
NoofVars<-seq(5,70,by = 5)

# List of models
Modslist<-lapply(1:length(NoofVars),function(x){
    
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
                num.trees =10000,
                importance = "impurity_corrected",
                local.importance = TRUE)
  return(rfNew)
 
})%>%
  setNames(paste(NoofVars,"Most_ImpVaribles",sep="_"))

# We can print the prediction error for each model, see below
listofmoderors<-lapply(Modslist,function(x)
  return(x$prediction.error))

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
  coord_flip()



















