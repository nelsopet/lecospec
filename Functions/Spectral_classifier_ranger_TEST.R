# Builds a random forest classification model from hyperspectral dataset 
# Function takes two arguments, the spectral library object and 
# output directory in which you want your files to be stored

Spectral_classifier_ranger<-function(x, out_file){
  
  # Creates a string of possible names that will be removed
  remove_names<-c("ScanID","Class1","Class2","Class4","Area","Class2_Freq"
                  ,"Class3_Freq","Class4_Freq","Tree_numbe","x","y")
  
      # Unit test PASSES
      # Spectral_Library<-read.csv(names_SpecLibPreds) %>% dim() #6506 by 268
  # Reads in spectral library
  Spectral_Library<-read.csv(x)
  
      # Unit test PASSES 
      # Spectral_Library[remove_names] = NULL # dim( Spectral_Library) 6506 by 265 ... removes 3 columns
  # Removes unwanted metadata from dataframe 
  Spectral_Library[remove_names] = NULL
  
  # Change column name with all the levels to "classes"
  names(Spectral_Library)[1]<-"Classes"
  
  Spectral_Library[, "Classes"] <- as.factor(Spectral_Library[, "Classes"])

    set.seed(123)
    # Unit test passes
    rf_mod_rang<-ranger(Classes ~ .,data = Spectral_Library,
                        num.trees = 300,
                        importance = "impurity_corrected",
                        local.importance = TRUE)
    
    # Selects the 50 most important variables
    ImportantVarsFrame<-enframe(rf_mod_rang$variable.importance, 
                                name="predictor", value="importance")
    
    # Selects the 50 most important variables
    Imp_Vars50<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:50,]
    
    # Creates a plot of the 25 most important varibles
    #pdf(paste(out_file,"ImpVars",".pdf",sep =""))#, units="px",height = 1400, width=2400, res=350)
    
    # Creates plot
    ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:25,]%>%
      ggplot()+
      geom_col(aes(x  = predictor, y = importance))+
      coord_flip()+
      theme_bw()
    
    # Saves the plot 
    ggsave(paste(out_file,"ImpVars.png", sep=""))
    dev.off
    
    # Grabs the names of the 50 most important variables from predictors dataframe
    ImpVars_names<-unique(Imp_Vars50$predictor)%>%as.character()
    
    # Creates a new model built on important variables
    New_Speclib<-Spectral_Library%>%
      dplyr::select(Classes,ImpVars_names)
    
    # Build ne model
    rfNew<-ranger(Classes ~ .,data = New_Speclib,
                  num.trees =500,
                  importance = "impurity_corrected",
                  local.importance = TRUE)
    
    
    # Lets save the confusion matrix from the model above
    Confusion_matrix<-rfNew$confusion.matrix%>%
      as.data.frame()
    
    # Writes out confusion matrix to output folder
    write.csv(Confusion_matrix,paste0(out_file,"confusion_matrix.csv"),row.names = F)
    
    return(rfNew)
    
  } # Random Forest Function ends
  
# Heres an example of how to use this function
Final_model<-Spectral_classifier_ranger("Output/SpecLib_Derivs.csv", out_file = "Output/")


#outputs_folder<-"/Forests/ROutputs/"
#input_folder  <-"/Forests/ROutputs/Predictors/"
#
#
#names_SpecLibPreds = list.files(input_folder, pattern="all_preds_cal",full.names = T)
#
## Reads in spectral library and their predictors for each sensor
#
#SpecLibs_Preds<-lapply(names_SpecLibPreds,Spectral_classifier_ranger) %>% 
#  
#  # Removes dir path from the name
#  setNames(gsub(input_folder,"",names_SpecLibPreds))
#
## Unlist models to the environment
#list2env(SpecLibs_Preds ,.GlobalEnv)
#
#all_confusionMatrix_ranger<-all_preds.csv$confusion%>%as.data.frame()
