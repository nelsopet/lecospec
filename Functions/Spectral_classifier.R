## Builds a random forest classification model from hyperspectral dataset 

Spectral_classifier<-function(classifier){
  
  # Function selects the 50 most importnat variables
  ImportantVars<-function(x){
    
    # Creates a string of possible names that will be removed
    remove_names<-c("ScanID","PFT","PFT_2","PFT_4","Area","PFT2_Freq"
                    ,"PFT3_Freq","PFT4_Freq","Tree_numbe","x","y")
    
    # Removes unwanted metadata from dataframe 
    # (the only columns we need are the PFT_3 column and all the predictors)
    x[remove_names] = NULL
    
    # Creates Random forest Model
    set.seed(2017)
    rf_mod<-randomForest(PFT_3~.,data = x,
                         mtry = sqrt(ncol(x)),
                         ntree = 1001,
                         localImp = TRUE)
    
    # Selects the 50 most important variables
    # This is achieved by calculating distribution of minimal depth 
    ImportantVars<-plot_min_depth_distribution(
      min_depth_distribution(rf_mod),
      min_no_of_trees = 200,
      mean_sample = "relevant_trees",
      k = 50)
    return(ImportantVars)
  }
  
  Randomforest_mod<-function(x){
    # creates a dataframe with the Important varibles
    mostImportantVars<-ImportantVars(x)
    
    # Grabs the 50 most important variables from predictors dataframe
    ImpVars<-unique(mostImportantVars$data$variable)%>%as.character()%>%
      as.character()
    
    # Creates a new model built on important variables
    # Need to change PFT_3 to classes
    # Classes should be the column you're trying to predict in a classification problem
    newdf<-x%>%
      dplyr::select(PFT_3,ImpVars)
    
    rfNew<-randomForest(PFT_3 ~.,data = newdf,
                        mtry = sqrt(ncol(newdf)),
                        ntree = 1001,
                        localImp = TRUE)
    # You could write out the confusion matrix
    # You could save the model to desired folder
    return(rfNew)
  }
  
  Randomforest_mod(classifier)
  
}# Function ends

# Test function below
# Test_case2<-Spectral_classifier(SpecLibs_Preds[[2]])