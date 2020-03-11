## Builds a random forest classification model from hyperspectral dataset 

Spectral_classifier<-function(classifier){
  
  # Creates a string of possible names that will be removed
  remove_names<-c("ScanID","PFT","PFT_2","PFT_4","Area","PFT2_Freq"
                  ,"PFT3_Freq","PFT4_Freq","Tree_numbe","x","y")
  
  # Reads in spectral library
  Spectral_Library<-read.csv(classifier)
  
  # Removes unwanted metadata from dataframe 
  # (the only columns we need are the PFT_3 column and all the predictors)
  Spectral_Library[remove_names] = NULL
  
  # Change column name with all the levels to "classes"
  names(Spectral_Library)[1]<-"Classes"
  
  # Function selects the 50 most importnat variables
  ImportantVars<-function(x){
    
    # Creates Random forest Model
    set.seed(2017)
    rf_mod<-randomForest(Classes ~ .,data = x,
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
  } # ImportantVars function ends
  
  Randomforest_mod<-function(x){
    # creates a dataframe with the Important varibles
    mostImportantVars<-ImportantVars(Spectral_Library)
    
    # Grabs the 50 most important variables from predictors dataframe
    ImpVars<-unique(mostImportantVars$data$variable)%>%as.character()%>%
      as.character()
    
    # Creates a new model built on important variables
    newdf<-Spectral_Library%>%
      dplyr::select(Classes,ImpVars)
    
    rfNew<-randomForest(Classes ~ .,data = newdf,
                        mtry = sqrt(ncol(newdf)),
                        ntree = 1001,
                        localImp = TRUE)
    # You could write out the confusion matrix
    # You could save the model to desired folder
    return(rfNew)
  } # Random Forest Function ends
  
  Randomforest_mod(classifier)
  
}# Function Spectral ends
