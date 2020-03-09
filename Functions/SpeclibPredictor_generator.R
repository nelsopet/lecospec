# Function generates predictors for hyperspectral libraries
Spectral_Predictors<-function(PREDICT){
  
  # Lets create functions that will remove all the metatdata and one that will keep the data for bandspasses
  # These function will be used alot
  # Returns columns that are bandpasses
  metaRemove<-function(x){
    meta<-c(grep("^[0-9][0-9][0-9]",colnames(x)))
    colremove<-x[,meta]
    return(colremove)
  }
  
  # Returns columns that are not bandpasses
  bandsRemove<-function(x){
    meta<-c(grep("[a-z A-Z]",colnames(x)))
    colremove<-x[,meta]
    return(colremove)
  }# Function ends

# Function resamples a spectral library every 5, 10, 50 and 100 nm and combines those results into a dataframe
# Resampling will change reflectance values which might result in negative values (need to remove these rows)
Func_Resamp<-function(Resamp){
  # Function adds  suffix to columns in dataframes
  # This is a uniqe way to identify columns when merging these dataframes later 
  add_suffix<-function(x){
    colnames(x)<-paste(colnames(x),deparse(substitute(x)),sep = "")
    return(x)}
  
  # Removes metadata before function can be applied
  DF<-metaRemove(Resamp)
  
  # Function resamples spectral library every 5nm
  nm_5<-DF%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,5))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_5<-add_suffix(nm_5)
  
  # Function resamples spectral library every 10nm
  nm_10<-DF%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,10  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_10<-add_suffix(nm_10)
  
  # Function resamples spectral library every 50nm
  nm_50<-DF%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,50  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_50<-add_suffix(nm_50)
  
  # Function resamples spectral library every 100nm
  nm_100<-DF%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,100  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_100<-add_suffix(nm_100)
  
  # Combines 5nm, 10nm 50nm and 100nm dataframes and add metadata
  df<-Reduce(cbind,list(bandsRemove(Resamp)
                        ,nm_5
                        ,nm_10
                        ,nm_50
                        ,nm_100))
  # Removes rows with negative values or values >2
  goodValues<-df%>%
    filter_at(vars(c(colnames(metaRemove(df)))), all_vars(.  <2))%>%
    filter_at(vars(c(colnames(metaRemove(df)))), all_vars(. >=0))
  return(goodValues)}

Func_VI<-function(VI){
  
  # Converts dataframe to matrix before VIs can be applied
  matrix_a<-as.matrix(metaRemove(VI))
  
  # Creates numeric vector of wavelengths
  bat<-metaRemove(VI)%>%
    colnames()%>%
    as.numeric()
  
  # Creates a spectralib object
  spec_library<- speclib(matrix_a,bat)
  
  # creates a vectror of names of all the vegitation indices
  AVIRIS_VI  <-vegindex()[-58]
  Headwall_VI<-vegindex()[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]
  
  # Creates dataframe with Vegitation indices
  VI_CALC<-if(ncol(metaRemove(VI)) == 272){
    vegindex(spec_library,index = Headwall_VI)
  } else {
    vegindex(spec_library,index = AVIRIS_VI)}
  
  # Function removes spaces and special charcters from column names
  # Models will not run if these aren't removed
  names(VI_CALC)<-str_remove_all(names(VI_CALC),"[[:punct:]]| ")
  
  VI_DF<-cbind(bandsRemove(VI),VI_CALC)
  return(VI_DF)
}

# Creates Predictors for model prediction
Func_preds<-function(x){
  
  preds<-if (ncol(metaRemove(x)) == 272){
    cbind(Func_VI(x),
          Func_Resamp(metaRemove(x)))
  }else{
    Func_VI(x)
  }
  
  # Removes Infs and NA values from dataframe 
  Df_preds<-preds[Reduce(`&`,lapply(preds,function(x)
    !is.na(x) & is.finite(x))),]
  
  return(Df_preds)}

output_df<-Func_preds(PREDICT)

}# Function ends

# Test function below
# Test_case<-Spectral_Predictors(PUT NAME OF YOUR DATA HERE)
















