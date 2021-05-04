# Function Responsible For Vegindex Calculations
Func_VI<-function(VI){
  
  print("Calculating Vegitation Indices")
  
  # Converts dataframe to matrix before VIs can be applied
  matrix_a<-as.matrix(metaRemove(VI))
  
  # Creates numeric vector of wavelengths
  namescolumn<-metaRemove(VI)%>%
    colnames()%>%
    as.numeric()
  
  # Creates a spectralib object
  spec_library<-hsdar::speclib(matrix_a,namescolumn)
  
  # creates a vectror of names of all the vegitation indices
  AVIRIS_VI  <-hsdar::vegindex()[-58]
  Headwall_VI<-hsdar::vegindex()[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]
  
  # Get amount of cores to use
  cores <- parallel::detectCores()-1
  
  # prepare for parallel process
  c1<- parallel::makeCluster(cores, setup_timeout = 0.5)
  doParallel::registerDoParallel(c1)
  
  
  # Creates dataframe with Vegitation indices
  VI_CALC<-if(ncol(metaRemove(VI)) == 272){
    foreach(i=1:length(Headwall_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
      a<-hsdar::vegindex(spec_library, index=Headwall_VI[[i]], weighted=FALSE)}
    
  } else {
    foreach(i=1:length(AVIRIS_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
      a<-hsdar::vegindex(spec_library, index=AVIRIS_VI[[i]], weighted = FALSE)}
  }
  
  # Stops cluster
  parallel::stopCluster(c1)
  
  # Converts Matrix to a datframe 
  VI_CALC<-as.data.frame(VI_CALC)
  
  # Function Renames columns
  if(ncol(VI_CALC) == 95){
    names(VI_CALC)<-Headwall_VI
  } else {
    names(VI_CALC)<-AVIRIS_VI}
  
  # Function removes spaces and special charcters from column names
  # Models will not run if these aren't removed
  names(VI_CALC)<-str_remove_all(names(VI_CALC),"[[:punct:]]| ")
  
  # Conbines VIs and Lat/long info
  VI_DF<-cbind(bandsRemove(VI),VI_CALC)
  
  print("Vegitation index calculations successful")
  
  return(VI_DF)
} # Func_VI ends