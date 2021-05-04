# Functions returns columns that are bandpasses
metaRemove<-function(x){
  meta<-c(grep("^[0-9][0-9][0-9]",colnames(x)))
  colremove<-x[,meta]
  return(colremove)
}# metaRemove Function ends

# Functions returns columns that are not bandpasses
bandsRemove<-function(x){
  meta<-c(grep("[a-z A-Z]",colnames(x)))
  colremove<-x[,meta]
  return(colremove)
}# bandsRemove Function ends