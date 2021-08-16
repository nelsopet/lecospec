## Function generates Derivatives for hyperspectral libraries and Hyperspectral images
## The Function takes spectral libraries that are .csv files and Headwall datacubes 
## The function takes three arguments, see lines 4 - 6 below 
## filename = dir.path to where your datacube or Spectral library is located, e.g "path1/path2/path3/"
## out_file =  dir.path to where your results will be written, eg "path1/path2/path3/"
## Classif_Model = dir.path to where the model to be used for classification is located
## A subfolder will be created in you out_file path for aving all the output files

LandCoverEstimator<-function(filename,out_file,Classif_Model,datatype,extension){
  
 
  # --------------------- Functions applied to Datacube/Spectral Library ---------------------
  
  # Creates SubFolder
  if(extension == TRUE){
    
    # Creates the basename for the output filename with extension
    basename_F = basename(file_path_sans_ext(filename))
    
    
  }else{
    
    # Creates the basename for the output filename with no extension
    basename_F = basename(filename)
    
  }
  
  SubFolder<-(paste(out_file,basename_F,sep=""))
  dir.create(SubFolder)
  
  if(datatype == "raster"){
    
    print("Importing Datacube")
    
    # Reads in the Hyperspectral datacubes as a Rasterstack raster
    Converted_Dcube<-brick(filename)
    
    print("Splitting raster into 30 tiles")
    
    # Creates 30 tiles 
    Tiles<-splitRaster(Converted_Dcube[[1]],30)
    
    # Use 4 cores
    cores<- detectCores()-1
    
    print(paste0(cores, " cores being used"))
    
    ## Start cluster
    c1<- parallel::makeCluster(cores, setup_timeout = 0.5)
    doParallel::registerDoParallel(c1)
    parallel::clusterEvalQ(c1,library(raster))
    
    parallel::parLapply(c1,1:length(Tiles),function(x){
      
      # Creates outfile
      Out_tif = paste(SubFolder,"/A_001_",basename_F,"_Tile_",x,".tif",sep = "")
      # Statement checks if file already exist
      if(!file.exists(Out_tif)){
        
        # Crops raster based on the extent of the 24 tiles that we created
        raster::crop(Converted_Dcube,extent(Tiles[[x]]),
                     filename = Out_tif,
                     dataType = "INT4S",format="GTiff",overwrite = T)
        
      }
      print("30 Tiles were sucessfully created")
    })
    
    # Stops cluster
    stopCluster(c1)
    
    cat("\n")
    print("Creating Predicted Layer")
    
    # Creates a list of the names of all the tiles created 
    print("Creating a list of the names of all the tiles created")
    list_of_Tiles<-list.files(SubFolder,
                              pattern = glob2rx("A_001*.tif"),
                              full.names = T)
    
    # Iterate through the list using lapply
    List_of_PredLayers<-lapply(1:length(list_of_Tiles), function(i){
      
      # Creates the name of the output file and the location in which its stored
      out_tif2 = paste0(SubFolder,"/",basename_F,"_PredLayer1.tif")
      
      # Statement checks if file already exist
      if(!file.exists(out_tif2)){
        
        print(paste0("Prediction for Tile ", i, " Initiated"))
        print(paste0("Derivative calculation for Tile ",i, " initiated"))
        
        # Reads in the tiles and converts it to a dataframe
        DfofRas<-brick(list_of_Tiles[[i]])%>%
          rasterToPoints()%>%
          as.data.frame()
        
        if (nrow(DfofRas) == 0){print("No data in tile")} 
        
        
        if (nrow(DfofRas) > 0) {
          
          print("Masking negative values and values greater than 2 percent reflectance")
          
          # Removes noisey bands
          # Find out a way to Automatically remove bands based on the variance
          # remove hard coding 
          DfofRas<-DfofRas[1:274]
          
          # Rename bandpasses
          # Change numeric reference to columns
          # Remove hard coding
          print("Renaming bandpasses")
          names(DfofRas)[-1:-2]<-Headwall_bandpasses
          
          # Convert weird values
          print("Converting weird values to -999")
          DfofRas[-1:-2][DfofRas[-1:-2] > 1.5] <- -999
          DfofRas[-1:-2][DfofRas[-1:-2] < 0  ] <- -999
          
          # Saves all the rownames with NAs
          # ALL_NAs<-which(is.na(DfofRas), arr.ind=TRUE)
          
          # # Saves all the rownames with NAs
          # rownames_NA<-c(ALL_NAs[,1]%>%
          #                  unique)
          
          # Converts NA values
          print("Converting NA values to -999")
          DfofRas[is.na(DfofRas)] <- -999
          
          # Applies Derivative function
          print("Calculating derivatives -999")
          DfofRas<-Deriv_combine(DfofRas)
          
          # Convert NANs AND infs to NAs
          # NANs are being created because there's 0/0 = NAN
          DfofRas[-1:-2][sapply(DfofRas[-1:-2],is.nan)]     <-NA
          DfofRas[-1:-2][sapply(DfofRas[-1:-2],is.infinite)]<-NA
          
          # Convert those rows back to NAs
          # DfofRas[rownames_NA,-1:-2] <- NA
          
          # Reads in classifier
          print("Loading classifier")
          Model = get(load(Classif_Model))
              ##BUG 
              #Error in readChar(con, 5L, useBytes = TRUE) : cannot open the connection
              #In addition: Warning messages:
              #  1: In dir.create(SubFolder) : 'Output/SubsetDatacube' already exists
              #2: In readChar(con, 5L, useBytes = TRUE) :
              #  cannot open compressed file 'Output/E_007_Best_Model_Ranger_50vars.rda', probable reason 'No such file or directory'
              #Called from: readChar(con, 5L, useBytes = TRUE)
              # Grabs the names of the varibles
          print("Loading varible names")
          Vars_names<-c(Model$forest$independent.variable.names) #(ranger model)
          #Vars_names<-c(Model$forest$independent.variable.names) #(ranger model)
          #Vars_names<-c(as.data.frame(Model$importance)%>%rownames())
          
          # Removes the X from columns with the names of banpasses
          print("Fixing Names of varibles ")
          #Vars_names2<-gsub("^XIL","",Vars_names[1:length(Vars_names)])
          Vars_names2<-gsub("^X","",Vars_names[1:length(Vars_names)]) #ranger
          
          # Creates a new model built on important variables
          print("Selecting important varibles")
          New_df<-DfofRas%>%
            dplyr::select(x,y,all_of(Vars_names2))
          
          # Converts the results to a raster Brick and saves it on disk
          print("Converting results to a raster brick")
          #OFF for ranger
          RastoDF<-raster::rasterFromXYZ(New_df,
                                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          
          names(RastoDF)<-colnames(New_df)[-1:-2]
          
          # Deletes the dataframe
          rm(DfofRas)
          #rm(New_df) 
          
          # Grabs the spitial information from original raster layer
          RasterBrick<-brick(filename)
          
          print(paste0("Pointing Model at Tile ", i))
          
          # Predict calss of each pixel and returns a Raster layer
          #Predicted_layer<-raster::predict(RastoDF,Model,na.rm = TRUE,progress='text')
          #Predicted_layer<-raster::predict(RastoDF,Model,na.rm = TRUE,progress='text') #randomForest 
          Predicted_layer<-raster::predict(RastoDF,Model,na.rm = FALSE,type='response', progress='text', fun = function(Model, ...) predict(Model, ...)$predictions) #(Ranger model)
          #Predicted_layer<-predict(New_df,Model,na.rm = TRUE,progress='text')#ranger
          Predicted_layer<-raster::rasterFromXYZ(Predicted_layer,
                                                 crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          #ranger ... change back to raster for the rest of the pipline
          cat("\n")
          rm(New_df)
          # Matches the spaitial information to the original raster
          Predicted_layerResamp<-raster::resample(Predicted_layer,RasterBrick,method = "ngb")%>%
            as.factor()
          
          # Restores attribute table
          Predicted_layerResamp@data@attributes <- Predicted_layer@data@attributes
          
          # # Writes out the predicted Layer
          writeRaster(Predicted_layerResamp,filename = paste0(SubFolder,"/B_001_",basename(filename),"_Tile",i,"_PredLayer.tif")
                      ,overwrite = T)
          
          print(paste0(" Prediction for Tile ",i," Completed"))
          cat("\n")
          
          return(Predicted_layerResamp)
          
        }
      }
      
    })
    
    # combines the tiles and writes the output to disk
    Predicted_Layer<-do.call(raster::merge, List_of_PredLayers)%>%
      as.factor()
    
    # Restres attribute table
    Predicted_Layer@data@attributes <- List_of_PredLayers[[1]]@data@attributes
    
    writeRaster(Predicted_Layer,filename = paste0(SubFolder,"/",basename(filename),"_PredLayer.tif")
                ,overwrite = T)
    
    return(Predicted_Layer)
  }
  
}
