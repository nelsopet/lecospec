#New image test
#The scripts below run with the small test dataset but not a full datacube
source("Scripts/1_Species_dataframe.R")
source("Scripts/2_DataMunging.R")
source("Scripts/3_Create_SpecLibPSR.R")
source("Scripts/4_Model_Development.R")
#source("Scripts/5_Classify_Image1.R") #Change which datacube is being used in this script


##One tile of new image test
##Step through nelsopet_rf functions with one tile of a full image to see what is happening
list.files("Functions/nelsopet_rf") %>% as.matrix()
source("Functions/nelsopet_rf/1_Bandpass_Def.R"       )      
source("Functions/nelsopet_rf/1_FuncResamp.R"         )      
source("Functions/nelsopet_rf/1_MetaBand_Remove.R"    )      
source("Functions/nelsopet_rf/1_VI_CALC.R"            )      
source("Functions/nelsopet_rf/2_DerivCombine.R"       )      
source("Functions/nelsopet_rf/3_Make_SpecLib_Derivs.R")      

#Define testing inputs
filename = "Data/SubsetDatacube"
#filename = "M:/Alaksa_Datacubes/Raw_files/WickershamDome_2019_08_08_19_31_51_2000_rd_rf_or"
#out_file = "M:/Alaksa_Datacubes/Predictions/NEW/"
out_file = "Output/"
extension = FALSE


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

#Define datatype to be used in Deriv_Combine
datatype = "raster"

#Make one tile
Out_tif = paste(SubFolder,"/A_001_",basename_F,"_Tile_",1,".tif",sep = "")
raster::crop(Converted_Dcube,extent(Tiles[[1]]),
             filename = Out_tif,
             dataType = "INT4S",format="GTiff",overwrite = T)        


#Make a list of tiles, in the test case there will be 1
list_of_Tiles<-list.files(SubFolder,
                          pattern = glob2rx("A_001*.tif"),
                          full.names = T)

#Load classifier
Classif_Model = "Output/E_004_Best_Model_Ranger.rda"

#Apply Derive_combine, classifier
#List_of_PredLayers<-lapply(1:length(list_of_Tiles), function(i) {
output_filenames <- list()

  
  # Creates the name of the output file and the location in which its stored
  out_tif2 = paste0(SubFolder, "/",basename_F,"_PredLayer1.tif")
  
  # Statement checks if file already exist
  #if(!file.exists(out_tif2)){
    
    print(paste0("Prediction for Tile ", i, " Initiated"))
    print(paste0("Derivative calculation for Tile ",i, " initiated"))
    
    # Reads in the tiles and converts it to a dataframe
    DfofRas<-brick(list_of_Tiles[[1]])%>%
      rasterToPoints()%>%
      as.data.frame()
    
    #if (nrow(DfofRas) == 0){print("No data in tile")} 
    
    
    #if (nrow(DfofRas) > 0) {
      
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
      
      # Grabs the names of the varibles
      print("Loading varible names")
      Vars_names<-c(Model$forest$independent.variable.names) #(ranger model)
      #Vars_names<-c(Model$forest$independent.variable.names) #(ranger model)
      #Vars_names<-c(as.data.frame(Model$importance)%>%rownames())
      
      # Removes the X from columns with the names of bandpasses
      print("Fixing Names of varibles ")
      #Vars_names2<-gsub("^XIL","",Vars_names[1:length(Vars_names)])
      Vars_names2<-gsub("^X","",Vars_names[1:length(Vars_names)]) #ranger
      
      # Creates a new model built on important variables
      print("Selecting important varibles")
      New_df<-DfofRas%>%
        dplyr::select(x,y,all_of(Vars_names2))
      
      print("Data Frame of Variables Selected")
      print(colnames(New_df))
      
      print("Saving to Disk")
      save_path <- "Data/base_df.csv"
      write.table(DfofRas, file = save_path, sep = ",")
      
      # Converts the results to a raster Brick and saves it on disk
      print("Converting results to a raster brick")
      #OFF for ranger
      RastoDF<-raster::rasterFromXYZ(
        New_df,
        crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      )
      
      names(RastoDF) <- colnames(New_df)[-1:-2]
      
      # Deletes the dataframe
      rm(DfofRas)
      #rm(New_df) 
      
      # Grabs the spatial information from original raster layer <- no it doesn't.  This loads the data
      RasterBrick <- raster::brick(filename)
      
      
      
      print(paste0("Pointing Model at Tile ", i))
      
      # function to fill in missing values using partial mean matching
      impute_spectra <- function(x) {
        
        df <- x
        
        if (! is.data.frame(x)) {
          df <- x %>%
            rasterToPoints() %>%
            as.data.frame()
        }
        if (is.data.frame(df)){
          print("input converted to dataframe")
          print("input df dimension")
          print(dim(df))
        } else {
          print("Conversion to dataframe failed")
        }
        
        print("Imputing...")
        missForest::missForest(df, maxiter = 1)
        print("Checking Imputer Data Type: pass?")
        print(is.data.frame(df))
        print("Checking for missing Values...")
        print(which(is.na(df)))
        
        print("Imputed Data of size")
        print(dim(df))
        
        
        
        if (! is.data.frame(x)) {
          
          ExtractBands <- function(df){
            bands <- .RemoveBandColumn(df) %>%
              colnames() %>%
              as.numeric()
            print("Bands")
            print(bands)
            return(bands)
          }
          
          .RemoveMetaColumn<-function(x){
            meta <- c(grep("^[0-9][0-9][0-9]", colnames(x)))
            print("Input Dimension")
            print(dim(x))
            df_no_metadata <- x[, ! names(x) %in% meta]
            #df_no_metadata <- subset(x, select = -meta)
            return(df_no_metadata)
          }
          
          .RemoveBandColumn<-function(x){   
            meta<-c(grep("[a-z A-Z]",colnames(x)))
            colremove <- x[,meta]
            return(colremove)
          }
          
          print("Removing Metadata")
          df_no_metadata <- .RemoveMetaColumn(df)
          print("Converting to Matrix")
          spectralMatrix <- as.matrix(df)
          print("Converting to Spectral Library")
          output_data <- raster::rasterFromXYZ(
            spectralMatrix,
            crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          raster::extent(output_data) <- raster::extent(x)
        }
        return(output_data)
      }#end impute_spectra
      
      print("Imputing spectra")
      cleaned_RastoDF <- impute_spectra(RastoDF)
      print("DF Before Imputing")
      print(RastoDF)
      print("DF After Imputing")
      print(cleaned_RastoDF)
      
      #cleaned_RastoDF <- na.roughfix(RastoDF)# KRB
      #print(paste0(summary(cleaned_RastoDF)))
      
      # Predict calss of each pixel and returns a Raster layer
      #Predicted_layer<-raster::predict(RastoDF,Model,na.rm = TRUE,progress='text')
      #Predicted_layer_df<-raster::predict(RastoDF,Model,progress='text') #randomForest 
      print("Running Prediction")
      Predicted_layer_df<-raster::predict(cleaned_RastoDF,
                                          Model,
                                          na.rm = TRUE,
                                          type='response',
                                          progress='text',
                                          fun = function(Model, ...) predict(Model, ...)$predictions) #(Ranger model)
      #Predicted_layer<-predict(New_df,Model,na.rm = TRUE,progress='text')#ranger
      
      
      
      print("Model Output")
      print(Predicted_layer_df)
      
      #Predicted_layer <- raster::rasterFromXYZ(
      #  Predicted_layer_df,
      #  crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      #
      Predicted_layer<-Predicted_layer_df
      # Set extent of predicted to be the same as the Data Brick
      print("Fixing Extents")
      raster::extent(Predicted_layer) <- raster::extent(RasterBrick)
      
      
      #print("Raster Brick Attributes")
      #print(RasterBrick)
      #ranger ... change back to raster for the rest of the pipeline
      #cat("\n")
      rm(New_df)
      Predicted_layer@data@attributes <- RasterBrick@data@attributes
      # Matches the spatial information to the original raster
      Predicted_layerResamp<-raster::resample(
        Predicted_layer,
        RasterBrick,
        method = "ngb")# %>% as.factor()
      
      # Restores attribute table
      Predicted_layerResamp@data@attributes <- Predicted_layer@data@attributes
      
      # # Writes out the predicted Layer
      
      layer_filename <- paste0(SubFolder,"/B_001_",basename(filename),"_Tile",1,"_PredLayer.tif")
      output_filenames <- append(output_filenames, layer_filename)
      
      writeRaster(
        Predicted_layerResamp,
        filename = layer_filename,
        overwrite = TRUE)
      
      print(paste0(" Prediction for Tile ", i, " Completed"))
      cat("\n")
      print("Saved to ")
      print(layer_filename)
      
      print("Predicted Layer")
      print(Predicted_layer)
      print("Predicted Layer After Resampling")
      print(Predicted_layerResamp)
      Predicted_layer_df
#      return(Predicted_layer_df)
#      
#    }
#  }
#  
#})



