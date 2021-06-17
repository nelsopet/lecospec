#
#New image test
#The scripts below run with the small test dataset but not a full datacube
source("Scripts/1_Species_dataframe.R")
source("Scripts/2_DataMunging.R")
source("Scripts/3_Create_SpecLibPSR.R")
source("Scripts/4_Model_Development.R")
source("Scripts/5_Classify_Image1.R") #Change which datacube is being used in this script


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
filename = "Output/SubsetDatacube/A_001_SubsetDatacube_Tile_1.tif"
#list.files(filename)
#filename = "Data/SubsetDatacube"
#filename = "M:/Alaksa_Datacubes/Raw_files/WickershamDome_2019_08_08_19_31_51_2000_rd_rf_or"
#out_file = "M:/Alaksa_Datacubes/Predictions/NEW/"
out_file = "Output/"
extension = FALSE
datatype = "raster"


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

# Function Reads in the data and replace/removes weird values
if(datatype == "csv"){
  print("CSV File Detected")
  
  # Reads in spectral libray as .csv
  # Right now your spectral library would have already have weird values removed/replaced
  Spectral_lib<-read.csv(filename, check.names = F)
  
  Spectral_lib<-Deriv_combine(Spectral_lib)
  
  write.csv(Spectral_lib,paste(out_file,"D_002_SpecLib_Derivs",".csv", sep=""),row.names = F)
  
  # Normalize Values here
  return(Spectral_lib)
  
}

if(datatype == "raster"){
  print("Raster File Detected")
  
  print("Importing Datacube")
  
  # Reads in the Hyperspectral datacubes as a Rasterstack raster
  Converted_Dcube <- raster::brick(filename)
  
  print("Splitting raster into 30 tiles")
  
  # Creates 30 tiles 
  num_tiles <- 100# make 30 for production; reduced for faster testing
  Tiles <- SpaDES.tools::splitRaster(Converted_Dcube[[1]], num_tiles)
  
  
  # define some helper functions here
  remove_meta_column <- function(x) {
    meta <- c(grep("^[0-9][0-9][0-9]", colnames(x)))
    colremove <- x[, meta]
    return(colremove)
  }
  remove_band_column <- function(x) {   
    meta <- c(grep("[a-z A-Z]", colnames(x)))
    colremove <- x[, meta]
    return(colremove)
  }
  
  extract_bands <- function(df){
    bands <- remove_band_column(df) %>%
      colnames() %>%
      as.numeric()
    return(bands)
  }
  
  
  #' Converts Data Frame to a spaectral library
  df_to_speclib <- function(df) {
    # Convert to a spectral library
    df_no_metadata <- remove_meta_column(df)
    spectral_matrix <- as.matrix(df_no_metadata)
    bands <- extract_bands(df)
    spectral_lib <- hsdar::speclib(spectral_matrix, bands)
    return(spectral_lib)
  }
  
  # Use 4 cores
  cores <- parallel::detectCores() - 1
  
  print(paste0(cores, " cores being used"))
  
  ## Start cluster
  # parallel infrastructure
  
  # Get amount of cores to use
  cores <- parallel::detectCores()-1
  
  # prepare for parallel process
  c1 <- parallel::makeCluster(cores, setup_timeout = 0.5)
  doParallel::registerDoParallel(c1)
  parallel::clusterEvalQ(c1, library(raster))
  
  # imputing
  
  
  parallel::parLapply(c1,1:length(Tiles),function(x){
    
    # Creates outfile
    Out_tif = paste(SubFolder,"/A_001_",basename_F,"_Tile_",x,".tif",sep = "")
    # Statement checks if file already exist
    if(!file.exists(Out_tif)) {
      
      # Crops raster based on the extent of the 24 tiles that we created
      raster::crop(Converted_Dcube,extent(Tiles[[x]]),
                   filename = Out_tif,
                   dataType = "INT4S",format="GTiff",overwrite = T)
      
    }
    print("30 Tiles were sucessfully created")
  })#end lapply
  
  # Stops cluster
  unregister_dopar()
  parallel::stopCluster(c1)
  
  cat("\n")
  print("Creating Predicted Layer")
  
  # Creates a list of the names of all the tiles created 
  print("Creating a list of the names of all the tiles created")
  list_of_Tiles<-list.files(SubFolder,
                            pattern = glob2rx("A_001*.tif"),
                            full.names = T)
  
  
  
  # Iterate through the list using lapply
  # 1:length(list_of_Tiles) change to 1:2 for testing
  
  #-------------------------List_of_Predlauers----------------------$
  
  List_of_PredLayers<-lapply(1:length(list_of_Tiles), function(i) {
    
    
    # Creates the name of the output file and the location in which its stored
    out_tif2 = paste0(SubFolder, "/",basename_F,"_PredLayer1.tif")
    
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
        
        DfofRas <- impute_spectra(DfofRas)
        
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
        New_df<-DfofRas%>% colnames()
          dplyr::select(x,y,all_of(Vars_names2))
        
        #print("Imputing spectra")
        #cleaned_df <- impute_spectra(New_df)
        
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
        
        #colnames(RastoDF) <- colnames(New_df)[-1:-2]
        
        # Deletes the dataframe
        rm(DfofRas)
        #rm(New_df) 
        
        # Grabs the spatial information from original raster layer <- no it doesn't.  This loads the data
        RasterBrick <- raster::brick(filename)
        
        
        
        print(paste0("Pointing Model at Tile ", i))
        
        # function to fill in missing values using partial mean matching
        
        
        
        #tile_speclib <- df_to_speclib(cleaned_RastoDF)
        #print("DF Before Imputing")
        #print(RastoDF)
        #print("DF After Imputing")
        #print(cleaned_RastoDF)
        
        #cleaned_RastoDF <- na.roughfix(RastoDF)# KRB
        #print(paste0(summary(cleaned_RastoDF)))
        
        # Predict calss of each pixel and returns a Raster layer
        print("Running Prediction")
        #Predicted_layer<-raster::predict(RastoDF,Model,na.rm = TRUE,progress='text')
        #Predicted_layer_df<-raster::predict(RastoDF,Model,progress='text') #randomForest 
        
        
        
        Predicted_layer_df<-raster::predict(RastoDF,
                                            Model,
                                            na.rm = TRUE,
                                            type='response',
                                            progress='text',
                                            fun = function(Model, ...) predict(Model, ...)$predictions) #(Ranger model)
        #Predicted_layer <- predict(Model, New_df, type="response")
        #randomForest Model
        
        #write.csv(Predicted_layer_df,paste0(SubFolder,"/B_001_",basename(filename),"_Tile",i,"_PredLayer.csv"))
        
        print("Prediction complete")
        
        Predicted_layer <- raster::rasterFromXYZ(
          Predicted_layer_df,
          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
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
        
        layer_filename <- paste0(SubFolder,"/B_001_",basename(filename),"_Tile",i,"_PredLayer.tif")
        #output_filenames <- append(output_filenames, layer_filename)
        
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
        
        return(Predicted_layer_df)
        
      }
    }
    
  })
  
  crop_layer_extents <- function(list_of_rasters, base_map){
    cropped_rasters <- sapply(
      list_of_rasters,
      function(x){
        y <- raster::resample(x, raster::crop(base_map, x))
        return(y)
      },
      simplify = FALSE
    )
    sapply(list_of_rasters, function(x){
      print(length(x))
    })
    return(cropped_rasters)
  }
  
  aggregate_results_df <- function(predictions, x, y) {
    
    dfx <- data.frame(x)
    colnames(dfx) <- c("x")
    dfxy <- cbind(dfx, y)
    z <- c()
    
    for (my_prediction in predictions) {
      prediction_df <- as.data.frame(
        raster::levels(my_prediction)[[1]],
        xy=TRUE)
      num_pixels <- nrow(prediction_df)
      print("Number of pixels in prediction")
      print(num_pixels)
      for (iter_idx in seq_len(num_pixels)) {
        #print("Match Found while aggregating")
        if (!is.na(prediction_df[iter_idx, 2])) {
          z <- append(z, prediction_df[iter_idx, 2])
        }
      }
    }
    print("vector z")
    print(z)
    
    if (length(z) > nrow(dfxy)) {
      z <- z[1:nrow(dfxy)]
    } else if (length(z) < nrow(dfxy)) {
      dfxy <- head(dfxy, n = length(z))
    } 
    df <- cbind(dfxy, z)
    return(df)
  }
  save_path <- "Data/base_df.csv"
  saved_df <- read.csv2(save_path, header = TRUE, sep=",")
  print("Loaded saved data")
  print("Beginning Aggregation")
  output_df <- aggregate_results_df(
    List_of_PredLayers,
    saved_df$x,
    saved_df$y)
  print("Aggregated Output")
  print(summary(output_df))
  # combines the tiles and writes the output to disk
  print("Combining Tiles")
  #RasterBrick <- raster::brick(filename)
  #list_of_predicted_layers <- crop_layer_extents(List_of_PredLayers, RasterBrick)
  #Predicted_Layer <- do.call(raster::merge, List_of_PredLayers)
  
  
  
  ## To Fix: replace NAs with something reasonable -> KNN Imputer to replace them? 
  
  preload_files <- function(list_of_files) {
    num_files <- length(list_of_files)
    output_files <- list()
    for(idx in 1:num_files){
      append(output_files, system.file(list_of_files[idx], package = "gdalUtils"))
    }
    return(output_files)
  }
  
  
  print("File Locations for Testing")
  print(List_of_PredLayers)
  print("Number of Files")
  print(length(List_of_PredLayers))
  
  
  #quantize_classes <- function(z) {
  #  df <- read.csv2("Data/quantkey.csv")
  #  quantized_z <- c()
  #  for(z_level in z) {
  #    for (row in seq_len(nrow(df))) {
  #      if(df[row, 2] == z_level) {
  #        append(quantized_z, row)
  #      }
  #    }
  #  }
  #  return(quantized_z)
  #}
  #print("Quantizing Output Levels")
  #quantized_levels <- quantize_classes(output_df$z)
  #output_df$levels <- quantized_levels
  
  
  
  
  
  #predicted_output_file <- file.path(SubFolder,paste0("O_001_",basename(filename),"_combined_PredLayer.tif"))
  #predicted_layer <- gdalUtils::mosaic_rasters(List_of_PredLayers, predicted_output_file, output_Raster = TRUE)
  #print("Converting to Raster")
  #Predicted_Layer <- raster::brick(output_df)
  #print("Mosaic Creation Completed")
  print("Data Frame")
  print(base::summary(output_df))
  print("Raster Object")
  #print(Predicted_Layer)
  # Restres attribute table
  #print(paste0("Restoring Attribute Scale"))
  #Predicted_Layer@data@attributes <- List_of_PredLayers[1][[1]]@data@attributes
  
  #print(paste0("Saving data to File"))
  #writeRaster(
  #  Predicted_Layer,
  #  filename = paste0(
  #    SubFolder,
  #    "/",
  #    basename(filename),
  #    "_PredLayer.tif"),
  #  overwrite = T)
  
  #<<<<<<< Updated upstream
  plot_agg_results <- function(df, save_file = "Outputs/results.jpeg") {
    #=======
    #    create_raster_image <- function(df, base_image = NULL) {
    #      speclib <- raster(df)
    #      #speclib@data@attributes <- base_image@data@attributes
    #      png("model_output.png")
    #      raster::image(speclib)
    #      dev.off()
    #    }
    #
    #      plot_agg_results <- function(df, save_file = "Output/results.jpeg") {
    #>>>>>>> Stashed changes
    my_plot <- ggplot2::ggplot(data = df) +
      geom_point(aes(df$x, df$y, color=df$z))
    print(my_plot)
    #ggplot2::ggsave(save_file)
    return(my_plot)
  }
  
  print("Plotting Results")
  
  plot_agg_results(output_df)
  print("Done")
  write.table(output_df, file = "Outputs/full_model_results.csv")
  
  return(output_df)
}
}

# Function Reads in the data and replace/removes weird values
Make_Speclib_Derivs<- function(filename)  {  
  # Reads in spectral libray as .csv
  # Right now your spectral library would have already have weird values removed/replaced
  Spectral_lib<-read.csv(filename, check.names = F)
  
  Spectral_lib<-Deriv_combine(Spectral_lib)
  
  write.csv(Spectral_lib,paste(out_file,"D_002_SpecLib_Derivs",".csv", sep=""),row.names = F)
  
  # Normalize Values here
  return(Spectral_lib)
}


