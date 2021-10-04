## Function generates Derivatives for hyperspectral libraries and Hyperspectral images
## The Function takes spectral libraries that are .csv files and Headwall datacubes 
## The function takes three arguments, see lines 4 - 6 below 
## filename = dir.path to where your datacube or Spectral library is located, e.g "path1/path2/path3/"
## out_file =  dir.path to where your results will be written, eg "path1/path2/path3/"
## Classif_Model = dir.path to where the model to be used for classification is located
## A subfolder will be created in you out_file path for aving all the output files

LandCoverEstimator<-function(filename, out_file, Classif_Model, datatype = "raster", extension, output_filename = "full_model_results") {

  
  
  # Creates a vector of the bandpasses for the headwall sensor that will be used
  # Noisey band were omitted (only bands 1:272 below)
  # Need to find a way to decide what bands we want to leave out
  Headwall_bandpasses<-c(
    397.593,399.444, 401.296, 403.148, 405.000, 406.851, 408.703, 410.555, 412.407,
    414.258,416.110, 417.962, 419.814, 421.666, 423.517, 425.369, 427.221, 429.073,
    430.924,432.776, 434.628, 436.480, 438.332, 440.183, 442.035, 443.887, 445.739,
    447.590,449.442, 451.294, 453.146, 454.998, 456.849, 458.701, 460.553, 462.405,
    464.256,466.108, 467.960, 469.812, 471.664, 473.515, 475.367, 477.219, 479.071,
    480.922,482.774, 484.626, 486.478, 488.330, 490.181, 492.033, 493.885, 495.737,
    497.588,499.440, 501.292, 503.144, 504.996, 506.847, 508.699, 510.551, 512.403,
    514.254,516.106, 517.958, 519.810, 521.662, 523.513, 525.365, 527.217, 529.069,
    530.920,532.772, 534.624, 536.476, 538.328, 540.179, 542.031, 543.883, 545.735,
    547.586,549.438, 551.290, 553.142, 554.994, 556.845, 558.697, 560.549, 562.401,
    564.252,566.104, 567.956, 569.808, 571.659, 573.511, 575.363, 577.215, 579.067,
    580.918,582.770, 584.622, 586.474, 588.325, 590.177, 592.029, 593.881, 595.733,
    597.584,599.436, 601.288, 603.140, 604.991, 606.843, 608.695, 610.547, 612.399,
    614.250,616.102, 617.954, 619.806, 621.657, 623.509, 625.361, 627.213, 629.065,
    630.916,632.768, 634.620, 636.472, 638.323, 640.175, 642.027, 643.879, 645.731,
    647.582,649.434, 651.286, 653.138, 654.989, 656.841, 658.693, 660.545, 662.397,
    664.248,666.100, 667.952, 669.804, 671.655, 673.507, 675.359, 677.211, 679.063,
    680.914,682.766, 684.618, 686.470, 688.321, 690.173, 692.025, 693.877, 695.729,
    697.580,699.432, 701.284, 703.136, 704.987, 706.839, 708.691, 710.543, 712.395,
    714.246,716.098, 717.950, 719.802, 721.653, 723.505, 725.357, 727.209, 729.061,
    730.912,732.764, 734.616, 736.468, 738.319, 740.171, 742.023, 743.875, 745.726,
    747.578,749.430, 751.282, 753.134, 754.985, 756.837, 758.689, 760.541, 762.392,
    764.244,766.096, 767.948, 769.800, 771.651, 773.503, 775.355, 777.207, 779.058,
    780.910,782.762, 784.614, 786.466, 788.317, 790.169, 792.021, 793.873, 795.724,
    797.576,799.428, 801.280, 803.132, 804.983, 806.835, 808.687, 810.539, 812.390,
    814.242,816.094, 817.946, 819.798, 821.649, 823.501, 825.353, 827.205, 829.056,
    830.908,832.760, 834.612, 836.464, 838.315, 840.167, 842.019, 843.871, 845.722,
    847.574,849.426, 851.278, 853.130, 854.981, 856.833, 858.685, 860.537, 862.388,
    864.240,866.092, 867.944, 869.796, 871.647, 873.499, 875.351, 877.203, 879.054,
    880.906,882.758, 884.610, 886.462, 888.313, 890.165, 892.017, 893.869, 895.720,
    897.572,899.424)

            unregister_dopar <- function() {
              env <- foreach:::.foreachGlobals
              rm(list=ls(name=env), pos=env)
            }


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
              print("Imputing...")
            #missForest::missForest(df, maxiter = 1, parallelize = "trees")
            
            df <- useful::simple.impute(df)
            return(df)
            } else {
              print("Conversion to dataframe failed")
            }
            

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

              print("Imputing...")
              df <- useful::simple.impute(df)
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
  
  #--------------------------- Functions For Derivative Calculations ------------------------
  # Function responsible for resampling
  Func_Resamp<-function(Resamp){
    
    # Removes metadata before function can be applied
    df<-metaRemove(Resamp)
    
    # Converts the dataframe to a spectral object
    SpeclibObj<-spectrolab::as_spectra(df)
    
    print("Resampling spectra every 5nm")
    
    # Creates functions that will do the resampling every 5nm
    final<-spectrolab::resample(SpeclibObj,seq(397.593,899.424,5))%>%
      as.data.frame() %>%
      dplyr::select(-sample_name)
    
    # Rename columns
    colnames(final)<-paste(colnames(final),"5nm",sep = "_")
    
    # Combines all the dataframes created into one df
    ResampledDF<-cbind(bandsRemove(Resamp),final)
    
    print("Resampling sucessful")
    
    return(ResampledDF)} # Func_Resamp ends
  
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
    base_index <- hsdar::vegindex()
    AVIRIS_VI  <-base_index [-58]
    Headwall_VI<-base_index [-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]
    

    
    # Creates dataframe with Vegitation indices
    VI_CALC<-if(ncol(metaRemove(VI)) == 272){
      foreach(i=1:length(Headwall_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
        a<-hsdar::vegindex(spec_library, index=Headwall_VI[[i]], weighted=FALSE)}
      
    } else {
      foreach(i=1:length(AVIRIS_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
        a<-hsdar::vegindex(spec_library, index=AVIRIS_VI[[i]], weighted = FALSE)}
    }
    

    
    # Converts Matrix to a datframe 
    VI_CALC <- as.data.frame(VI_CALC)
    
    # Function Renames columns
    names(VI_CALC) <- Headwall_VI
    #if(ncol(VI_CALC) == 95){
    #  names(VI_CALC) <- Headwall_VI
    #} else {
    #  names(VI_CALC) <- AVIRIS_VI}
    
    # Function removes spaces and special charcters from column names
    # Models will not run if these aren't removed
    names(VI_CALC)<-str_remove_all(names(VI_CALC),"[[:punct:]]| ")
    
    # Conbines VIs and Lat/long info
    VI_DF<-cbind(bandsRemove(VI),VI_CALC)
    
    print("Vegitation index calculations successful")
    
    return(VI_DF)
  } # Func_VI ends
  
  # Function Combines both Derivitive that are calculated
  Deriv_combine<- function(x){
    
    # Resampling Dataset
    Resampled_data<-Func_Resamp(x)
    
    # Calculating VIs for Dataset
    VegIndex_data<-Func_VI(x)
    
    DF<-cbind(VegIndex_data,metaRemove(Resampled_data))
    
    return(DF)
  } # Deriv_combine ends
  
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
    num_tiles <- 50# make 30 for production; reduced for faster testing
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
          New_df<-DfofRas%>%
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
          out<-str(Predicted_layer_df)
          print(out)
          Predicted_layer <- Predicted_layer_df
      #Predicted_layer <- raster::rasterFromXYZ(
      #  Predicted_layer_df,
      #  crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          
          #Predicted_layer <-
          # Set extent of predicted to be the same as the Data Brick
          print("Fixing Extents")
      #raster::extent(Predicted_layer) <- raster::extent(RasterBrick)

          
          #print("Raster Brick Attributes")
          #print(RasterBrick)
          #ranger ... change back to raster for the rest of the pipeline
          #cat("\n")
          rm(New_df)
      #Predicted_layer@data@attributes <- RasterBrick@data@attributes
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


      plot_agg_results <- function(df, save_file = "Output/results.jpeg") {

#    create_raster_image <- function(df, base_image = NULL) {
#      speclib <- raster(df)
#      #speclib@data@attributes <- base_image@data@attributes
#      png("model_output.png")
#      raster::image(speclib)
#      dev.off()
#    }
#
#      plot_agg_results <- function(df, save_file = "Output/results.jpeg") {
        my_plot <- ggplot2::ggplot(data = df) +
          geom_point(aes(df$x, df$y, color=df$z))
        print(my_plot)
        #ggplot2::ggsave(save_file)
        return(my_plot)
      }

      print("Plotting Results")
      
      plot_agg_results(output_df)
      print("Done")
      write.table(output_df, file = paste0("Output/", output_filename, ".csv")

      output_raster <- raster::rasterFromXYZ(output_df)
      raster::writeRaster(output_raster, paste0("Output/", output_filename, ".tif")

    return(output_raster)
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