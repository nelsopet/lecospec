## Function generates Derivatives for hyperspectral libraries and Hyperspectral images
## The Function takes spectral libraries that are .csv files and Headwall datacubes 
## The function takes three arguments, see lines 4 - 6 below 
## filename = dir.path to where your datacube or Spectral library is located, e.g "path1/path2/path3/"
## out_file =  dir.path to where your results will be written, eg "path1/path2/path3/"
## Classif_Model = dir.path to where the model to be used for classification is located
## A subfolder will be created in you out_file path for aving all the output files

LandCoverEstimator<-function(filename,out_file,Classif_Model,datatype,extension){
  
  # Creates a vector of the bandpasses for the headwall sensor that will be used
  # Noisey band were omitted (only bands 1:272 below)
  # Need to find a way to decide what bands we want to leave out
  Headwall_bandpasses<-c(397.593,399.444, 401.296, 403.148, 405.000, 406.851, 408.703, 410.555, 412.407,
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
    SpeclibObj<-spectrolab::as.spectra(df)
    
    print("Resampling spectra every 5nm")
    
    # Creates functions that will do the resampling every 5nm
    final<-spectrolab::resample(SpeclibObj,seq(397.593,899.424,5))%>%
      as.data.frame()%>%
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
    AVIRIS_VI  <-hsdar::vegindex()[-58]
    Headwall_VI<-hsdar::vegindex()[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]
    
    # Get amount of cores to use
    cores <- parallel::detectCores()-1
    
    # prepare for parallel process
    c1<- parallel::makeCluster(cores)
    doParallel::registerDoParallel(c1)
    
    
    # Creates dataframe with Vegitation indices
    VI_CALC<-if(ncol(metaRemove(VI)) == 272){
      foreach(i=1:length(Headwall_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
        a<-hsdar::vegindex(spec_library,index=Headwall_VI[[i]])}
      
    } else {
      foreach(i=1:length(AVIRIS_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
        a<-hsdar::vegindex(spec_library,index=AVIRIS_VI[[i]])}
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
    
    # Reads in spectral libray as .csv
    # Right now your spectral library would have already have weird values removed/replaced
    Spectral_lib<-read.csv(filename, check.names = F)
    
    Spectral_lib<-Deriv_combine(Spectral_lib)
    
    write.csv(Spectral_lib,paste(out_file,"D_002_SpecLib_Derivs",".csv", sep=""),row.names = F)
    
    # Normalize Values here
    return(Spectral_lib)
    
    }
  
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
    c1<- parallel::makeCluster(cores)
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
          
          # Convert those rows back to NAs
          # DfofRas[rownames_NA,-1:-2] <- NA
          
          # Reads in classifier
          print("Loading classifier")
          Model = get(load(Classif_Model))
          
          # Grabs the names of the varibles
          print("Loading varible names")
          # Vars_names<-c(Model$forest$independent.variable.names) (ranger model)
          Vars_names<-c(as.data.frame(Model$importance)%>%rownames())
          
          # Removes the X from columns with the names of banpasses
          print("Fixing Names of varibles ")
          Vars_names2<-gsub("^X","",Vars_names[1:length(Vars_names)])
          
          # Creates a new model built on important variables
          print("Selecting important varibles")
          New_df<-DfofRas%>%
            dplyr::select(x,y,all_of(Vars_names2))
          
          # Converts the results to a raster Brick and saves it on disk
          print("Converting results to a raster brick")
          RastoDF<-raster::rasterFromXYZ(New_df,
                                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          
          # Deletes the dataframe
          rm(DfofRas)
          rm(New_df) 
          
          # Grabs the spitial information from original raster layer
          RasterBrick<-brick(filename)
          
          print(paste0("Pointing Model at Tile ", i))
          
          # Predict calss of each pixel and returns a Raster layer
          Predicted_layer<-raster::predict(RastoDF,Model,na.rm = TRUE,progress='text')
          cat("\n")
          
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

