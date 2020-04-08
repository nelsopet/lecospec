## Function generates Derivatives for hyperspectral libraries and Hyperspectral images
## The Function takes spectral libraries that are .csv files and Headwall datacubes 
## The function takes three arguments, see lines 5 and 6 below 
## filename = dir.path to where your datacube or Spectral library is located, e.g "path1/path2/path3/"
## Classif_Model = dir.path to where the model to be used for classification is located
## out_file =  dir.path to where your results will be written, eg "path1/path2/path3/"
## A subfolder will be created in you out_file path for the respective input file
# Classif_Model
HyperSpec_DerivGenerator<-function(filename,out_file,Classif_Model){
  
  print("calling Packages")
  
  # Call packages to be used
  library(spectrolab)
  library(tidyverse)
  library(raster)
  library(SpaDES)
  library(doParallel)
  library(parallel)
  library(hsdar)
  library(randomcoloR)
  
  # Creates a vector of the bandpasses for the headwall sensor that will be used
  # Noisey band were omitted (only bands 1:272 below)
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
 
  ###### Functions For Derivative Calculations #######
  # Function responsible for resampling
  Func_Resamp<-function(Resamp){
    
    # Removes metadata before function can be applied
    df<-metaRemove(Resamp)
    
    # Converts the dataframe to a spectral object
    SpeclibObj<-as.spectra(df)
    
    # Creates functions that will do the resampling
    Fun1<-function(x) spectrolab::resample(x,seq(397.593,899.424,5))
    Fun2<-function(x) spectrolab::resample(x,seq(397.593,899.424,10))
    Fun3<-function(x) spectrolab::resample(x,seq(397.593,899.424,50))
    Fun4<-function(x) spectrolab::resample(x,seq(397.593,899.424,100))
    
    # Creates a list of functions above
    ResampledArgs<-list(
      wvl_005nm=function(x) Fun1(x),
      wvl_010nm=function(x) Fun2(x),
      wvl_050nm=function(x) Fun3(x),
      wvl_100nm=function(x) Fun4(x)
    )
    
    # Make sure the number of cores is equal to the objects in the list created
    cores = detectCores()-1
    if(cores>length(ResampledArgs)){
      cores<-length(ResampledArgs)
    }
    
    #Creates cluster
    cl = makeCluster(cores)
    
    # Pass objects to each core in use
    clusterExport(cl, varlist=c("Fun1","Fun2","Fun3","Fun4","SpeclibObj"),envir=environment())
    
    print("Resampling spectra every 5nm, 10nm, 50nm and 100nm")
    
    # Runs resampling functions in parallel
    ResampledObjs<- parLapply(cl,ResampledArgs,function(f) 
      f(SpeclibObj)
    )
    
    #Stops cluster
    stopCluster(cl)
    
    # Converts results to a dataframe
    final<-lapply(ResampledObjs,function(x){
      as.data.frame(x)%>%
        dplyr::select(-sample_name)})
    
    #Adds suffix to the end of each column name
    for (i in 1:length(final)){
      colnames(final[[i]])<-paste(colnames(final[[i]]),names(final[i]),sep = "_")
    }
    
    # Combines all the dataframes created into one df
    ResampledDF<-cbind(bandsRemove(Resamp),do.call("cbind",final))
    
    # wvl_010nm.|wvl_050nm.|wvl_100nm.
    # Renames columns 
    colnames(ResampledDF)<-str_remove_all(names(ResampledDF),"wvl_005nm.|wvl_010nm.|wvl_050nm.|wvl_100nm.")
    
    if(ncol(bandsRemove(ResampledDF)) == 2){
      
      # Converts rows with negative values or values >2 to 0
      # This is for hyperspec Image
      #ResampledDF[-1:-2][ResampledDF[-1:-2] < 0] <- 0
      #ResampledDF[-1:-2][ResampledDF[-1:-2] > 2] <- 0
      
    } else {
      
      # Removes rows with negative values or values >2
      # This is for your spectral library/ground truth data
      ResampledDF%>%
        filter_at(vars(c(colnames(metaRemove(ResampledDF)))), all_vars(.  <2))%>%
        filter_at(vars(c(colnames(metaRemove(ResampledDF)))), all_vars(. >=0))
    }
    
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
    spec_library<- speclib(matrix_a,namescolumn)
    
    # creates a vectror of names of all the vegitation indices
    AVIRIS_VI  <-vegindex()[-58]
    Headwall_VI<-vegindex()[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]
    
    # Get amount of cores to use
    cores <- detectCores()-1
    
    # prepare for parallel process
    c1<- makeCluster(cores)
    registerDoParallel(c1)
    
    
    # Creates dataframe with Vegitation indices
    VI_CALC<-if(ncol(metaRemove(VI)) == 272){
      foreach(i=1:length(Headwall_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
        a<-vegindex(spec_library,index=Headwall_VI[[i]])}
      
    } else {
      foreach(i=1:length(AVIRIS_VI), .combine=cbind, .packages = 'hsdar') %dopar%{
        a<-vegindex(spec_library,index=AVIRIS_VI[[i]])}
    }
    
    # Stops cluster
    stopCluster(c1)
    
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
    
    # converts NAs and Infs to 0s 
    #is.na(VI_DF)<-sapply(VI_DF, is.infinite)
    #VI_DF[is.na(VI_DF)]<-0
    
    print("Vegitation index calculations successful")
    
    return(VI_DF)
  } # Func_VI ends
  
  # Function Combines both Derivitive that are calculated
  Deriv_combine<- function(x){

    # Resampling Dataset
    Resampled_data<-Func_Resamp(x)
    
    # Calculating VIs for Dataset
    VegIndex_data<-Func_VI(x)
    
    return(cbind(VegIndex_data,metaRemove(Resampled_data)))
  } # Deriv_combine ends
  
  ###### Functions For Derivative Calculations above #######
  
  # Creates SubFolder
  SubFolder<-(paste(out_file,basename(filename),sep=""))
  dir.create(SubFolder)
  
  # Function Reads in the data and replace/removes weird values
  if(grepl(".csv",filename) == TRUE){

      # Reads in spectral libray as .csv
      # Right now your spectral library would have already have weird values removed/replaced
      Spectral_lib<-read.csv(filename, check.names = F)
      
      Spectral_lib<-Deriv_combine(Spectral_lib)
      
      write.csv(Spectral_lib,paste(out_file,"SpecLib_Derivs",".csv", sep=""),row.names = F)
      
      # Normalize Values here
      return(Spectral_lib)
      
    } else {
      
      print("Importing Datacube")
      
      # Reads in the Hyperspectral datacubes as a Rasterstack raster
      # Only 272 Layers/bands
      # This takes a while, might be able to parallelize or ignore completely
      # Compare The time it takes using a Raster Stack instead of a RaterBrick
      Converted_Dcube<-brick(filename)
      
      
      print("Masking negative values and values greater than 2 percent reflectance")
      
      # Mask out weird values
      # Need to check if this is being done right
      # Need to parralellize this
      #Converted_Dcube[Converted_Dcube > 0.2 | Converted_Dcube[[i]] < 0 ]<-NA
      
      # You could normalize Values here
      # what should I use 
      # Min-max (does not cope well with outlies) scaling or 
      # standardization/z-score normalization (Normalized data may be on different scales)
      # This might help with memory/speed up raster calculations
      
      print("Masking Completed")
      
      # Lets Split the raster into tiles by cropping
      # Should I put the number of tiles as a parameter in my function??
      # I also need to come up with a more efficient approach to splitting the raster
      
      print("Splitting raster into 24 tiles")
      
      # Creates x tiles 
      Tiles<-splitRaster(Converted_Dcube[[1]],24)
      
      # prepare for parallel process
      # Need a more efficient approach here
      # This currently takes 13 minutes on 6 cores for whole datacube
      cores<-parallel::detectCores()-1
      
      if(cores < detectCores()-1){
        cores<-1
      }
      
      ## Start cluster
      c1<- makeCluster(cores)
      registerDoParallel(c1)
      clusterEvalQ(c1,library(raster))
      
      parLapply(c1,1:length(Tiles),function(x){
        
        # CHANGE .ENVI to .TIF/ OTHER IMAGE EXTENSIONS
        Out_tif = paste(SubFolder,"/A_001_",basename(filename),"_Tile_",x,".envi",sep = "")
        
        # Statement checks if file already exist
        if(!file.exists(Out_tif)){
          
        # Crops raster based on the extent of the 24 tiles that we created
        raster::crop(Converted_Dcube,extent(Tiles[[x]]),
             filename = paste(SubFolder,"/A_001_",basename(filename),"_Tile_",x,".envi",sep = ""),
             dataType = "INT4S",overwrite = T)
        }
      })
      
      # Stops cluster
      stopCluster(c1)
      
      print("Tiles were sucessfully created")
      
      print("Calculating Derivatives")
      
      # Creates a list of the names of all the tiles created 
      list_of_Tiles<-list.files(SubFolder,
                                pattern = glob2rx("A_001*.envi"),
                                full.names = T)
      
      # Now Lets calculate derivatives for each tile
      # Create an empty loop
      list_built<-list()
      
      # Iterate through the list using lapply
      lapply(1:length(list_of_Tiles), function(i){
        
        out_tif2 = paste0(list_of_Tiles[[i]] %>% 
                            gsub("A_001", "B_001", .))%>%
                            gsub(".envi", ".csv" , .)
                          
        
        # Statement checks if file already exist
        if(!file.exists(out_tif2)){
    
        # Reads in the tiles and converts it to a dataframe
        DfofRas<-brick(list_of_Tiles[[i]])%>%
          rasterToPoints()%>%
          as.data.frame()
        
        # Removes noisey bands
        # Automatically remove bands based on the variance
        DfofRas<-DfofRas[1:274]
        
        # Rename bandpasses
        # Change numeric reference to columns
        names(DfofRas)[-1:-2]<-Headwall_bandpasses
        
        # Converts NA values
        DfofRas[is.na(DfofRas)] <- -999
        
        # Applies Derivative function
        DfofRas<-Deriv_combine(DfofRas)
        
        # Converts the results to a raster Brick and saves it on disk
        # RastoDF<-raster::brick(DfofRas)
        
        # Write raster out as a brick
        # writeRaster(DfofRas,filename = out_tif2,
        #    format = "ENVI",overwrite = T)
        
        # Writes each out as .csv files
        write.csv(DfofRas,out_tif2,row.names = F)
        
        }
        
      })
  
  print("Derivatives for all 24 tiles were created and saved as .envi file")
  
# ------------------------------------------------------Prediction Portion being updated ----------------------------
  
  ## Applies Random forest model to each Tile created
  ## Calls the classifier
  #Model = load(Classif_Model)
  #
  ## Creates a list of the names of all the tiles created 
  #name_list_DerivativeDF<-list.files(SubFolder,
  #                                    pattern = (".csv"),
  #                                    full.names = T)
  #
  #print("Generating Predicted Raster Layer")
  #
  #if(is.null(Classif_Model)){
  #  print("Classification Model missing")
  #}
  #
  ### Grabs the x,y values from the original Bricked raster 
  ### This needs to be done in order to maintain a regular raster
  ### Imports raster Brick
  #
  #Regular_Brick<-brick(filename)
  #
  ## Extracts x, y values
  #Raster_Values<-Regular_Brick%>%
  #  xyFromCell(1:ncell(Regular_Brick))%>%
  #  as.data.frame()
  #
  ## Cobmbines the list of predicted layers
  #Results_df<-do.call("rbind",
  #                    
  #                    # Does an iteration for each Tile
  #                    lapply(1:length(name_list_DerivativeDF),function(i){
  #                      
  #                      # Reads in the tiles and the derivaties calculated for each
  #                      Tile_Derivs<-read.csv(name_list_DerivativeDF[[i]])
  #                      
  #                      # Applies prediction
  #                        Tile_Prediction<-predict(get(Model),Tile_Derivs)
  #                      
  #                      # Converts Results to a dataframe
  #                      Results_df<-as.data.frame(Tile_Prediction)%>%
  #                        'names<-'("Predicted")
  #                    }))
  #  
  #  # Creates Unique IDs for classes/categories 
  #  Unique_class<-unique(as.data.frame(Results_df$Predicted)) 
  #  Unique_class$Category<-seq(1:nrow(Unique_class))
  #  names(Unique_class)[1]<-"Predicted"
  #  
  #  # Create dataframe with unique class IDs and location info
  #  Results_final<-merge(Results_df,
  #                       Unique_class, 
  #                       by="Predicted")
  #  
  #  Final<-cbind(Raster_Values[1:2],Results_final)%>%
  #    dplyr::select(x,y,Category,Predicted)
  #  
  #  classesinimage<-semi_join(Unique_class,Final,by = "Category")
  #  
  #  # convert to a raster 
  #  Raster<-rasterFromXYZ(Final[-4],crs = crs(Regular_Brick))
  #  
  #  # Creates a distinct color pallet
  #  chm_colors<-randomcoloR::distinctColorPalette(k=nrow(classesinimage))
  #  
  #  # Saves plot as a jpeg
  #  jpeg(paste(SubFolder,"/C_001_",basename(filename),"PredLayer",".jpeg",sep = ""),width=1200, height=700)
  #  plot(
  #    Raster,
  #    legend = FALSE,
  #    axes=FALSE,
  #    col = chm_colors[-8],
  #    box= FALSE,
  #    xlab="Longitude", 
  #    ylab="Latitude"
  #  )
  #  
  #  legend(
  #    "topleft",
  #    legend = c(as.character(classesinimage$Predicted)),
  #    fill =chm_colors,
  #    border = FALSE,
  #    bty = "n",
  #    cex=1.3,
  #    xjust =1,
  #    horiz = FALSE,
  #    inset = -0.009,
  #    par(cex=0.4)
  #    
  #  )             
  #  dev.off()
  #  
  #  print("Predicted Raster Layer Created")
# ---------------------------------------------------------------------------------------------------------------------
    
  
    }   
 
}
