###--------------------------------Functions Used-----------------------------------------------------#####
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
  namescolumn<-metaRemove(VI) %>%
    colnames() %>%
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

##--------test Starts---------------##
# Reads in tile as raster brick
Test_Tiles<-brick("Output/Masked_LittleLake_2018_07_31_01_09_59_132_rd_rf_or.dat/A_001_Masked_LittleLake_2018_07_31_01_09_59_132_rd_rf_or.dat_Tile_2.tif")
plot(Test_Tiles[[1]])

# Converts raster to dataframe
Ras_convert<-rasterToPoints(Test_Tiles)%>%
  as.data.frame()

if (nrow(Ras_convert) == 0){print("No data in tile")} 

if (nrow(Ras_convert) > 0) {
  
  print("Masking negative values and values greater than 2 percent reflectance")}
  
  # Removes noisey bands
  # Find out a way to Automatically remove bands based on the variance
  # remove hard coding 
  Remove_noiseybands<-Ras_convert[1:274]
  
  # Rename bandpasses
  # Change numeric reference to columns
  # Remove hard coding
  print("Renaming bandpasses")
  names(Remove_noiseybands)[-1:-2]<-Headwall_bandpasses

  # Convert weird values
  print("Converting weird values to -999")
  Remove_noiseybands[-1:-2][Remove_noiseybands[-1:-2] > 1.5] <- -999
  Remove_noiseybands[-1:-2][Remove_noiseybands[-1:-2] < 0  ] <- -999
  
  # Converts NA values
  print("Converting NA values to -999")
  Remove_noiseybands[is.na(Remove_noiseybands)] <- -999
  
  # Applies Derivative function
  print("Calculating derivatives")
  Remove_noiseybands<-Deriv_combine(Remove_noiseybands)
  
  # Convert NANs to NAs
  # NANs are being created because there's 0/0 = NAN
  Remove_noiseybands[-1:-2][sapply(Remove_noiseybands[-1:-2],is.nan)]     <-NA
  Remove_noiseybands[-1:-2][sapply(Remove_noiseybands[-1:-2],is.infinite)]<-NA
  
  # Reads in classifier
  print("Loading classifier")
  Model = get(load("Output/E_003_Best_Model_RandomForest_86vars.rda"))
  
  print("Loading varible names")
  # Vars_names<-c(Model$forest$independent.variable.names) (ranger model)
  Vars_names<-c(as.data.frame(Model$importance)%>%rownames())
  
  # Removes the X from columns with the names of banpasses
  print("Fixing Names of varibles ")
  Vars_names2<-gsub("^X","",Vars_names[1:length(Vars_names)])
  
  # Creates a new model built on important variables
  print("Selecting important varibles")
  New_df<-Remove_noiseybands%>%
    dplyr::select(x,y,all_of(Vars_names2))
  
  # Converts the results to a raster Brick and saves it on disk
  print("Converting results to a raster brick")
  RastoDF<-raster::rasterFromXYZ(New_df,
                                 crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  
  # Grabs the spitial information from original raster layer
  RasterBrick<-brick("Output/Masked_LittleLake_2018_07_31_01_09_59_132_rd_rf_or.dat/A_001_Masked_LittleLake_2018_07_31_01_09_59_132_rd_rf_or.dat_Tile_2.tif")
  
  print(paste0("Pointing Model at Tile ", i))
  
  # Predict calss of each pixel and returns a Raster layer
  Predicted_layer<-raster::predict(RastoDF,Model,na.rm = TRUE,progress='text')
  
  # Matches the spaitial information to the original raster
  Predicted_layerResamp<-raster::resample(Predicted_layer,RasterBrick,method = "ngb")%>%
    as.factor()
  
  # Restores attribute table
  Predicted_layerResamp@data@attributes <- Predicted_layer@data@attributes  
  
  plot(Predicted_layerResamp)
  