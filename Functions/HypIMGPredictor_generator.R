# Function generates predictors for hyperspectral images

ImagePredictor_generator<-function(OBJECT){
  
  # Creates a vector of the bandpasses for the headwall sensor that will be used
  # Noisey band were omitted (only bands 1:272 below)
  Headwall_bandpasses<-c(397.593
                          ,399.444
                          ,401.296
                          ,403.148
                          ,405
                          ,406.851
                          ,408.703
                          ,410.555
                          ,412.407
                          ,414.258
                          ,416.11
                          ,417.962
                          ,419.814
                          ,421.666
                          ,423.517
                          ,425.369
                          ,427.221
                          ,429.073
                          ,430.924
                          ,432.776
                          ,434.628
                          ,436.48
                          ,438.332
                          ,440.183
                          ,442.035
                          ,443.887
                          ,445.739
                          ,447.59
                          ,449.442
                          ,451.294
                          ,453.146
                          ,454.998
                          ,456.849
                          ,458.701
                          ,460.553
                          ,462.405
                          ,464.256
                          ,466.108
                          ,467.96
                          ,469.812
                          ,471.664
                          ,473.515
                          ,475.367
                          ,477.219
                          ,479.071
                          ,480.922
                          ,482.774
                          ,484.626
                          ,486.478
                          ,488.33
                          ,490.181
                          ,492.033
                          ,493.885
                          ,495.737
                          ,497.588
                          ,499.44
                          ,501.292
                          ,503.144
                          ,504.996
                          ,506.847
                          ,508.699
                          ,510.551
                          ,512.403
                          ,514.254
                          ,516.106
                          ,517.958
                          ,519.81
                          ,521.662
                          ,523.513
                          ,525.365
                          ,527.217
                          ,529.069
                          ,530.92
                          ,532.772
                          ,534.624
                          ,536.476
                          ,538.328
                          ,540.179
                          ,542.031
                          ,543.883
                          ,545.735
                          ,547.586
                          ,549.438
                          ,551.29
                          ,553.142
                          ,554.994
                          ,556.845
                          ,558.697
                          ,560.549
                          ,562.401
                          ,564.252
                          ,566.104
                          ,567.956
                          ,569.808
                          ,571.659
                          ,573.511
                          ,575.363
                          ,577.215
                          ,579.067
                          ,580.918
                          ,582.77
                          ,584.622
                          ,586.474
                          ,588.325
                          ,590.177
                          ,592.029
                          ,593.881
                          ,595.733
                          ,597.584
                          ,599.436
                          ,601.288
                          ,603.14
                          ,604.991
                          ,606.843
                          ,608.695
                          ,610.547
                          ,612.399
                          ,614.25
                          ,616.102
                          ,617.954
                          ,619.806
                          ,621.657
                          ,623.509
                          ,625.361
                          ,627.213
                          ,629.065
                          ,630.916
                          ,632.768
                          ,634.62
                          ,636.472
                          ,638.323
                          ,640.175
                          ,642.027
                          ,643.879
                          ,645.731
                          ,647.582
                          ,649.434
                          ,651.286
                          ,653.138
                          ,654.989
                          ,656.841
                          ,658.693
                          ,660.545
                          ,662.397
                          ,664.248
                          ,666.1
                          ,667.952
                          ,669.804
                          ,671.655
                          ,673.507
                          ,675.359
                          ,677.211
                          ,679.063
                          ,680.914
                          ,682.766
                          ,684.618
                          ,686.47
                          ,688.321
                          ,690.173
                          ,692.025
                          ,693.877
                          ,695.729
                          ,697.58
                          ,699.432
                          ,701.284
                          ,703.136
                          ,704.987
                          ,706.839
                          ,708.691
                          ,710.543
                          ,712.395
                          ,714.246
                          ,716.098
                          ,717.95
                          ,719.802
                          ,721.653
                          ,723.505
                          ,725.357
                          ,727.209
                          ,729.061
                          ,730.912
                          ,732.764
                          ,734.616
                          ,736.468
                          ,738.319
                          ,740.171
                          ,742.023
                          ,743.875
                          ,745.726
                          ,747.578
                          ,749.43
                          ,751.282
                          ,753.134
                          ,754.985
                          ,756.837
                          ,758.689
                          ,760.541
                          ,762.392
                          ,764.244
                          ,766.096
                          ,767.948
                          ,769.8
                          ,771.651
                          ,773.503
                          ,775.355
                          ,777.207
                          ,779.058
                          ,780.91
                          ,782.762
                          ,784.614
                          ,786.466
                          ,788.317
                          ,790.169
                          ,792.021
                          ,793.873
                          ,795.724
                          ,797.576
                          ,799.428
                          ,801.28
                          ,803.132
                          ,804.983
                          ,806.835
                          ,808.687
                          ,810.539
                          ,812.39
                          ,814.242
                          ,816.094
                          ,817.946
                          ,819.798
                          ,821.649
                          ,823.501
                          ,825.353
                          ,827.205
                          ,829.056
                          ,830.908
                          ,832.76
                          ,834.612
                          ,836.464
                          ,838.315
                          ,840.167
                          ,842.019
                          ,843.871
                          ,845.722
                          ,847.574
                          ,849.426
                          ,851.278
                          ,853.13
                          ,854.981
                          ,856.833
                          ,858.685
                          ,860.537
                          ,862.388
                          ,864.24
                          ,866.092
                          ,867.944
                          ,869.796
                          ,871.647
                          ,873.499
                          ,875.351
                          ,877.203
                          ,879.054
                          ,880.906
                          ,882.758
                          ,884.61
                          ,886.462
                          ,888.313
                          ,890.165
                          ,892.017
                          ,893.869
                          ,895.72
                          ,897.572
                          ,899.424)
  
  # Creates a vector of the bandpasses for the Aviris sensor that will be used
  AVIRIS_bandpasses  <-c(381.870000,  386.880000,  391.890000,  396.890000,  401.900000,  406.910000,
                         411.920000,  416.930000,  421.940000,  426.950000,  431.960000,  436.960000,
                         441.970000,  446.980000,  451.990000,  457.000000,  462.010000,  467.020000,
                         472.020000,  477.030000,  482.040000,  487.050000,  492.060000,  497.070000,
                         502.080000,  507.090000,  512.090000,  517.100000,  522.110000,  527.120000,
                         532.130000,  537.140000,  542.150000,  547.150000,  552.160000,  557.170000,
                         562.180000,  567.190000,  572.200000,  577.210000,  582.220000,  587.220000,
                         592.230000,  597.240000,  602.250000,  607.260000,  612.270000,  617.280000,
                         622.280000,  627.290000,  632.300000,  637.310000,  642.320000,  647.330000,
                         652.340000,  657.350000,  662.350000,  667.360000,  672.370000,  677.380000,
                         682.390000,  687.400000,  692.410000,  697.410000,  702.420000,  707.430000,
                         712.440000,  717.450000,  722.460000,  727.470000,  732.480000,  737.480000,
                         742.490000,  747.500000,  752.510000,  757.520000,  762.530000,  767.540000,
                         772.540000,  777.550000,  782.560000,  787.570000,  792.580000,  797.590000,
                         802.600000,  807.610000,  812.610000,  817.620000,  822.630000,  827.640000,
                         832.650000,  837.660000,  842.670000,  847.670000,  852.680000,  857.690000,
                         862.700000,  867.710000,  872.720000,  877.730000,  882.740000,  887.740000,
                         892.750000,  897.760000,  902.770000,  907.780000,  912.790000,  917.800000,
                         922.810000,  927.810000,  932.820000,  937.830000,  942.840000,  947.850000,
                         952.860000,  957.870000,  962.870000,  967.880000,  972.890000,  977.900000,
                         982.910000,  987.920000,  992.930000,  997.940000, 1002.940000, 1007.950000,
                         1012.960000, 1017.970000, 1022.980000, 1027.990000, 1033.000000, 1038.000000,
                         1043.010000, 1048.020000, 1053.030000, 1058.040000, 1063.050000, 1068.060000,
                         1073.070000, 1078.070000, 1083.080000, 1088.090000, 1093.100000, 1098.110000,
                         1103.120000, 1108.130000, 1113.130000, 1118.140000, 1123.150000, 1128.160000,
                         1133.170000, 1138.180000, 1143.190000, 1148.200000, 1153.200000, 1158.210000,
                         1163.220000, 1168.230000, 1173.240000, 1178.250000, 1183.260000, 1188.260000,
                         1193.270000, 1198.280000, 1203.290000, 1208.300000, 1213.310000, 1218.320000,
                         1223.330000, 1228.330000, 1233.340000, 1238.350000, 1243.360000, 1248.370000,
                         1253.380000, 1258.390000, 1263.390000, 1268.400000, 1273.410000, 1278.420000,
                         1283.430000, 1288.440000, 1293.450000, 1298.460000, 1303.460000, 1308.470000,
                         1313.480000, 1318.490000, 1323.500000, 1328.510000, 1333.520000, 1338.520000,
                         1343.530000, 1433.690000, 1438.700000, 1443.710000, 1448.720000, 1453.720000,
                         1458.730000, 1463.740000, 1468.750000, 1473.760000, 1478.770000, 1483.780000,
                         1488.780000, 1493.790000, 1498.800000, 1503.810000, 1508.820000, 1513.830000,
                         1518.840000, 1523.850000, 1528.850000, 1533.860000, 1538.870000, 1543.880000,
                         1548.890000, 1553.900000, 1558.910000, 1563.910000, 1568.920000, 1573.930000,
                         1578.940000, 1583.950000, 1588.960000, 1593.970000, 1598.980000, 1603.980000,
                         1608.990000, 1614.000000, 1619.010000, 1624.020000, 1629.030000, 1634.040000,
                         1639.040000, 1644.050000, 1649.060000, 1654.070000, 1659.080000, 1664.090000,
                         1669.100000, 1674.110000, 1679.110000, 1684.120000, 1689.130000, 1694.140000,
                         1699.150000, 1704.160000, 1709.170000, 1714.170000, 1719.180000, 1724.190000,
                         1729.200000, 1734.210000, 1739.220000, 1744.230000, 1749.240000, 1754.240000,
                         1759.250000, 1764.260000, 1769.270000, 1774.280000, 1954.590000, 1959.600000,
                         1964.610000, 1969.620000, 1974.630000, 1979.630000, 1984.640000, 1989.650000,
                         1994.660000, 1999.670000, 2004.680000, 2009.690000, 2014.700000, 2019.700000,
                         2024.710000, 2029.720000, 2034.730000, 2039.740000, 2044.750000, 2049.760000,
                         2054.760000, 2059.770000, 2064.780000, 2069.790000, 2074.800000, 2079.810000,
                         2084.820000, 2089.830000, 2094.830000, 2099.840000, 2104.850000, 2109.860000,
                         2114.870000, 2119.880000, 2124.890000, 2129.890000, 2134.900000, 2139.910000,
                         2144.920000, 2149.930000, 2154.940000, 2159.950000, 2164.960000, 2169.960000,
                         2174.970000, 2179.980000, 2184.990000, 2190.000000, 2195.010000, 2200.020000,
                         2205.020000, 2210.030000, 2215.040000, 2220.050000, 2225.060000, 2230.070000,
                         2235.080000, 2240.090000, 2245.090000, 2250.100000, 2255.110000, 2260.120000,
                         2265.130000, 2270.140000, 2275.150000, 2280.150000, 2285.160000, 2290.170000,
                         2295.180000, 2300.190000, 2305.200000, 2310.210000, 2315.220000, 2320.220000,
                         2325.230000, 2330.240000, 2335.250000, 2340.260000, 2345.270000, 2350.280000,
                         2355.280000, 2360.290000, 2365.300000, 2370.310000, 2375.320000, 2380.330000,
                         2385.340000, 2390.350000, 2395.350000, 2400.360000, 2405.370000, 2410.380000,
                         2415.390000, 2420.400000, 2425.410000, 2430.410000, 2435.420000, 2440.430000,
                         2445.440000, 2450.450000, 2455.460000, 2460.470000, 2465.480000, 2470.480000,
                         2475.490000, 2480.500000, 2485.510000, 2490.520000, 2495.530000,2500)
  
  
# Lets create functions that will remove all the metatdata and one that will keep the data for bandspasses
# These function will be used alot
# Returns columns that are bandpasses
metaRemove<-function(x){
  meta<-c(grep("^[0-9][0-9][0-9]",colnames(x)))
  colremove<-x[,meta]
  return(colremove)
}# metaRemove Function ends

# Returns columns that are not bandpasses
bandsRemove<-function(x){
  meta<-c(grep("[a-z A-Z]",colnames(x)))
  colremove<-x[,meta]
  return(colremove)
}# bandsRemove Function ends

# Function resamples a spectral library every 5, 10, 50 and 100 nm and combines those results into a dataframe
# Resampling will change reflectance values which might result in negative values (need to remove these rows)
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
    wl_005nm=function(x) Fun1(x),
    wl_010nm=function(x) Fun2(x),
    wl_050nm=function(x) Fun3(x),
    wl_100nm=function(x) Fun4(x)
  )
  
  # Make sure the number of cores is equal to the objects in the list created
  cores = detectCores()
  if(cores>length(ResampledArgs)){
    cores<-length(ResampledArgs)
  }
  
  #Creates cluster
  cl = makeCluster(cores)
  
  # Pass objects to each core in use
  clusterExport(cl, varlist=c("Fun1","Fun2","Fun3","Fun4","SpeclibObj"),envir=environment())
  
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
  colnames(ResampledDF)[-1:-2]<-substring(colnames(ResampledDF)[-1:-2],10)
  
  # Removes rows with negative values or values >2
  ResampledDF[-1:-2][ResampledDF[-1:-2] < 0] <- 0
  ResampledDF[-1:-2][ResampledDF[-1:-2] > 2] <- 0
  
  return(ResampledDF)
  
}# Func_Resamp ends

# Function calculates vegitation indices
Func_VI<-function(VI){
  
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
  
  #Get amount of cores to use
  cores <- detectCores()
  
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
  
  df<-cbind(bandsRemove(VI),VI_CALC)
  
  # converts NAs and Infs to 0s 
  is.na(df)<-sapply(df, is.infinite)
  df[is.na(df)]<-0

  # Changes the name of the columns to regular names (numbers)
  # For now lets use -1:-2 since these are the x and y columns
  # For some reason the function above changes the y to x.1
  names(df)[-1:-2]<-names(VI_CALC)
  
  return(df)
}# Func_VI ends 

# Function reads in a Hyperspectral image as a bricked datacube
HyperSpecDerivs <-function(x){
  
  # Reads in the Hyperspectral datacubes as a bricked raster
  HyperDatacube<-brick(x)%>%
    
    # Converts Hyperspectral datacube to a dataframe
    rasterToPoints()%>%
    as.data.frame()
  
  #Removes noisey bands out in the NIR region
  HyperDatacube[275:328]<-NULL
  
  # Converts negative values to 0s
  HyperDatacube[-1:-2][HyperDatacube[-1:-2] < 0] <- 0
  
  # Converts NAs to median 0s
  HyperDatacube[-1:-2][is.na(HyperDatacube[-1:-2])]<-0
  
  # Changes the name of the columns to regular names (numbers)
  names(HyperDatacube)[-1:-2]<-Headwall_bandpasses
  
  HyDf_resamp<-Func_Resamp(HyperDatacube)
  HyDf_VI<-Func_VI(HyperDatacube)
  
  preds<-if (ncol(metaRemove(HyperDatacube)) == 272){
    cbind(HyDf_VI,metaRemove(HyDf_resamp))
  }else{
    Func_VI(x)
  }
  
  # Returns a dataframe with resampled bands and vegitation index calculation for each 
  # Pixel in a hyperspectral image
  return(preds)
  
  } # Function HyperSpecImages ends
  
  # Gets information on datacube
    FuncTiles<-function(x){
      
      Datacube<-GDALinfo(x)
    
    # Tiles datacube into x cm blocks
    # Introduce block size for y
    tile.lst <- getSpatialTiles(Datacube, block.x=.0000680, return.SpatialPolygons=TRUE)
    tile.tbl <- getSpatialTiles(Datacube, block.x=.0000680, return.SpatialPolygons=FALSE)
    
    # Adds a unique ID to each Tile
    tile.tbl$ID <- as.character(1:nrow(tile.tbl))
    
    # Creates Tiling function
    for (i in 1:2){
      Tile<-readGDAL(x, offset=unlist(tile.tbl[i,c("offset.y","offset.x")]),
                     region.dim=unlist(tile.tbl[i,c("region.dim.y","region.dim.x")]),
                     output.dim=unlist(tile.tbl[i,c("region.dim.y","region.dim.x")]),
                     silent = TRUE)
      
      # Apply the predictor function to each tile
      Tile2<-HyperSpecDerivs(Tile)
      
      # Creates a subfolder based on the name of the datacube
      subfolder<-file.path(paste(outputs_folder,basename(x)))
      dir.create(subfolder)
      
      # Writes the output to folder
      write.csv(Tile2,
                file = paste(subfolder,
                             "/PredSsDF_Tile_",i,
                             '.csv',sep=""), row.names = F)
      
      # Removes Object from memory
      rm(Tile2)
    }
    }

# Applies HyperSpecImages FUNCTION TO OBJECT
    FuncTiles(OBJECT)

} # Function ImagePredictor_generator ends

# Test your data below
# test_case3<-Func_preds(HyperSpecImages[[2]])
