###############################Creates a spectral library from spectroradiometeric scans based on headwall bandpasses##################################################
library(spectrolab)
library(tidyverse)

##Reads in spectral library as a spectral object
##This is the spectral library that had all uncalibrated scans removed
alaskaSpeclib<-readRDS("Test_Outputs/1_Field_Spec/1_Processing/alaskaSpeclib.rds")

##creates and object of bandpasses from imagery
##We'll omit bandpasses 900-1000 since there is alot of random noise in that region of the spectrum
Headwall_wv<-c(397.593
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
  #,901.276
  #,903.127
  #,904.979
  #,906.831
  #,908.683
  #,910.535
  #,912.386
  #,914.238
  #,916.09
  #,917.942
  #,919.793
  #,921.645
  #,923.497
  #,925.349
  #,927.201
  #,929.052
  #,930.904
  #,932.756
  #,934.608
  #,936.459
  #,938.311
  #,940.163
  #,942.015
  #,943.867
  #,945.718
  #,947.57
  #,949.422
  #,951.274
  #,953.125
  #,954.977
  #,956.829
  #,958.681
  #,960.533
  #,962.384
  #,964.236
  #,966.088
  #,967.94
  #,969.791
  #,971.643
  #,973.495
  #,975.347
  #,977.199
  #,979.05
  #,980.902
  #,982.754
  #,984.606
  #,986.457
  #,988.309
  #,990.161
  #,992.013
  #,993.865
  #,995.716
  #,997.568
  #,999.42)

##Now we want to resample alsakSpeclib based on the band passes
alaskaSpeclib_HDW<-spectrolab::resample(alaskaSpeclib,Headwall_wv)

###Lets convert our new spectral library spectral object  to a dataframe
##Run logical test to see if this conversion affect reflectance values
##Are there values outside of the rane 0-2???
alaskaSpecLib_test<-alaskaSpeclib_HDW%>%as.data.frame()%>%dplyr::select(-sample_name)

####Lets run that test again
tst2<-lapply(alaskaSpecLib_test[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst2$V1%>%range()##There are negative values being created here, this is where the problem lies, how can we solve this???
tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2

#tst2 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this

alaskaSpecLib_test[-1:-7] %>% 
  as.data.frame()%>%
  'colnames<-'(Headwall_wv) %>% #dim() ] 1917  333
  dplyr::select(`445.739`) %>% 
  subset(`445.739`<0) %>% nrow() ###there is only one row here that has negative values, we could try this on multiple columns
                                 ##all those columns that we know have rows that have negative values

##Lets remove this row and convert our new spectral library back to a dataframe
alaskaSpecLib_HDW1<-alaskaSpecLib_test%>%subset(`445.739`>0) ##dim()  1916  333

##Now lets convert to a spectral object and add metadata
alaskaSpecLib_HDW<-alaskaSpecLib_HDW1[-1:-7]%>%as.spectra()
meta(alaskaSpecLib_HDW)<-data.frame(alaskaSpecLib_HDW1[1:7], stringsAsFactors = FALSE)

####Lets run that test again
tst3<-lapply(alaskaSpecLib_HDW1[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
##Converting a spectral object should not change reflectance values

#Now lets create a dataframe with all scans that are equal to 25 scans per functional group
alaskaSpecLib_HDW_df<-alaskaSpecLib_HDW%>%as.data.frame()%>%dplyr::select(-sample_name)##convert to a dataframe first
alaskaSpecLib_HDW_df_equal25<-alaskaSpecLib_HDW_df%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)

  
##Lets save our bandpasses and other outputs
write(Headwall_wv,"Test_Outputs/2_HDW_Imagery/1_Processing/Headwall_wv")
write.csv(alaskaSpecLib_HDW_df        ,"Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpecLib_HDW_df.csv"        ,row.names = FALSE)
write.csv(alaskaSpecLib_HDW_df_equal25,"Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpecLib_HDW_df_equal25.csv",row.names = FALSE)

##Now lets save our New headwall spectral library
saveRDS(alaskaSpecLib_HDW,"Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW.rds")


