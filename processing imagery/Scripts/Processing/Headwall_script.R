###########################Headwall_Flight_Lines####################################
library(raster)
library(spectrolab)

##Read in Imagery
Clayton_Headwall_Tiff<-brick("Imagery/raw_8580_rd_rf_or_Area0.dat")


##Convert to dataframe
Clayton_Headwall_Tiff_df<-rasterToPoints(Clayton_Headwall_Tiff)%>%as.data.frame()


##Generate list with the wavelength values for Imagery
Headwall_wv<-c(
  397.593
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
  ,899.424
  ,901.276
  ,903.127
  ,904.979
  ,906.831
  ,908.683
  ,910.535
  ,912.386
  ,914.238
  ,916.09
  ,917.942
  ,919.793
  ,921.645
  ,923.497
  ,925.349
  ,927.201
  ,929.052
  ,930.904
  ,932.756
  ,934.608
  ,936.459
  ,938.311
  ,940.163
  ,942.015
  ,943.867
  ,945.718
  ,947.57
  ,949.422
  ,951.274
  ,953.125
  ,954.977
  ,956.829
  ,958.681
  ,960.533
  ,962.384
  ,964.236
  ,966.088
  ,967.94
  ,969.791
  ,971.643
  ,973.495
  ,975.347
  ,977.199
  ,979.05
  ,980.902
  ,982.754
  ,984.606
  ,986.457
  ,988.309
  ,990.161
  ,992.013
  ,993.865
  ,995.716
  ,997.568
  ,999.42)

##Reads in spectral library as .rda
AK2018_spectra <- readRDS("processed spec/AK2018/AK2018_spectra.rds")
bethelLib_spectra <- readRDS("processed spec/bethelLib/bethelLib_spectra.rds")
brooksLib_spectra <- readRDS("processed spec/brooksLib/brooksLib_spectra.rds")
Murph2_spectra <- readRDS("processed spec/Murph2_Lib/Murph2_spectra.rds")
Murph_lib_spectra <- readRDS("processed spec/Murph_lib/Murph_lib_spectra.rds")
yKDeltLib_spectra <- readRDS("processed spec/yKDeltLib/yKDeltLib_spectra.rds")

##Resample spectral library based on Imagery bandpasses
AK2018_spectra_HDW<-resample(AK2018_spectra,Headwall_wv)
bethelLib_spectra_HDW<-resample(bethelLib_spectra,Headwall_wv)
brooksLib_spectra_HDW<-resample(brooksLib_spectra,Headwall_wv)
Murph2_spectra_HDW<-resample(Murph2_spectra,Headwall_wv)
Murph_lib_spectra_HDW<-resample(Murph_lib_spectra,Headwall_wv)
yKDeltLib_spectra_HDW<-resample(yKDeltLib_spectra,Headwall_wv)

##Converts resampled spectral library to dataframe
AK2018_spectra_HDW   <-as.data.frame(AK2018_spectra_HDW)
bethelLib_spectra_HDW<-as.data.frame(bethelLib_spectra_HDW)
brooksLib_spectra_HDW<-as.data.frame(brooksLib_spectra_HDW)
Murph2_spectra_HDW   <-as.data.frame(Murph2_spectra_HDW)
Murph_lib_spectra_HDW<-as.data.frame(Murph_lib_spectra_HDW)
yKDeltLib_spectra_HDW<-as.data.frame(yKDeltLib_spectra_HDW)

##Combines all dataframes from previous step
alaskaSpecLib_HDW_all<-rbind(AK2018_spectra_HDW,
                        bethelLib_spectra_HDW,
                        brooksLib_spectra_HDW,
                        Murph2_spectra_HDW,
                        Murph_lib_spectra_HDW,
                        yKDeltLib_spectra_HDW)

###deletes col "sample_name"
alaskaSpecLib_HDW_all$sample_name<-NULL

#Changes band names so they'll correspond to the ones from the images
colnames(alaskaSpecLib_HDW_all)[-(1:3)]<-c(colnames(Clayton_Headwall_Tiff_df)[-(1:2)])

##Add column PFT_2 (SPECIES)
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="clacuc"]<-"FlAVocetraria cucculata"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="flacuc"]<-"FlAVocetraria cucculata"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="flaniv"]<-"FlAVocetraria nivalis"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="gravel"]<-"gravel"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_HDW_all$PFT_2[alaskaSpecLib_HDW_all$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="aultur"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="betneo"]<-"Tree"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="claama"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="claran"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="claste"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="equarv"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="herlan"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="irisit"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="luparc"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="masric"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="petfri"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="picmar"]<-"Tree"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="plesch"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="poljen"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="poljun"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="polstr"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="popbal"]<-"Tree"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="raclan"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salala"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salova"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salric"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="wooly_salix"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="aultur"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="betneo"]<-"Tree"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="claama"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="claran"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="claste"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="equarv"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="herlan"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="irisit"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="luparc"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="masric"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="petfri"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="picmar"]<-"Tree"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="plesch"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="poljen"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="poljun"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="polstr"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="popbal"]<-"Tree"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="raclan"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salala"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salova"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="salric"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_HDW_all$PFT_3[alaskaSpecLib_HDW_all$PFT=="wooly_salix"]<-"Shrub"

#Plants only
alaskaSpecLib_HDW_plants<-subset(alaskaSpecLib_HDW_all,PFT_3!="Abiotic")

##Extracts scans for each life form (lichen, bryophyte, vascular plant)
alaskaSpecLib_HDW_lichen     <-subset(alaskaSpecLib_HDW_plants,PFT_3=="Lichen")
alaskaSpecLib_HDW_bryo       <-subset(alaskaSpecLib_HDW_plants,PFT_3=="Moss")
alaskaSpecLib_HDW_lichen_bryo<-subset(alaskaSpecLib_HDW_plants,PFT_3=="Lichen"|PFT_3=="Moss")
alaskaSpecLib_HDW_vascular   <-subset(alaskaSpecLib_HDW_plants,PFT_3!="Lichen"&PFT_3!="Moss")

##SAVe dataframes generated
###sAVe table
write.csv(Clayton_Headwall_Tiff_df,"processing imagery/processed_data/Processing/Clayton_Headwall_Tiff_df.csv",row.names= FALSE)
write.csv(alaskaSpecLib_HDW_all,"processing imagery/processed_data/Processing/alaskaSpecLib_HDW_all.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_plants,"processing imagery/processed_data/Processing/alaskaSpecLib_HDW_plants.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_lichen,"processing imagery/processed_data/Processing/alaskaSpecLib_HDW_lichen.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_bryo,"processing imagery/processed_data/Processing/alaskaSpecLib_HDW_bryo.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_lichen_bryo,"processing imagery/processed_data/Processing/alaskaSpecLib_HDW_lichen_bryo.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_vascular,"processing imagery/processed_data/Processing/alaskaSpecLib_HDW_vascular.csv",row.names = FALSE)
