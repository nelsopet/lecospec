###################################AVIRIS_FLIGHT_LINES####################################
library(raster)
library(spectrolab)

##Reads in Imagery as multi layer raster
Clayton_AVIRIS_Tiff_Spatial<-brick("Imagery/Spectral_Subset_2019_05_15T22_29_34Z_Area0.dat")

##Marks raster as unrotated
Clayton_AVIRIS_Tiff_Spatial@rotated<-FALSE

##Converts raster layer to combined dataframe to dataframe
Clayton_AVIRIS_Tiff_df<-rasterToPoints(Clayton_AVIRIS_Tiff_Spatial)%>% as.data.frame()

##Generates list with the wAVelength values for Imagery
AVIRIS_wv<-c(381.870000,  386.880000,  391.890000,  396.890000,  401.900000,  406.910000,
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
             2475.490000, 2480.500000, 2485.510000, 2490.520000, 2495.530000, 2500)

##Reads in spectral library as .rda
AK2018_spectra <- readRDS("processed spec/AK2018/AK2018_spectra.rds")
bethelLib_spectra <- readRDS("processed spec/bethelLib/bethelLib_spectra.rds")
brooksLib_spectra <- readRDS("processed spec/brooksLib/brooksLib_spectra.rds")
Murph2_spectra <- readRDS("processed spec/Murph2_Lib/Murph2_spectra.rds")
Murph_lib_spectra <- readRDS("processed spec/Murph_lib/Murph_lib_spectra.rds")
yKDeltLib_spectra <- readRDS("processed spec/yKDeltLib/yKDeltLib_spectra.rds")

##Resample spectral library based on Imagery bandpasses
AK2018_spectra_AV<-resample(AK2018_spectra,AVIRIS_wv)
bethelLib_spectra_AV<-resample(bethelLib_spectra,AVIRIS_wv)
brooksLib_spectra_AV<-resample(brooksLib_spectra,AVIRIS_wv)
Murph2_spectra_AV<-resample(Murph2_spectra,AVIRIS_wv)
Murph_lib_spectra_AV<-resample(Murph_lib_spectra,AVIRIS_wv)
yKDeltLib_spectra_AV<-resample(yKDeltLib_spectra,AVIRIS_wv)

##Converts resampled spectral library to dataframe
AK2018_spectra_AV   <-as.data.frame(AK2018_spectra_AV)
bethelLib_spectra_AV<-as.data.frame(bethelLib_spectra_AV)
brooksLib_spectra_AV<-as.data.frame(brooksLib_spectra_AV)
Murph2_spectra_AV   <-as.data.frame(Murph2_spectra_AV)
Murph_lib_spectra_AV<-as.data.frame(Murph_lib_spectra_AV)
yKDeltLib_spectra_AV<-as.data.frame(yKDeltLib_spectra_AV)


##Combines all dataframes from previous step
alaskaSpecLib_AV_all<-rbind(AK2018_spectra_AV,
                        bethelLib_spectra_AV,
                        brooksLib_spectra_AV,
                        Murph2_spectra_AV,
                        Murph_lib_spectra_AV,
                        yKDeltLib_spectra_AV)

###deletes col "sample_name"
alaskaSpecLib_AV_all$sample_name<-NULL

#Changes band names so they'll correspond to the ones from the images
colnames(alaskaSpecLib_AV_all)[-(1:3)]<-c(colnames(Clayton_AVIRIS_Tiff_df)[-(1:2)])

##Add column PFT_2 (SPECIES)
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="clacuc"]<-"FlAVocetraria cucculata"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="flacuc"]<-"FlAVocetraria cucculata"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="flaniv"]<-"FlAVocetraria nivalis"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="gravel"]<-"gravel"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_AV_all$PFT_2[alaskaSpecLib_AV_all$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="aultur"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="betneo"]<-"Tree"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="claama"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="claran"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="claste"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="equarv"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="herlan"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="irisit"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="luparc"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="masric"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="petfri"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="picmar"]<-"Tree"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="plesch"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="poljen"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="poljun"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="polstr"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="popbal"]<-"Tree"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="raclan"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salala"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salova"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salric"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="wooly_salix"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="aultur"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="betneo"]<-"Tree"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="claama"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="claran"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="claste"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="equarv"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="herlan"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="irisit"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="luparc"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="masric"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="petfri"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="picmar"]<-"Tree"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="plesch"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="poljen"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="poljun"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="polstr"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="popbal"]<-"Tree"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="raclan"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salala"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salova"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="salric"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_AV_all$PFT_3[alaskaSpecLib_AV_all$PFT=="wooly_salix"]<-"Shrub"

#Plants only
alaskaSpecLib_AV_plants<-subset(alaskaSpecLib_AV_all,PFT_3!="Abiotic")

##Extracts scans for each life form (lichen, bryophyte, vascular plant)
alaskaSpecLib_AV_lichen     <-subset(alaskaSpecLib_AV_plants,PFT_3=="Lichen")
alaskaSpecLib_AV_bryo       <-subset(alaskaSpecLib_AV_plants,PFT_3=="Moss")
alaskaSpecLib_AV_lichen_bryo<-subset(alaskaSpecLib_AV_plants,PFT_3=="Lichen"|PFT_3=="Moss")
alaskaSpecLib_AV_vascular   <-subset(alaskaSpecLib_AV_plants,PFT_3!="Lichen"&PFT_3!="Moss")

##SAVe dataframes generated
###sAVe table
write.csv(Clayton_AVIRIS_Tiff_df,"processing imagery/processed_data/Processing/Clayton_AVIRIS_Tiff_df.csv",row.names= FALSE)
write.csv(alaskaSpecLib_AV_all,"processing imagery/processed_data/Processing/alaskaSpecLib_AV_all.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_plants,"processing imagery/processed_data/Processing/alaskaSpecLib_AV_plants.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_lichen,"processing imagery/processed_data/Processing/alaskaSpecLib_AV_lichen.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_bryo,"processing imagery/processed_data/Processing/alaskaSpecLib_AV_bryo.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_lichen_bryo,"processing imagery/processed_data/Processing/alaskaSpecLib_AV_lichen_bryo.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_vascular,"processing imagery/processed_data/Processing/alaskaSpecLib_AV_vascular.csv",row.names = FALSE)
