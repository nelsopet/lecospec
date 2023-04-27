source("Functions/lecospectR.R")
AllSpecRaw<-read.csv("./Output/C_001_SC3_Cleaned_SpectralLib.csv") %>% as.data.frame()
colnames(AllSpecRaw[,1:35])
AllSpecRaw<-AllSpecRaw %>% dplyr::select(
        X,
        ScanID,
        Area,
        Code_name,
        Species_name,
        Functional_group1,
        Functional_group2,
        Species_name_Freq,
        Functional_group1_Freq,
        Functional_group2_Freq,
        Genus,
        Version,
        File.Name,
        Instrument,
        Detectors,
        Measurement,
        Date,
        Time,
        Battery.Voltage,
        Averages,
        Integration1,
        Integration2,
        Integration3,
        Dark.Mode,
        Foreoptic,
        Radiometric.Calibration,
        Units,
        Latitude,
        Longitude,
        Altitude,
        GPS.Time,
        Satellites,
        Calibrated.Reference.Correction.File,
        Channels,
        ScanNum,
        everything()
    ) 

AllAreas<-AllSpecRaw %>% group_by(Area) %>% tally() 
colnames(AllAreas)<-c("Area","ScanCount")
# 1 12mile                               62
# 2 Barrow                              154
# 3 Big Trail                            61
# 4 BirchLake                            10
# 5 Brooks_Range                        104
# 6 Eagle Summit                         51
# 7 Eight Mile                            5
# 8 Fairbanks area                       69
# 9 Fairbanks Area Spinach Circle        18
#10 Fairbanks Area Vault Road             9
#11 Murphy                              341
#12 Murphy Dome                         213
#13 Seward_Penn                         116
#14 Wickersham                            1
#15 Yukon_Delta                         129

AllSps<-AllSpecRaw %>% group_by(Species_name, Functional_group1,Functional_group2, Area) %>% tally()
write.csv(AllSps, "./figures/GroundScanSpeciesCountByFncGrp1.csv")

AreasOut<-c("Barrow","Yukon_Delta","Seward_Penn") #"Brooks_Range", 

AllSpecRaw_CenAK<-AllSpecRaw %>% subset(!(Area %in% AreasOut)) #%>% dim
AllSpecRaw_CenAK %>% group_by(Functional_group1,Area) %>% tally %>% pivot_wider(values_from = n, names_from = Functional_group1)
unique(AllSpecRaw$Species_name)

#[1] "Alectoria ochroleuca"     "Alnus sp."
# [3] "Arctagrostis latifolia"   "Arctocetraria centrifuga"
# [5] "Arctophila fulva"         "Arctostaphyllos"
# [7] "Arenaria pseudofrigida"   "Asahinea chrysantha"
# [9] "Aulacomnium palustre"     "Aulacomnium turgidum"
#[11] "Bare Rock"                "Bare Soil"
#[13] "Betula nana"              "Betula neoalaskana"
#[15] "Calamogrostis sp."        "Carex aquatilis"
#[17] "Carex sp."                "Cassiope tetragona"
#[19] "Ceratadon purpureus"      "Cetraria islandica"
#[21] "Cetraria laevigata"       "Cladonia amaurocraea"
#[23] "Cladonia cornuta"         "Cladonia gracilis"
#[25] "Cladonia mitis"           "Cladonia rangiferina"
#[27] "Cladonia stellaris"       "Cladonia stygia"
#[29] "Cladonia sulphurina"      "Cladonia uncialis"
#[31] "Dactylina arctica"        "Dead Salix"
#[33] "Dicranum sp."             "Dryas sp."
#[35] "Dupontia fisheri"         "Empetrum nigrum"
#[37] "Equisetum arvense"        "Equisetum sylvaticum"
#[39] "Eriophorum angustifolium" "Eriophorum vaginatum"
#[41] "Flavocetraria nivalis"    "Heracleum lanatum"
#[43] "Hieracium sp."            "Hylocomium splendens"
#[45] "Iris sp."                 "Ledum decumbens"
#[47] "Loisleuria procumbens"    "Lupinus sp."
#[49] "Masonhalea richardsonii"  "Melanelia sp."
#[51] "Nephroma arcticum"        "Parmelia omphalodes"
#[53] "Parmeliopsis ambigua"     "Pedicularis racemosa"
#[55] "Pedicularis sudetica"     "Peltigera apthosa"
#[57] "Peltigera malacea"        "Peltigera scabrata"
#[59] "Peltigers leucophlebia"   "Pestasites frigidus"
#[61] "Petasites frigidus"       "Picea mariana"
#[63] "Pices (bark)"             "Pilophorus acicularis"
#[65] "Plagiomnium sp."          "Pleurozium schreberi"
#[67] "Polytrichum juniperinum"  "Polytrichum sp."
#[69] "Populus balsamifera"      "Porpidia sp."
#[71] "Quartz"                   "Racomitrium lanoiginosum"
#[73] "Rhizocarpon geographicum" "Rhizocarpon sp."
#[75] "Rhytidum rugosum"         "Rosa acicularis"
#[77] "Rubus sp."                "Salix (wooly)"
#[79] "Salix alaxensis"          "Salix arbusculoides"
#[81] "Salix glauca"             "Salix lanata"
#[83] "Salix ovalifolia"         "Salix phlebophylla"
#[85] "Salix pulchra"            "Salix richardsonii"
#[87] "Saxifraga punctata"       "Sphagnum sp."
#[89] "Stereocaulon sp."         "Toefeldia sp."
#[91] "Tomenthypnum nitens"      "Trapelopsis granulosa"
#[93] "Umbilicaria arctica"      "Umbilicaria hyperborea"
#[95] "Vaccinium uliginosum"     "Vaccinium vitis-idea"
#[97] "Cladonia steallaris"      "Petasites frigida"

#DROP ABIOTIC "Quartz" #subset(Species_name!="Quartz") %
#DROP Forbs AreasOut   #subset(!(Area %in% AreasOut)|Functional_group1 != "Forb") 
#KEEP All TreeBroadleaf but consider looking for other tree species scans present in the images like POPTRE?
#KEEP All Graminoids
#Drop EvergreenShrub from Dryas at Eagle Summit, which is 50% of the class # subset(Area!="Eagle Summit",Species_name != "Dryas sp.")
#Drop ShrubDecid dominated by Salix alaxensis at YK Delta (20% of this class is just this one species/location)
#Drop MOSS 
#Drop LICHEN not common enough to contribute much; make a list to keep 
LichenKeep<-c(
"Cetraria islandica",
"Cetraria laevigata",
"Cladonia amaurocraea", 
"Cladonia mitis",
"Cladonia rangiferina", 
"Cladonia gracilis",
"Cladonia stellaris",
"Cladonia steallaris",
"Cladonia uncialis",
"Flavocetraria nivalis", 
"Masonhalea richardsonii", 
"Melanelia sp.", 
"Nephroma arcticum", 
"Rhizocarpon geographicum",
"Umbilicaria hyperborea", 
"Stereocaulon sp.") %>% as.data.frame() #%>% 
colnames(LichenKeep)<-"Species_name"
LichenAll<-AllSpecRaw %>% filter(Functional_group1 == "Lichen") %>% select(Species_name) %>% unique() %>% as.data.frame() #%>% dplyr::rename(Lichen = .)

LichenDrop<-setdiff(LichenAll$Species_name, LichenKeep$.)

MossAll<-AllSpecRaw %>% filter(Functional_group1 == "Moss") %>% group_by(Species_name) %>% tally() %>% as.data.frame() #%>% dplyr::rename(Lichen = .)

#               Species_name  n
#1      Aulacomnium palustre 12
#2      Aulacomnium turgidum  8
#3       Ceratadon purpureus  5
#4              Dicranum sp.  5
#5      Hylocomium splendens 13
#6           Plagiomnium sp.  4
#7      Pleurozium schreberi  9
#8   Polytrichum juniperinum 12
#9           Polytrichum sp. 26
#10 Racomitrium lanoiginosum  4
#11         Rhytidum rugosum 12
#12             Sphagnum sp.  8
#13      Tomenthypnum nitens  4

MossDrop<-c(
"Aulacomnium turgidum",
"Ceratadon purpureus",
"Dicranum sp",
"Polytrichum sp.",
"Plagiomnium sp.",
"Rhytidum rugosum", 
"Tomenthypnum nitens") #%>% #as.data.frame()
#colnames(MossDrop)<-"Species_name"

MossDrop_df<-c(
"Aulacomnium turgidum",
"Ceratadon purpureus",
"Dicranum sp",
"Polytrichum sp.",
"Plagiomnium sp.",
"Rhytidum rugosum", 
"Tomenthypnum nitens") %>% as.data.frame()
colnames(MossDrop_df)<-"Species_name"

MossKeep<-setdiff(MossAll$Species_name,MossDrop_df$.)

AllSpecRaw_CommonSp<- 
AllSpecRaw %>%
subset(Species_name!="Quartz") %>% #dim 1293 rows 
subset(!(Area %in% AreasOut)|Functional_group1 != "Forb") %>% #dim #1204 rows
subset(Area!="Eagle Summit"|Species_name != "Dryas sp.") %>% #dim #1153 rows
subset(Area!="Yukon_Delta"|Species_name != "Salix alaxensis") %>% #dim #1106rows
subset(!(Species_name %in% LichenDrop$Species_name)) %>% #dim #923 rows
subset(!(Species_name %in% MossDrop)) #%>% #dim #864 rows
#group_by(Species_name, Functional_group1) %>% tally()

write.csv(AllSpecRaw_CommonSp, "./Output/C_001_SC3_Cleaned_SpectralLib_CommonSp.csv")

AllSpecRaw_CenAKCommonSp<- 
AllSpecRaw_CenAK %>%
subset(Species_name!="Quartz") %>% #dim 1293 rows 
subset(!(Area %in% AreasOut)|Functional_group1 != "Forb") %>% #dim #1204 rows
subset(Area!="Eagle Summit"|Species_name != "Dryas sp.") %>% #dim #1153 rows
subset(Area!="Yukon_Delta"|Species_name != "Salix alaxensis") %>% #dim #1106rows
subset(!(Species_name %in% LichenDrop)) %>% #dim #923 rows
subset(!(Species_name %in% MossDrop)) #%>% #dim #864 rows
#group_by(Species_name, Functional_group1) %>% tally()

AllSpecRaw_CenAKCommonSp %>% group_by(Area,
    Functional_group1) %>% tally %>% pivot_wider(values_from = n, names_from = Functional_group1)
dim(AllSpecRaw_CenAKCommonSp)
write.csv(AllSpecRaw_CenAKCommonSp, "./Output/C_001_SC3_Cleaned_SpectralLib_CenAkCommonSp.csv")
