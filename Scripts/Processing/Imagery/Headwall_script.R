###################################Headwall_FLIGHT_LINES####################################
library(raster)
library(spectrolab)
library(tidyverse)

##Reads in Imagery as multi layer raster
Clayton_HDW_Tiff_Spatial<-brick("Imagery/raw_8580_rd_rf_or_Area0.dat")

##Converts raster layer to combined dataframe 
Clayton_HDW_Tiff_df<-rasterToPoints(Clayton_HDW_Tiff_Spatial)%>% as.data.frame()

##Reads in bandpasses for imagery
HDW_ng_wv<-scan("Processed_imagery/Headwall/Bandpasses/Headwall_WVLs", numeric())

##Reads in spectral library as .rda
alaskaSpecLib_HDW<-readRDS("Processed_spec/All_locations/alaskaSpeclib.rds")

##Resample spectral library based on Imagery bandpasses
alaskaSpecLib_HDW<-spectrolab::resample(alaskaSpecLib_HDW,HDW_ng_wv)%>%as.data.frame()

###deletes col "sample_name"
alaskaSpecLib_HDW$sample_name<-NULL

#Changes band names so they'll correspond to the ones from the images
colnames(alaskaSpecLib_HDW)[-(1:3)]<-c(colnames(Clayton_HDW_Tiff_df)[-(1:2)])

##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="gravel"]<-"Gravel"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_HDW$PFT_2[alaskaSpecLib_HDW$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column_HDW PF_HDWT_3 (Couser respo_HDWnse variables)
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="aultur"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="betneo"]<-"Tree"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="claama"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="claran"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="claste"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="equarv"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="herlan"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="irisit"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="luparc"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="masric"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="petfri"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="picmar"]<-"Tree"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="plesch"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="poljen"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="poljun"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="polstr"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="popbal"]<-"Tree"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="raclan"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="salala"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="salova"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="salric"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_HDW$PFT_3[alaskaSpecLib_HDW$PFT=="wooly_salix"]<-"Shrub"

#Plants only
alaskaSpecLib_HDW_plants<-subset(alaskaSpecLib_HDW,PFT_3!="Abiotic")

##Creates new dataframe with a frequency column (shows the amount of scans per species within the library) 
alaskaSpecLib_HDW_freqTab       <-as.data.frame(table(alaskaSpecLib_HDW$PFT_2       ))
alaskaSpecLib_HDW_plants_freqTab<-as.data.frame(table(alaskaSpecLib_HDW_plants$PFT_2))

##Combines the frequency dataframe with spectral library
alaskaSpecLib_HDW$Freq       <-alaskaSpecLib_HDW_freqTab$Freq       [match(alaskaSpecLib_HDW$PFT_2,alaskaSpecLib_HDW_freqTab$Var1)]
alaskaSpecLib_HDW_plants$Freq<-alaskaSpecLib_HDW_plants_freqTab$Freq[match(alaskaSpecLib_HDW_plants$PFT_2,alaskaSpecLib_HDW_plants_freqTab$Var1)]

##Reorder columns
alaskaSpecLib_HDW       <-alaskaSpecLib_HDW%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())
alaskaSpecLib_HDW_plants<-alaskaSpecLib_HDW_plants%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())

##Creates dataframes that has all the species with scans greater than specified values
alaskaSpecLib_HDW_more05<-subset(alaskaSpecLib_HDW,Freq>=5)
alaskaSpecLib_HDW_more10<-subset(alaskaSpecLib_HDW,Freq>=10)
alaskaSpecLib_HDW_more15<-subset(alaskaSpecLib_HDW,Freq>=15)
alaskaSpecLib_HDW_more20<-subset(alaskaSpecLib_HDW,Freq>=20)

alaskaSpecLib_HDW_plants_more05<-subset(alaskaSpecLib_HDW_plants,Freq>=5)
alaskaSpecLib_HDW_plants_more10<-subset(alaskaSpecLib_HDW_plants,Freq>=10)
alaskaSpecLib_HDW_plants_more15<-subset(alaskaSpecLib_HDW_plants,Freq>=15)
alaskaSpecLib_HDW_plants_more20<-subset(alaskaSpecLib_HDW_plants,Freq>=20)

##creates datafranme with equal scans for each species (10,15,20 scans per species )
alaskaSpecLib_HDW_equal05<-alaskaSpecLib_HDW_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_HDW_equal10<-alaskaSpecLib_HDW_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_HDW_equal15<-alaskaSpecLib_HDW_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_HDW_equal20<-alaskaSpecLib_HDW_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

alaskaSpecLib_HDW_plants_equal05<-alaskaSpecLib_HDW_plants_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_HDW_plants_equal10<-alaskaSpecLib_HDW_plants_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_HDW_plants_equal15<-alaskaSpecLib_HDW_plants_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_HDW_plants_equal20<-alaskaSpecLib_HDW_plants_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##Save spectral Library to be used in imagery prediction 
write.csv(Clayton_HDW_Tiff_df                        ,"Processed_spec/Imagery/Raw/Clayton_HDW_Tiff_df.csv     ",row.names= FALSE)
write.csv(alaskaSpecLib_HDW                          ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW.csv       ",row.names= FALSE)
write.csv(alaskaSpecLib_HDW_plants                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants.csv",row.names= FALSE)

write.csv(alaskaSpecLib_HDW_more05                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_more05.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_more10                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_more10.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_more15                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_more15.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_more20                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_more20.csv  ",row.names = FALSE)

write.csv(alaskaSpecLib_HDW_plants_more05            ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants_more05.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_plants_more10            ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants_more10.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_plants_more15            ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants_more15.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_plants_more20            ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants_more20.csv  ",row.names = FALSE)

write.csv(alaskaSpecLib_HDW_equal05                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal05.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_equal10                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal10.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_equal15                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal15.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_equal20                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal20.csv",row.names = FALSE)

write.csv(alaskaSpecLib_HDW_plants_equal05           ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants_equal05.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_plants_equal10           ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants_equal10.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_plants_equal15           ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants_equal15.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_plants_equal20           ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_plants_equal20.csv",row.names = FALSE)



