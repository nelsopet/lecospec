###################################AVIRIS_FLIGHT_LINES####################################
library(raster)
library(spectrolab)
library(tidyverse)

##Reads in Imagery as multi layer raster
Clayton_AVIRIS_Tiff_Spatial<-brick("Imagery/Spectral_Subset_2019_05_15T22_29_34Z_Area0.dat")

##Marks raster as unrotated
Clayton_AVIRIS_Tiff_Spatial@rotated<-FALSE

##Converts raster layer to combined dataframe 
Clayton_AVIRIS_Tiff_df<-rasterToPoints(Clayton_AVIRIS_Tiff_Spatial)%>% as.data.frame()

##Reads in bandpasses for imagery
AVIRIS_ng_wv<-scan("Processed_imagery/AVIRIS/Bandpasses/AVIRIS_WVLs", numeric())

##Reads in spectral library as .rda
alaskaSpecLib_AV<-readRDS("Processed_spec/All_locations/alaskaSpeclib.rds")

##Resample spectral library based on Imagery bandpasses
alaskaSpecLib_AV<-spectrolab::resample(alaskaSpecLib_AV,AVIRIS_ng_wv)%>%as.data.frame()

###deletes col "sample_name"
alaskaSpecLib_AV$sample_name<-NULL

#Changes band names so they'll correspond to the ones from the images
colnames(alaskaSpecLib_AV)[-(1:3)]<-c(colnames(Clayton_AVIRIS_Tiff_df)[-(1:2)])

##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="gravel"]<-"Gravel"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_AV$PFT_2[alaskaSpecLib_AV$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column_AV PF_AVT_3 (Couser respo_AVnse variables)
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="aultur"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="betneo"]<-"Tree"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="claama"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="claran"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="claste"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="equarv"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="herlan"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="irisit"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="luparc"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="masric"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="petfri"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="picmar"]<-"Tree"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="plesch"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="poljen"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="poljun"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="polstr"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="popbal"]<-"Tree"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="raclan"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="salala"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="salova"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="salric"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_AV$PFT_3[alaskaSpecLib_AV$PFT=="wooly_salix"]<-"Shrub"

#Plants only
alaskaSpecLib_AV_plants<-subset(alaskaSpecLib_AV,PFT_3!="Abiotic")

##Creates new dataframe with a frequency column (shows the amount of scans per species within the library) 
alaskaSpecLib_AV_freqTab       <-as.data.frame(table(alaskaSpecLib_AV$PFT_2       ))
alaskaSpecLib_AV_plants_freqTab<-as.data.frame(table(alaskaSpecLib_AV_plants$PFT_2))

##Combines the frequency dataframe with spectral library
alaskaSpecLib_AV$Freq       <-alaskaSpecLib_AV_freqTab$Freq       [match(alaskaSpecLib_AV$PFT_2,alaskaSpecLib_AV_freqTab$Var1)]
alaskaSpecLib_AV_plants$Freq<-alaskaSpecLib_AV_plants_freqTab$Freq[match(alaskaSpecLib_AV_plants$PFT_2,alaskaSpecLib_AV_plants_freqTab$Var1)]

##Reorder columns
alaskaSpecLib_AV       <-alaskaSpecLib_AV%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())
alaskaSpecLib_AV_plants<-alaskaSpecLib_AV_plants%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())

##Creates dataframes that has all the species with scans greater than specified values
alaskaSpecLib_AV_more05<-subset(alaskaSpecLib_AV,Freq>=5)
alaskaSpecLib_AV_more10<-subset(alaskaSpecLib_AV,Freq>=10)
alaskaSpecLib_AV_more15<-subset(alaskaSpecLib_AV,Freq>=15)
alaskaSpecLib_AV_more20<-subset(alaskaSpecLib_AV,Freq>=20)

alaskaSpecLib_AV_plants_more05<-subset(alaskaSpecLib_AV_plants,Freq>=5)
alaskaSpecLib_AV_plants_more10<-subset(alaskaSpecLib_AV_plants,Freq>=10)
alaskaSpecLib_AV_plants_more15<-subset(alaskaSpecLib_AV_plants,Freq>=15)
alaskaSpecLib_AV_plants_more20<-subset(alaskaSpecLib_AV_plants,Freq>=20)

##creates datafranme with equal scans for each species (10,15,20 scans per species )
alaskaSpecLib_AV_equal05<-alaskaSpecLib_AV_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_AV_equal10<-alaskaSpecLib_AV_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_AV_equal15<-alaskaSpecLib_AV_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_AV_equal20<-alaskaSpecLib_AV_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

alaskaSpecLib_AV_plants_equal05<-alaskaSpecLib_AV_plants_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_AV_plants_equal10<-alaskaSpecLib_AV_plants_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_AV_plants_equal15<-alaskaSpecLib_AV_plants_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_AV_plants_equal20<-alaskaSpecLib_AV_plants_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##Save spectral Library to be used in imagery prediction 
write.csv(Clayton_AVIRIS_Tiff_df                    ,"Processed_spec/Imagery/Raw/Clayton_AVIRIS_Tiff_df.csv " ,row.names= FALSE)
write.csv(alaskaSpecLib_AV                          ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV.csv       " ,row.names= FALSE)
write.csv(alaskaSpecLib_AV_plants                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants.csv",row.names= FALSE)


write.csv(alaskaSpecLib_AV_more05                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_more05.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_more10                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_more10.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_more15                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_more15.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_more20                   ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_more20.csv",row.names = FALSE)

write.csv(alaskaSpecLib_AV_plants_more05            ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants_more05.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_AV_plants_more10            ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants_more10.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_AV_plants_more15            ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants_more15.csv  ",row.names = FALSE)
write.csv(alaskaSpecLib_AV_plants_more20            ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants_more20.csv  ",row.names = FALSE)

write.csv(alaskaSpecLib_AV_equal05                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_equal05.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_equal10                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_equal10.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_equal15                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_equal15.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_equal20                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_equal20.csv",row.names = FALSE)

write.csv(alaskaSpecLib_AV_plants_equal05           ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants_equal05.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_plants_equal10           ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants_equal10.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_plants_equal15           ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants_equal15.csv",row.names = FALSE)
write.csv(alaskaSpecLib_AV_plants_equal20           ,"Processed_spec/Imagery/Raw/alaskaSpecLib_AV_plants_equal20.csv",row.names = FALSE)



