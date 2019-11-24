########Creates a A spectral library object of all the scans collected in Alaksa in 2018 and 2019############
library(spectrolab)
library(tidyverse)

##Reads in a spectral object for each area sampled in Alaksa for the years 2018-2019)...all object have bands from 350:2500nm and metadata being ScanaID,PFT and Area
AK2018_spectra          <-readRDS("Outputs/1_Field_spec/1_Processing/AK2018_spectra.rds"          )##SPECTRAL OBJECT WITH 99 SAMPLES
bethelLib_spectra       <-readRDS("Outputs/1_Field_spec/1_Processing/bethelLib_spectra.rds"       )##SPECTRAL OBJECT WITH 60 SAMPLES
brooksLib_spectra       <-readRDS("Outputs/1_Field_spec/1_Processing/brooksLib_spectra.rds"       )##SPECTRAL OBJECT WITH 132 SAMPLES
Murph2_spectra          <-readRDS("Outputs/1_Field_spec/1_Processing/Murph2_spectra.rds"          )##SPECTRAL OBJECT WITH 140 SAMPLES
Murph_lib_spectra       <-readRDS("Outputs/1_Field_spec/1_Processing/Murph_lib_spectra.rds"       )##SPECTRAL OBJECT WITH 249 SAMPLES
yKDeltLib_spectra       <-readRDS("Outputs/1_Field_spec/1_Processing/yKDeltLib_spectra.rds"       )##SPECTRAL OBJECT WITH 183 SAMPLES
TwelveMile_spectra      <-readRDS("Outputs/1_Field_spec/1_Processing/TwelveMile_spectra.rds"      )##SPECTRAL OBJECT WITH 73 SAMPLES
Big_Trail_Lake_spectra  <-readRDS("Outputs/1_Field_spec/1_Processing/Big_Trail_Lake_spectra.rds"  )##SPECTRAL OBJECT WITH 82 SAMPLES
Eagle_summit_spectra    <-readRDS("Outputs/1_Field_spec/1_Processing/Eagle_summit_spectra.rds"    )##SPECTRAL OBJECT WITH 467 SAMPLES
Murphy_domeA_spectra    <-readRDS("Outputs/1_Field_spec/1_Processing/Murphy_domeA_spectra.rds"    )##SPECTRAL OBJECT WITH 83 SAMPLES
Murphy_domeB_spectra    <-readRDS("Outputs/1_Field_spec/1_Processing/Murphy_domeB_spectra.rds"    )##SPECTRAL OBJECT WITH 52 SAMPLES
Wickersham_domeA_spectra<-readRDS("Outputs/1_Field_spec/1_Processing/Wickersham_domeA_spectra.rds")##SPECTRAL OBJECT WITH 132 SAMPLES
Wickersham_domeB_spectra<-readRDS("Outputs/1_Field_spec/1_Processing/Wickersham_domeB_spectra.rds")##SPECTRAL OBJECT WITH 233 SAMPLES

##If we combine these spectral objects we should get a spectral library of 1985 ssamples
##This function combines the list of spectral objects above....spectral object with 1985 samples, bands from 350:2500nm and metadata being ScanaID,PFT and Area
alaskaSpecLib<-Reduce(spectrolab::combine,list(AK2018_spectra          
                                                   ,bethelLib_spectra       
                                                   ,brooksLib_spectra       
                                                   ,Murph2_spectra          
                                                   ,Murph_lib_spectra       
                                                   ,yKDeltLib_spectra       
                                                   ,TwelveMile_spectra      
                                                   ,Big_Trail_Lake_spectra  
                                                   ,Eagle_summit_spectra    
                                                   ,Murphy_domeA_spectra    
                                                   ,Murphy_domeB_spectra    
                                                   ,Wickersham_domeA_spectra
                                                   ,Wickersham_domeB_spectra))

##Now we want to convert our spectral object to a dataframe to build our spectral library...dataframe with 1985 rows and 2154 cols
alaskaSpecLib<-as.data.frame(alaskaSpecLib)%>%dplyr::select(-sample_name)

##Now we want to add columns that represent the species and funcional groups, PFT_2 and PFT_3 respectively. 
##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="abibal"]<-"Abies balsamea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="acerub"]<-"Acer rubrum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="acepen"]<-"Acer pensylvanicum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="alninc"]<-"Alnus Incana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arctop"]<-"Unknown"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betall"]<-"Betula alleghaniensis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betpap"]<-"Betula papyrifera"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betpop"]<-"Betula populifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carram"]<-"Carex sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="faggra"]<-"Fagus grandifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="fraame"]<-"Fraxinus americana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="gravel"]<-"Gravel"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="larlar"]<-"Larix Larcina"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="picrub"]<-"Picea rubens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pinstr"]<-"Pinus strobus"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="popgra"]<-"Populus grandidentata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="prupen"]<-"Prunus pensylvanica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="quartz"]<-"Quartz"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="querub"]<-"Quercus Rubra"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rhutyp"]<-"Rhus typhina"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="thuocc"]<-"Thuja occidentalis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="tsucan"]<-"Tsuga canadensis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="abibal"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="acerub"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="acepen"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="alninc"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arccen"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arctop"]<-"Unknown"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="asachr"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aulpal"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aultur"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betall"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betnan"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betneo"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betpap"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betpop"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carram"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cerpur"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claama"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clacor"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clagra"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clamit"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claran"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claste"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clasty"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clasul"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claunc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dicranum"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="equarv"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="equsyl"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="evemes"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="faggra"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="fraame"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="herlan"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hylspl"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hypspl"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="irisit"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="larlar"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="leddec"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="luparc"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="masric"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="melhep"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="neparc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="naparc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="paramb"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="paromp"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="parsul"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pedrac"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pedsud"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="petfri"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="picmar"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="picrub"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pinstr"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popbal"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popgra"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="prupen"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="plesch"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="poljen"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="poljun"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="polstr"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popbal"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="querub"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="raclan"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhutyp"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rosaci"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rosasc"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salala"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salarb"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salgla"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sallan"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salova"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="Salova"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salpul"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salric"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sphagn"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sphfus"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="stepas"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="stetas"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="thuocc"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tomnit"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tragra"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tsucan"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vaculi"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="wooly_salix"]<-"Shrub"

##Lets add more details to our spectral library by adding  frequency columns
##This frequency value represents the number of scans per species and the number of scans per functional group
##Lets start by creating a new dataframe with a frequency column for species and one for functional group
##Where PFT_2 represents species and PFT_3 represents functional groups
alaskaSpecLibPFT2_freqTab<-as.data.frame(table(alaskaSpecLib$PFT_2))##SPECIES FREQ TABLE
alaskaSpecLibPFT3_freqTab<-as.data.frame(table(alaskaSpecLib$PFT_3))##Func Group FREQ TABLE

###Please note these are the number of samples we have for each functional group
##The Dwarf shrub category is high because we scanned a high number of Dryas sp. in summer 2019, >700
#Abiotic      58
#Dwarf Shrub  992
#Forb         80
#Graminoid    30
#Lichen       464
#Moss         90
#Shrub        241
#Tree         25
#Unknown      5

##Lets combine the frequency table with our spectral library...this adds two more columns to our spectral library
alaskaSpecLib$Freq1<-alaskaSpecLibPFT2_freqTab$Freq[match(alaskaSpecLib$PFT_2,alaskaSpecLibPFT2_freqTab$Var1)]
alaskaSpecLib$Freq2<-alaskaSpecLibPFT3_freqTab$Freq[match(alaskaSpecLib$PFT_3,alaskaSpecLibPFT3_freqTab$Var1)]

##Lets reorder the columns so our data is a little more structured. 
alaskaSpecLib<-alaskaSpecLib%>%dplyr::select(ScanID,PFT,PFT_2,PFT_3,Freq1,Freq2,everything())

##Lets remove the 5 scans that were unknown...now we have a dataframe with dimensions 1980 2158
alaskaSpecLib<-subset(alaskaSpecLib,PFT_3!="Unknown")
#str(alaskaSpecLib)

###Lets remove all the uncalibrated scans, but first we have to identify those scans
##Checck to see if there are any values greater than 2 or less than 0
tst<-lapply(alaskaSpecLib[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst$V1%>%range()
tst$V2%>%range()
tst<-subset(tst,V2>2)

##now we have all the columns that have values greater than two, lets save those column names in an object
badscans<-rownames(tst)
badscans<-c("1891"  ,"1892" ,"1893" ,"1894" ,"1895" ,"1896" ,"1897" ,"1898" ,"1899" ,"1900" ,"1901" ,"1902" ,"1908" ,"1909"
            ,"1916" ,"1917" ,"1930" ,"1931" ,"1932" ,"1938" ,"1939" ,"1940" ,"1947" ,"1948" ,"2480" ,"2481" ,"2482" ,"2490"
            ,"2491" ,"2492" ,"2493" ,"2497" ,"2498" ,"2499" ,"2500")


##Column names are saved, lets create a function that will will remove all those rows that have values greater than 2
####Need to come up with a function
alaskaSpecLib<- alaskaSpecLib[apply(alaskaSpecLib[,badscans]<2, 1, all),]### dim 1975 2158

###lets create a df from our spectral library to be saved
alaskaSpecLib_df<-alaskaSpecLib

####Lets run that test again
tst<-lapply(alaskaSpecLib[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst$V2%>%range()##There are no weird values, those are values outside of 0 and 2

###Lets convert our new spectral library to a spectral object, then reconvert it to a dataframe
##Run logical test again to see if this conversion affect reflectance values
##Add metadata and 
alaskaSpecLib_test<-alaskaSpecLib[-1:-7]%>%spectrolab::as.spectra()
meta(alaskaSpecLib_test)<-data.frame(alaskaSpecLib[1:7], stringsAsFactors = FALSE)

###lets convert spectral object and run logical test again
alaskaSpecLib_test<-alaskaSpecLib_test%>%as.data.frame()%>%dplyr::select(-sample_name)

####Lets run that test again
tst1<-lapply(alaskaSpecLib_test[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst1$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst1$V2%>%range()##There are no weird values, those are values outside of 0 and 2
                 ##Converting a spectral object should not change reflectance values

#We want to create dataframes that have all the scans of each functional groups
##This can be used to make graphs of all the species within each functional group
alaskaSpecLib_Lichen    <-subset(alaskaSpecLib,PFT_3=="Lichen")
alaskaSpecLib_Tree      <-subset(alaskaSpecLib,PFT_3=="Tree")
alaskaSpecLib_Dwarfshrub<-subset(alaskaSpecLib,PFT_3=="Dwarf Shrub")
alaskaSpecLib_shrub     <-subset(alaskaSpecLib,PFT_3=="Shrub")
alaskaSpecLib_Moss      <-subset(alaskaSpecLib,PFT_3=="Moss")
alaskaSpecLib_Graminoid <-subset(alaskaSpecLib,PFT_3=="Graminoid")
alaskaSpecLib_Forb      <-subset(alaskaSpecLib,PFT_3=="Forb")

##Now we want to convert our new spectral library back to a spectral object to be saved
#First Remove metadata from spectral library
alaskaSpecLib_meta<-alaskaSpecLib[,c(1:7)]

###Create alskaspeclib without meta
alaskaSpecLib_spectra<-alaskaSpecLib[,c(-1:-7)]

##convert to a .rds file....Spectral object with 1975 samples with spectral range of 350-2500nm
alaskaSpecLib<-spectrolab::as.spectra(alaskaSpecLib_spectra)
##str(alaskaSpecLib_spectra)

##bind metadata...final producct is a spectral object with 1975 samples with spectral range of 350-2500nm and  6 variables being metadata
meta(alaskaSpecLib)<-data.frame(alaskaSpecLib_meta, stringsAsFactors = FALSE)

##Now lets smooth and resample our new spectral library so we can perform other analysis later
alaskaSpecLib_smooth<-smooth(alaskaSpecLib)

#resampling every 5 and 10nm
alaskaSpecLib_smooth_05nm<-spectrolab::resample(alaskaSpecLib_smooth, seq (350,2500,5 ))
alaskaSpecLib_smooth_10nm<-spectrolab::resample(alaskaSpecLib_smooth, seq (350,2500,10))


##Now lest save our new spectral library and all the necessary objects that were created
saveRDS(alaskaSpecLib            ,"Outputs/1_Field_spec/1_Processing/alaskaSpecLib.rds"            )
saveRDS(alaskaSpecLib_df          ,"Outputs/1_Field_spec/1_Processing/alaskaSpecLib_df.csv"        )
saveRDS(alaskaSpecLib_smooth     ,"Outputs/1_Field_spec/1_Processing/alaskaSpecLib_smooth.rds"     )
saveRDS(alaskaSpecLib_smooth_05nm,"Outputs/1_Field_spec/1_Processing/alaskaSpecLib_smooth_05nm.rds")
saveRDS(alaskaSpecLib_smooth_10nm,"Outputs/1_Field_spec/1_Processing/alaskaSpecLib_smooth_10nm.rds")















