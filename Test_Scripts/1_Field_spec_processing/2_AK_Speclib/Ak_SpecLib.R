###############################################################Creates a A spectral library object of all the scans collected in Alaksa during 2018 and 2019########################################################
library(spectrolab)
library(tidyverse)

##Reads in a spectral object for each area sampled in Alaksa for the years 2018-2019...all object have bands from 350:2500nm and metadata being ScanaID,PFT and Area
AK2018_spectra          <-readRDS("Test_Outputs/1_Field_spec/1_Processing/AK2018_spectra.rds"          )##SPECTRAL OBJECT WITH 99 SAMPLES
bethelLib_spectra       <-readRDS("Test_Outputs/1_Field_spec/1_Processing/bethelLib_spectra.rds"       )##SPECTRAL OBJECT WITH 60 SAMPLES
brooksLib_spectra       <-readRDS("Test_Outputs/1_Field_spec/1_Processing/brooksLib_spectra.rds"       )##SPECTRAL OBJECT WITH 132 SAMPLES
Murph2_spectra          <-readRDS("Test_Outputs/1_Field_spec/1_Processing/Murph2_spectra.rds"          )##SPECTRAL OBJECT WITH 140 SAMPLES
Murph_lib_spectra       <-readRDS("Test_Outputs/1_Field_spec/1_Processing/Murph_lib_spectra.rds"       )##SPECTRAL OBJECT WITH 249 SAMPLES
yKDeltLib_spectra       <-readRDS("Test_Outputs/1_Field_spec/1_Processing/yKDeltLib_spectra.rds"       )##SPECTRAL OBJECT WITH 183 SAMPLES
TwelveMile_spectra      <-readRDS("Test_Outputs/1_Field_spec/1_Processing/TwelveMile_spectra.rds"      )##SPECTRAL OBJECT WITH 73 SAMPLES
Big_Trail_Lake_spectra  <-readRDS("Test_Outputs/1_Field_spec/1_Processing/Big_Trail_Lake_spectra.rds"  )##SPECTRAL OBJECT WITH 82 SAMPLES
Eagle_summit_spectra    <-readRDS("Test_Outputs/1_Field_spec/1_Processing/Eagle_summit_spectra.rds"    )##SPECTRAL OBJECT WITH 467 SAMPLES
Murphy_domeA_spectra    <-readRDS("Test_Outputs/1_Field_spec/1_Processing/Murphy_domeA_spectra.rds"    )##SPECTRAL OBJECT WITH 83 SAMPLES
Murphy_domeB_spectra    <-readRDS("Test_Outputs/1_Field_spec/1_Processing/Murphy_domeB_spectra.rds"    )##SPECTRAL OBJECT WITH 52 SAMPLES
Wickersham_domeA_spectra<-readRDS("Test_Outputs/1_Field_spec/1_Processing/Wickersham_domeA_spectra.rds")##SPECTRAL OBJECT WITH 132 SAMPLES
Wickersham_domeB_spectra<-readRDS("Test_Outputs/1_Field_spec/1_Processing/Wickersham_domeB_spectra.rds")##SPECTRAL OBJECT WITH 233 SAMPLES

##If we combine these spectral objects we should get a spectral library of 1985 samples
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
                                               ,Wickersham_domeB_spectra)) ## dim(n_samples=1985, n_wavelegths=2151)

##Now we want to convert our spectral object to a dataframe to build our spectral library...dataframe with 1985 rows and 2154 cols
alaskaSpecLib<-as.data.frame(alaskaSpecLib)%>%dplyr::select(-sample_name) ##dim (1985 2154)

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

###Add column PFT_3 (Courser response variables)
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="abibal"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="acerub"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="acepen"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aleoch"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="alnfru"]<-"Shrub_Alder"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="alninc"]<-"Shrub_Alder"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arccen"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcnig"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcrub"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcsta"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arctop"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="asachr"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aulpal"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aultur"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bare rock"]<-"Abiotic_Rock"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bare_soil"]<-"Abiotic_Soil"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betall"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betnan"]<-"Shrub_Other"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betneo"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betpap"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betpop"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bryoria"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="calcan"]<-"Graminoid_Grass"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carlin"]<-"Graminoid_Sedge"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carlyn"]<-"Graminoid_Sedge"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carram"]<-"Graminoid_Sedge"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="castet"]<-"Dwarf_Shrub_Needle"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cerpur"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cetisl"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cetlae"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claama"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clacor"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clacuc"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clagra"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clamit"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claran"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claste"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clasty"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clasul"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claunc"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dacarc"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dead salix"]<-"Abiotic_Litter"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dicranum"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryala"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryhyb"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryoct"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="empnig"]<-"Dwarf_Shrub_Needle"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="equarv"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="equsyl"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="erivag"]<-"Graminoid_Sedge"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="evemes"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="faggra"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="flacuc"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="flaniv"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="fraame"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="gravel"]<-"Abiotic_Rock"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="grey_rhizocarpon"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="herlan"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hylspl"]<-"Moss_Pleurocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hypaus"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hypspl"]<-"Moss_Pleurocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="icmeri"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="irisit"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="larlar"]<-"Tree_Needle"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="leddec"]<-"Shrub_Other"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="loipro"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="luparc"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="masric"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="melanelia"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="melhep"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="neparc"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="naparc"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="orange_Porpidia"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="paramb"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="paromp"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="parsul"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pedrac"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pedsud"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelapt"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelleu"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelmal"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelsca"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="petfri"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="picmar"]<-"Tree_Needle"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="picrub"]<-"Tree_Needle"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pilaci"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pinstr"]<-"Tree_Needle"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="plagiomnium"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popbal"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popgra"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="prupen"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="plesch"]<-"Moss_Pleurocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="poljen"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="poljun"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="polstr"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="polytrichum"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popbal"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="quartz"]<-"Abiotic_Rock"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="querub"]<-"Tree_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="raclan"]<-"Moss_Pleurocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhigeo"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhyrug"]<-"Moss_Acrocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhutyp"]<-"Shrub_Other"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rosaci"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rosasc"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rubcam"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rubcha"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salala"]<-"Shrub_Salix"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salarb"]<-"Shrub_Salix"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salgla"]<-"Shrub_Salix"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sallan"]<-"Shrub_Salix"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salova"]<-"Shrub_Salix"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="Salova"]<-"Shrub_Salix"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salpul"]<-"Shrub_Salix"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salric"]<-"Shrub_Salix"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sphagn"]<-"Moss_Sphagnum"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sphfus"]<-"Moss_Sphagnum"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="spruce bark"]<-"Abiotic_Litter"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="stepas"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="stetas"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="thuocc"]<-"Tree_Needle"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tomnit"]<-"Moss_Pleurocarp"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tragra"]<-"Lichen_Light"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tsucan"]<-"Tree_Needle"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="umbarc"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="umbhyp"]<-"Lichen_Dark"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="usnlap"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="usnsca"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vacvit"]<-"Shrub_Other"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vaculi"]<-"Dwarf_Shrub_Broad"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vulpin"]<-"Lichen_Yellow"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="wooly_salix"]<-"Shrub_Salix"

##Lets add more details to our spectral library by adding  frequency columns
##These frequency values represent the number of scans per species and the number of scans per functional group
##Lets start by creating a new dataframe with a frequency column for species and one for functional group
##Where PFT_2 represents species and PFT_3 represents functional groups
alaskaSpecLibPFT2_freqTab<-as.data.frame(table(alaskaSpecLib$PFT_2))##SPECIES FREQ TABLE
alaskaSpecLibPFT3_freqTab<-as.data.frame(table(alaskaSpecLib$PFT_3))##Func Group FREQ TABLE

###Please note these are the number of samples we have for each functional group
##The Dwarf shrub category is high because we scanned a high number of Dryas sp. in summer 2019, >700
#table(alaskaSpecLib$PFT_3)%>%as.data.frame()
#Abiotic_Litter     13
#Abiotic_Rock       37
#Abiotic_Soil       8
#Dwarf_Shrub_Broad  971
#Dwarf_Shrub_Needle 21
#Forb               80
#Graminoid_Grass    4
#Graminoid_Sedge    26
#Lichen_Dark        173
#Lichen_Light       90
#Lichen_Yellow      206
#Moss_Acrocarp      55
#Moss_Pleurocarp    23
#Moss_Sphagnum      12
#Shrub_Alder        44
#Shrub_Other        96
#Shrub_Salix        101
#Tree_Broad         8
#Tree_Needle        17

##Lets combine the frequency table with our spectral library...this adds two more columns to our spectral library
alaskaSpecLib$Freq1<-alaskaSpecLibPFT2_freqTab$Freq[match(alaskaSpecLib$PFT_2,alaskaSpecLibPFT2_freqTab$Var1)]
alaskaSpecLib$Freq2<-alaskaSpecLibPFT3_freqTab$Freq[match(alaskaSpecLib$PFT_3,alaskaSpecLibPFT3_freqTab$Var1)]

##Lets reorder the columns so our data is a little more structured...dim () 1985 2158 
alaskaSpecLib<-alaskaSpecLib%>%dplyr::select(ScanID,PFT,PFT_2,PFT_3,Freq1,Freq2,everything())

###Lets remove all the uncalibrated scans, but first we have to identify those scans
##Checck to see if there are any values greater than 2 or less than 0
tst<-lapply(alaskaSpecLib[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst$V1%>%range()
tst$V2%>%range()##Values here exceed 2, so we should remove them
tst<-subset(tst,V2>2)
#tst%>%View()

##now we have all the columns that have values greater than two, lets save those column names in an object
##We could probably create a better function here to extract those rows with values >2
##Here I created an object with all the columns that had values <2 or >0
badscans<-rownames(tst)
badscans<-c("1891"  ,"1892" ,"1893" ,"1894" ,"1895" ,"1896" ,"1897" ,"1898" ,"1899" ,"1900" ,"1901" ,"1902" ,"1908" ,"1909"
            ,"1916" ,"1917" ,"1930" ,"1931" ,"1932" ,"1938" ,"1939" ,"1940" ,"1947" ,"1948" ,"2480" ,"2481" ,"2482" ,"2490"
            ,"2491" ,"2492" ,"2493" ,"2497" ,"2498" ,"2499" ,"2500")


##Column names are saved, lets create a function that will will remove all those rows that have values greater than 2
####Need to come up with a better function
alaskaSpecLib<- alaskaSpecLib[apply(alaskaSpecLib[,badscans]<2, 1, all),]### dim 1917 2158, there were 5 rows with values >2

##Lets check the number of scans we have for each functional category now that we removed all the bad scans
#table(alaskaSpecLib$PFT_3)%>%as.data.frame()
#Abiotic_Litter     13
#Abiotic_Rock       37
#Abiotic_Soil       8
#Dwarf_Shrub_Broad  971
#Dwarf_Shrub_Needle 21
#Forb               80
#Graminoid_Grass    4
#Graminoid_Sedge    21 there were 5 bad scans of this group
#Lichen_Dark        173
#Lichen_Light       90
#Lichen_Yellow      206
#Moss_Acrocarp      55
#Moss_Pleurocarp    23
#Moss_Sphagnum      12
#Shrub_Alder        44
#Shrub_Other        96
#Shrub_Salix        101
#Tree_Broad         8
#Tree_Needle        17

###lets create a df from our spectral library to be saved
alaskaSpecLib_df<-alaskaSpecLib

#This creates a dataframe of the species and functional groups
#alaskaSpecLib_df_species<-alaskaSpecLib_df%>%dplyr::select(PFT,PFT_2,PFT_3)
#write.csv(alaskaSpecLib_df_species                 ,"Test_Outputs/1_Field_spec/1_Processing/alaskaSpecLib_df_species.csv"                  ,row.names = F)

####Lets run that test again just to make sure our function worked
tst<-lapply(alaskaSpecLib[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst$V2%>%range()##There are no weird values, those are values outside of 0 and 2

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

##Lets create dataframe with each functional group
###Need to eliminate this step and apply functional coding
Abiotic_Litter    <-subset(alaskaSpecLib_df,PFT_3=="Abiotic_Litter")
Abiotic_Rock      <-subset(alaskaSpecLib_df,PFT_3=="Abiotic_Rock")
Abiotic_Soil      <-subset(alaskaSpecLib_df,PFT_3=="Abiotic_Soil")
Dwarf_Shrub_Broad <-subset(alaskaSpecLib_df,PFT_3=="Dwarf_Shrub_Broad")
Dwarf_Shrub_Needle<-subset(alaskaSpecLib_df,PFT_3=="Dwarf_Shrub_Needle")
Forb              <-subset(alaskaSpecLib_df,PFT_3=="Forb")
Graminoid_Grass   <-subset(alaskaSpecLib_df,PFT_3=="Graminoid_Grass")
Graminoid_Sedge   <-subset(alaskaSpecLib_df,PFT_3=="Graminoid_Sedge")
Lichen_Dark       <-subset(alaskaSpecLib_df,PFT_3=="Lichen_Dark")
Lichen_Light      <-subset(alaskaSpecLib_df,PFT_3=="Lichen_Light")
Lichen_Yellow     <-subset(alaskaSpecLib_df,PFT_3=="Lichen_Yellow")
Moss_Acrocarp     <-subset(alaskaSpecLib_df,PFT_3=="Moss_Acrocarp")
Moss_Pleurocarp   <-subset(alaskaSpecLib_df,PFT_3=="Moss_Pleurocarp")
Moss_Sphagnum     <-subset(alaskaSpecLib_df,PFT_3=="Moss_Sphagnum")
Shrub_Alder       <-subset(alaskaSpecLib_df,PFT_3=="Shrub_Alder")
Shrub_Other       <-subset(alaskaSpecLib_df,PFT_3=="Shrub_Other")
Shrub_Salix       <-subset(alaskaSpecLib_df,PFT_3=="Shrub_Salix")
Tree_Broad        <-subset(alaskaSpecLib_df,PFT_3=="Tree_Broad")
Tree_Needle       <-subset(alaskaSpecLib_df,PFT_3=="Tree_Needle")

##Lets do a check
Species_table<-alaskaSpecLib_df %>%
  group_by(PFT_3) %>%
  count(PFT_2)

##lets create df
Abiotic_Litter_a    <-Species_table%>%subset(PFT_3=="Abiotic_Litter"    )
Abiotic_Rock_a      <-Species_table%>%subset(PFT_3=="Abiotic_Rock"      )
Abiotic_Soil_a      <-Species_table%>%subset(PFT_3=="Abiotic_Soil"      )
Dwarf_Shrub_Broad_a <-Species_table%>%subset(PFT_3=="Dwarf_Shrub_Broad" )
Dwarf_Shrub_Needle_a<-Species_table%>%subset(PFT_3=="Dwarf_Shrub_Needle")
Forb_a              <-Species_table%>%subset(PFT_3=="Forb"              )
Graminoid_Grass_a   <-Species_table%>%subset(PFT_3=="Graminoid_Grass"   )
Graminoid_Sedge_a   <-Species_table%>%subset(PFT_3=="Graminoid_Sedge"   )
Lichen_Dark_a       <-Species_table%>%subset(PFT_3=="Lichen_Dark"       )
Lichen_Light_a      <-Species_table%>%subset(PFT_3=="Lichen_Light"      )
Lichen_Yellow_a     <-Species_table%>%subset(PFT_3=="Lichen_Yellow"     )
Moss_Acrocarp_a     <-Species_table%>%subset(PFT_3=="Moss_Acrocarp"     )
Moss_Pleurocarp_a   <-Species_table%>%subset(PFT_3=="Moss_Pleurocarp"   )
Moss_Sphagnum_a     <-Species_table%>%subset(PFT_3=="Moss_Sphagnum"     )
Shrub_Alder_a       <-Species_table%>%subset(PFT_3=="Shrub_Alder"       )
Shrub_Other_a       <-Species_table%>%subset(PFT_3=="Shrub_Other"       )
Shrub_Salix_a       <-Species_table%>%subset(PFT_3=="Shrub_Salix"       )
Tree_Broad_a        <-Species_table%>%subset(PFT_3=="Tree_Broad"        )
Tree_Needle_a       <-Species_table%>%subset(PFT_3=="Tree_Needle"       )


###Lets make the PFT_2 group equal within each functinal group (PFT_3)
#######Need to find a better way to do this.........Apply functional coding here
Abiotic_Rock<- Abiotic_Rock%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(),median(Abiotic_Rock_a$n)))%>%
  as.data.frame() 

Dwarf_Shrub_Broad<-Dwarf_Shrub_Broad%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(), median(Dwarf_Shrub_Broad_a$n)))%>%
  as.data.frame()

Forb<-Forb%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(), median(Forb_a$n)))%>%
  as.data.frame()

Lichen_Dark<-Lichen_Dark%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(), median(Lichen_Dark_a$n)))%>%
  as.data.frame()

Lichen_Light<-Lichen_Light%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(), median(Lichen_Light_a$n)))%>%
  as.data.frame()

Lichen_Yellow<-Lichen_Yellow %>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(), median(Lichen_Yellow_a$n)))%>%
  as.data.frame()

Moss_Acrocarp<-Moss_Acrocarp%>%
  sample_n(pmin(n(), median(Moss_Acrocarp_a$n)))%>%
  as.data.frame() 

Moss_Pleurocarp<-Moss_Pleurocarp%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(), median(Moss_Pleurocarp_a$n)))%>%
  as.data.frame()

Shrub_Alder<-Shrub_Alder%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(), median(Shrub_Alder_a$n)))%>%
  as.data.frame()

Shrub_Other<-Shrub_Other%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(), median(Shrub_Other_a$n)))%>%
  as.data.frame()

Shrub_Salix<-Shrub_Salix%>%
  group_by(PFT_2)%>%
  sample_n(pmin(n(),median(Shrub_Salix_a$n)))%>%
  as.data.frame()

##Now lets combine the dataframes above and convert to a spectral object
alaskaSpecLib_reduced_df<-rbind(Abiotic_Litter    
                              ,Abiotic_Rock      
                              ,Abiotic_Soil      
                              ,Dwarf_Shrub_Broad 
                              ,Dwarf_Shrub_Needle
                              ,Forb              
                              ,Graminoid_Grass   
                              ,Graminoid_Sedge   
                              ,Lichen_Dark       
                              ,Lichen_Light      
                              ,Lichen_Yellow     
                              ,Moss_Acrocarp     
                              ,Moss_Pleurocarp   
                              ,Moss_Sphagnum     
                              ,Shrub_Alder       
                              ,Shrub_Other       
                              ,Shrub_Salix       
                              ,Tree_Broad        
                              ,Tree_Needle       )

##Lets do a check
alaskaSpecLib_reduced_df %>%
  group_by(PFT_3) %>%
  count(PFT_3)%>%View()

##Lets convert our new spectral library to a spectral object to ne used later 
alaskaSpecLib_reduced <-spectrolab::as.spectra(alaskaSpecLib_reduced_df[-1:-7])
meta(alaskaSpecLib_reduced)<-data.frame(alaskaSpecLib_reduced_df[1:7], stringsAsFactors = FALSE)###adds metadata


##Lets check the number of scans we have for each functional category now that we removed all the bad scans
#alaskaSpecLib_reduced_df %>%group_by(PFT_3) %>%count(PFT_3)
#Abiotic_Litter        13
#Abiotic_Rock          19
#Abiotic_Soil           8
#Dwarf_Shrub_Broad    101
#Dwarf_Shrub_Needle    21
#Forb                  51
#Graminoid_Grass        4
#Graminoid_Sedge       21
#Lichen_Dark          133
#Lichen_Light          65
#Lichen_Yellow        153
#Moss_Acrocarp          6
#Moss_Pleurocarp       14
#Moss_Sphagnum         12
#Shrub_Alder           44
#Shrub_Other           61
#Shrub_Salix           50
#Tree_Broad             8
#Tree_Needle           17




##Now lest save our new spectral library and all the necessary objects that were created
saveRDS(alaskaSpecLib                 ,"Test_Outputs/1_Field_spec/1_Processing/alaskaSpecLib.rds"                              )
saveRDS(alaskaSpecLib_reduced          ,"Test_Outputs/1_Field_spec/1_Processing/alaskaSpecLib_reduced.rds"                     )

write.csv(alaskaSpecLib_df          ,"Test_Outputs/1_Field_spec/1_Processing/alaskaSpecLib_df.csv"               ,row.names = F)
write.csv(alaskaSpecLib_reduced_df   ,"Test_Outputs/1_Field_spec/1_Processing/alaskaSpecLib_reduced_df.csv"      ,row.names = F)












