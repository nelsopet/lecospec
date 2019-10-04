library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

##Reads in spectra for each area sampled in Alaksa 
AK2018_spectra_smooth_005nm          <-readRDS("Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_smooth_005nm.rds")
bethelLib_spectra_smooth_005nm       <-readRDS("Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_smooth_005nm.rds")
brooksLib_spectra_smooth_005nm       <-readRDS("Processed_spec/Alaska_Summer_2018/brooksLib/brooksLib_spectra_smooth_005nm.rds")
Murph2_spectra_smooth_005nm          <-readRDS("Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_smooth_005nm.rds")
Murph_lib_spectra_smooth_005nm       <-readRDS("Processed_spec/Alaska_Summer_2018/Murph_lib/Murph_lib_spectra_smooth_005nm.rds")
yKDeltLib_spectra_smooth_005nm       <-readRDS("Processed_spec/Alaska_Summer_2018/yKDeltLib/yKDeltLib_spectra_smooth_005nm.rds")
TwelveMile_spectra_smooth_005nm      <-readRDS("Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_smooth_005nm.rds")
Big_Trail_Lake_spectra_smooth_005nm  <-readRDS("Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_smooth_005nm.rds")
Eagle_summit_spectra_smooth_005nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_smooth_005nm.rds")
Murphy_domeA_spectra_smooth_005nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeA/Murphy_domeA_spectra_smooth_005nm.rds")
Murphy_domeB_spectra_smooth_005nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_smooth_005nm.rds")
Wickersham_domeA_spectra_smooth_005nm<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeA/Wickersham_domeA_spectra_smooth_005nm.rds")
Wickersham_domeB_spectra_smooth_005nm<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_smooth_005nm.rds")

##Combines Spectral Datasets for all areas sampled
alaskaSpecLib_smooth_005nm<-Reduce(spectrolab::combine,list(AK2018_spectra_smooth_005nm           
                                              ,bethelLib_spectra_smooth_005nm        
                                              ,brooksLib_spectra_smooth_005nm        
                                              ,Murph2_spectra_smooth_005nm          
                                              ,Murph_lib_spectra_smooth_005nm       
                                              ,yKDeltLib_spectra_smooth_005nm       
                                              ,TwelveMile_spectra_smooth_005nm      
                                              ,Big_Trail_Lake_spectra_smooth_005nm  
                                              ,Eagle_summit_spectra_smooth_005nm    
                                              ,Murphy_domeA_spectra_smooth_005nm    
                                              ,Murphy_domeB_spectra_smooth_005nm    
                                              ,Wickersham_domeA_spectra_smooth_005nm
                                              ,Wickersham_domeB_spectra_smooth_005nm))

##convert spectral library to dataframe
alaskaSpecLib_smooth_005nm<-as.data.frame(alaskaSpecLib_smooth_005nm)

##Removes sample_ame column because it is redundant 
alaskaSpecLib_smooth_005nm$sample_name<-NULL

##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="gravel"]<-"Gravel"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_smooth_005nm$PFT_2[alaskaSpecLib_smooth_005nm$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="aultur"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="betneo"]<-"Tree"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="claama"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="claran"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="claste"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="equarv"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="herlan"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="irisit"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="luparc"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="masric"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="petfri"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="picmar"]<-"Tree"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="plesch"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="poljen"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="poljun"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="polstr"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="popbal"]<-"Tree"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="raclan"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="salala"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="salova"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="salric"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_smooth_005nm$PFT_3[alaskaSpecLib_smooth_005nm$PFT=="wooly_salix"]<-"Shrub"

##Creates new dataframe with a frequency column (shows the amount of scans per species within the library) 
alaskaSpecLib_smooth_005nm_freqTab<-as.data.frame(table(alaskaSpecLib_smooth_005nm$PFT_2))

##Combines the frequency dataframe with spectral library
alaskaSpecLib_smooth_005nm$Freq<-alaskaSpecLib_smooth_005nm_freqTab$Freq[match(alaskaSpecLib_smooth_005nm$PFT_2,alaskaSpecLib_smooth_005nm_freqTab$Var1)]

##Reorder columns
alaskaSpecLib_smooth_005nm<-alaskaSpecLib_smooth_005nm%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())

##Plants only
alaskaSpecLib_smooth_005nm_plants<-subset(alaskaSpecLib_smooth_005nm,PFT_3!="Abiotic")

##Creates dataframes that has all the species with scans greater than specified values (5,10,15,20)
alaskaSpecLib_smooth_005nm_plants_more05<-subset(alaskaSpecLib_smooth_005nm_plants,Freq>=5)
alaskaSpecLib_smooth_005nm_plants_more10<-subset(alaskaSpecLib_smooth_005nm_plants,Freq>=10)
alaskaSpecLib_smooth_005nm_plants_more15<-subset(alaskaSpecLib_smooth_005nm_plants,Freq>=15)
alaskaSpecLib_smooth_005nm_plants_more20<-subset(alaskaSpecLib_smooth_005nm_plants,Freq>=20)

##creates datafranme with equal amount of scans for each species (10,15,20 scans per species )
alaskaSpecLib_smooth_005nm_plants_equal05<-alaskaSpecLib_smooth_005nm_plants_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_smooth_005nm_plants_equal10<-alaskaSpecLib_smooth_005nm_plants_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_smooth_005nm_plants_equal15<-alaskaSpecLib_smooth_005nm_plants_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_smooth_005nm_plants_equal20<-alaskaSpecLib_smooth_005nm_plants_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##Extracts scans for each life form (lichen, bryophyte, vascular plant) for spectral library
#alaskaSpecLib_smooth_005nm_lichen     <-subset(alaskaSpecLib_smooth_005nm_plants,PFT_3=="Lichen")
#alaskaSpecLib_smooth_005nm_bryo       <-subset(alaskaSpecLib_smooth_005nm_plants,PFT_3=="Moss")
#alaskaSpecLib_smooth_005nm_lichen_bryo<-subset(alaskaSpecLib_smooth_005nm_plants,PFT_3=="Lichen"|PFT_3=="Moss")
#alaskaSpecLib_smooth_005nm_vascular   <-subset(alaskaSpecLib_smooth_005nm_plants,PFT_3!="Lichen"&PFT_3!="Moss")

###save spectral library 
#write.csv(alaskaSpecLib_smooth_005nm            ,"Processed_spec/alaskaSpecLib_smooth_005nm/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_2019_all.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_005nm_plants     ,"Processed_spec/alaskaSpecLib_smooth_005nm/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_2019_plants.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_005nm_lichen     ,"Processed_spec/alaskaSpecLib_smooth_005nm/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_2019_lichen.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_005nm_bryo       ,"Processed_spec/alaskaSpecLib_smooth_005nm/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_2019_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_005nm_lichen_bryo,"Processed_spec/alaskaSpecLib_smooth_005nm/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_2019_lichen_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_005nm_vascular   ,"Processed_spec/alaskaSpecLib_smooth_005nm/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_2019_vascular.csv",row.names = FALSE)

write.csv(alaskaSpecLib_smooth_005nm_plants_more05 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_plants_more05.csv")
write.csv(alaskaSpecLib_smooth_005nm_plants_more10 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_plants_more10.csv")
write.csv(alaskaSpecLib_smooth_005nm_plants_more15 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_plants_more15.csv")
write.csv(alaskaSpecLib_smooth_005nm_plants_more20 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_plants_more20.csv")
write.csv(alaskaSpecLib_smooth_005nm_plants_equal05,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_plants_equal05.csv")
write.csv(alaskaSpecLib_smooth_005nm_plants_equal10,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_plants_equal10.csv")
write.csv(alaskaSpecLib_smooth_005nm_plants_equal15,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_plants_equal15.csv")
write.csv(alaskaSpecLib_smooth_005nm_plants_equal20,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_005nm_plants_equal20.csv")
