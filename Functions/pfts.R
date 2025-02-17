#' Converts Functional Group 2 names to integer codes
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#' 
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_fg2_string <- function(df) {
    df_convert_results <- df %>% dplyr::mutate(z = case_when(
        z == "Litter" ~ 0L,
        z == "Mineral" ~ 1L,
        z == "FernAlly" ~ 2L,
        z == "ForbFlower" ~ 3L,
        z == "GraminoidGrass" ~ 4L,
        z == "GraminoidSedge" ~ 5L,
        z == "DarkTerrestrialMacrolichen" ~ 6L,
        z == "LightTerrestrialCrustose" ~ 7L,
        z == "LightTerrestrialMacrolichen" ~ 8L,
        z == "YellowTerrestrialCrustose" ~ 9L,
        z == "YellowTerrestrialMacrolichen" ~ 10L,
        z == "MossAcrocarp" ~ 11L,
        z == "MossPleurocarp" ~ 12L,
        z == "MossSphagnum" ~ 13L,
        z == "ShrubAlder" ~ 14L,
        z == "ShrubBetula" ~ 15L,
        z == "ShrubDecidOther" ~ 16L,
        z == "ShrubSalix" ~ 17L,
        z == "ShrubEvergreenNeedle" ~ 18L,
        z == "ShrubEvergreenBroadleaf" ~ 19L,
        z == "TreeBetula" ~ 20L,
        z == "TreeBroadleafOther" ~ 21L,
        z == "TreePopulus" ~ 22L,
        z == "TreeConiferOther" ~ 23L,
        z == "TreeSpruce" ~ 24L,
        z == "Unknown" ~ 25L
    ), .keep = "unused") %>%
    dplyr::select(x,y,z)
return(df_convert_results)
}



#' Converts functional group 2 codes from integers to Strings
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#'
#' @return 
#' @param df: A data.frame.  Assumes that the target data is in column 'z'
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_fg2_int <- function(df) {
    converted_df <- df %>% dplyr::mutate(
        z = dplyr::case_when(
            z == 0L ~ "Litter",
            z == 1L ~ "Mineral",
            z == 2L ~ "FernAlly",
            z == 3L ~ "ForbFlower",
            z == 4L ~ "GraminoidGrass",
            z == 5L ~ "GraminoidSedge",
            z == 6L ~ "DarkTerrestrialMacrolichen",
            z == 7L ~ "LightTerrestrialCrustose",
            z == 8L ~ "LightTerrestrialMacrolichen",
            z == 9L ~ "YellowTerrestrialCrustose",
            z == 10L ~ "YellowTerrestrialMacrolichen",
            z == 11L ~ "MossAcrocarp",
            z == 12L ~ "MossPleurocarp",
            z == 13L ~ "MossSphagnum",
            z == 14L ~ "ShrubAlder",
            z == 15L ~ "ShrubBetula",
            z == 16L ~ "ShrubDecidOther",
            z == 17L ~ "ShrubSalix",
            z == 18L ~ "ShrubEvergreenNeedle",
            z == 19L ~ "ShrubEvergreenBroadleaf",
            z == 20L ~ "TreeBetula",
            z == 21L ~ "TreeBroadleafOther",
            z == 22L ~ "TreePopulus",
            z == 23L ~ "TreeConiferOther",
            z == 24L ~ "TreeSpruce",
            z == 25L ~ "Unknown"

        ), .keep = "unused"
    ) %>% 
    dplyr::select(x,y,z)

    return(converted_df)
}



#' Converts Functional Group 1 names to integer codes
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#'
#' @return a dataframe identitical to df except with the column 
#' `z` mutated to the alternative representation of the functional group
#' @param df: A data.frame with a colum named `z`
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_fg1_string <- function(df) {
    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == "Abiotic" ~ 0L,
            z == "Forb" ~ 1L,
            z == "Graminoid" ~ 2L,
            z == "Lichen" ~ 3L,
            z == "Moss" ~ 4L,
            z == "ShrubDecid" ~ 5L,
            z == "ShrubEvergreen" ~ 6L,
            z == "TreeBroadleaf" ~ 7L,
            z == "TreeConifer" ~ 8L,
            z == "Unknown" ~ 9L
    ), .keep = "unused") %>%
    dplyr::select(x,y,z)

    return(converted_df)
}

#' Converts Functional Group 1 integer codes to names (characers/strings)
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_fg1_int <- function(df) {
    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == 0 ~ "Abiotic",
            z == 1 ~ "Forb",
            z == 2 ~ "Graminoid",
            z == 3 ~ "Lichen",
            z == 4 ~ "Moss",
            z == 5 ~ "ShrubDecid",
            z == 6 ~ "ShrubEvergreen",
            z == 7 ~ "TreeBroadleaf",
            z == 8 ~ "TreeConifer",
            z == 9 ~ "Unknown"
    ), .keep = "unused") %>% 
    dplyr::select(x,y,z)

    return(converted_df)
}


#' Converts Functional Group 0 integer codes to string names
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
convert_fg0_int <- function(df){
    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == 0 ~ "Abiotic",
            z == 1 ~ "BroadleafDecid",
            z == 2 ~ "ConiferEvergreen",
            z == 3 ~ "Forb",
            z == 4 ~ "Graminoid",
            z == 5 ~ "Lichen",
            z == 6 ~ "Moss",
            z == 7 ~ "Unknown"
        ), .keep = "unused"
    ) %>% dplyr::select(x,y,z)

    return(converted_df)
}


#' Converts Functional Group 1 names to integer codes
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
convert_fg0_string <- function(df){
    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == "Abiotic" ~ 0L,
            z == "BroadleafDecid" ~ 1L,
            z == "ConiferEvergreen" ~ 2L,
            z == "Forb" ~ 3L,
            z == "Graminoid" ~ 4L,
            z == "Lichen" ~ 5L,
            z == "Moss" ~ 6L,
            z == "Unknown" ~ 7L
    ), .keep = "unused") %>%
    dplyr::select(x,y,z)

    return(converted_df)
}

#' Converts species names to integer codes
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#'
#' @return a dataframe identitical to df except with the column 
#' `z` mutated to the alternative representation of the functional group
#' @param df: A data.frame with a colum named `z`
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_species_string <- function(df){

    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == "Dead Salix" ~ 0L,
            z == "Pices (bark)" ~ 1L,
            z == "Bare Rock" ~ 2L,
            z == "Bare Soil" ~ 3L,
            z == "Quartz" ~ 4L,
            z == "Equisetum arvense" ~ 5L,
            z == "Equisetum sylvaticum" ~ 6L,
            z == "Arenaria pseudofrigida" ~ 7L,
            z == "Heracleum lanatum" ~ 8L,
            z == "Hieracium sp." ~ 9L,
            z == "Iris sp." ~ 10L,
            z == "Lupinus sp." ~ 11L,
            z == "Pedicularis racemosa" ~ 12L,
            z == "Pedicularis sudetica" ~ 13L,
            z == "Petasites frigida" ~ 14L,
            z == "Pestasites frigidus" ~ 15L,
            z == "Saxifraga punctata" ~ 16L,
            z == "Toefeldia sp." ~ 17L,
            z == "Arctagrostis latifolia" ~ 18L,
            z == "Arctophila fulva" ~ 19L,
            z == "Calamogrostis sp." ~ 20L,
            z == "Dupontia fisheri" ~ 21L,
            z == "Carex sp." ~ 22L,
            z == "Carex aquatilis" ~ 23L,
            z == "Eriophorum vaginatum" ~ 24L,
            z == "Eriophorum angustifolium" ~ 25L,
            z == "Bryoria sp." ~ 26L,
            z == "Cetraria islandica" ~ 27L,
            z == "Cetraria laevigata" ~ 28L,
            z == "Masonhalea richardsonii" ~ 29L,
            z == "Melanelia sp." ~ 30L,
            z == "Peltigera apthosa" ~ 31L,
            z == "Peltigers leucophlebia" ~ 32L,
            z == "Peltigera malacea" ~ 33L,
            z == "Peltigera scabrata" ~ 34L,
            z == "Porpidia sp." ~ 35L,
            z == "Rhizocarpon sp." ~ 36L,
            z == "Umbilicaria arctica" ~ 37L,
            z == "Umbilicaria hyperborea" ~ 38L,
            z == "Icmadophila ericetorum" ~ 39L,
            z == "Pilophorus acicularis" ~ 40L,
            z == "Stereocaulon sp." ~ 41L,
            z == "Trapelopsis granulosa" ~ 42L,
            z == "Alectoria ochroleuca" ~ 43L,
            z == "Arctocetraria centrifuga" ~ 44L,
            z == "Asahinea chrysantha" ~ 45L,
            z == "Cladonia cornuta" ~ 46L,
            z == "Cladonia gracilis" ~ 47L,
            z == "Cladonia rangiferina" ~ 48L,
            z == "Cladonia stygia" ~ 49L,
            z == "Hypogymnia austerodes" ~ 50L,
            z == "Parmelia omphalodes" ~ 51L,
            z == "Parmelis sulcata" ~ 52L,
            z == "Unknown" ~ 53L,
            z == "Rhizocarpon geographicum" ~ 54L,
            z == "Cladonia amaurocraea" ~ 55L,
            z == "Cladonia mitis" ~ 56L,
            z == "Cladonia steallaris" ~ 57L,
            z == "Cladonia sulphurina" ~ 58L,
            z == "Cladonia uncialis" ~ 59L,
            z == "Dactylina arctica" ~ 60L,
            z == "Evernia mesomorpha" ~ 61L,
            z == "Flavocetraria cucculata" ~ 62L,
            z == "Flavocetraria nivalis" ~ 63L,
            z == "Nephroma arcticum" ~ 64L,
            z == "Parmeliopsis ambigua" ~ 65L,
            z == "Usnea lapponica" ~ 66L,
            z == "Usnea scabrata" ~ 67L,
            z == "Vulpicida pinastri" ~ 68L,
            z == "Aulacomnium palustre" ~ 69L,
            z == "Aulacomnium turgidum" ~ 70L,
            z == "Ceratadon purpureus" ~ 71L,
            z == "Dicranum sp." ~ 72L,
            z == "Plagiomnium sp." ~ 73L,
            z == "Polytrichum juniperinum" ~ 74L,
            z == "Polytrichum strictum" ~ 75L,
            z == "Polytrichum sp." ~ 76L,
            z == "Racomitrium lanoiginosum" ~ 77L,
            z == "Hylocomium splendens" ~ 78L,
            z == "Pleurozium schreberi" ~ 79L,
            z == "Rhytidum rugosum" ~ 80L,
            z == "Tomenthypnum nitens" ~ 81L,
            z == "Sphagnum sp." ~ 82L,
            z == "Sphagnum fuscum" ~ 83L,
            z == "Alnus sp." ~ 84L,
            z == "Betula nana" ~ 85L,
            z == "Arctostaphyllos" ~ 86L,
            z == "Rhus typhina" ~ 87L,
            z == "Rosa acicularis" ~ 88L,
            z == "Rubus sp." ~ 89L,
            z == "Vaccinium uliginosum" ~ 90L,
            z == "Salix alaxensis" ~ 91L,
            z == "Salix arbusculoides" ~ 92L,
            z == "Salix glauca" ~ 93L,
            z == "Salix lanata" ~ 94L,
            z == "Salix ovalifolia" ~ 95L,
            z == "Salix pulchra" ~ 96L,
            z == "Salix richardsonii" ~ 97L,
            z == "Salix (wooly)" ~ 98L,
            z == "Salix phlebophylla" ~ 99L,
            z == "Cassiope tetragona" ~ 100L,
            z == "Dryas sp." ~ 101L,
            z == "Empetrum nigrum" ~ 102L,
            z == "Ledum decumbens" ~ 103L,
            z == "Loisleuria procumbens" ~ 104L,
            z == "Vaccinium vitis-idea" ~ 105L,
            z == "Betula alleghaniensis" ~ 106L,
            z == "Betula neoalaskana" ~ 107L,
            z == "Betula papyrifera" ~ 108L,
            z == "Betula populifolia" ~ 109L,
            z == "Acer rubrum" ~ 110L,
            z == "Acer pensylvanicum" ~ 111L,
            z == "Fagus grandifolia" ~ 112L,
            z == "Fraxinus americana" ~ 113L,
            z == "Prunus pensylvanica" ~ 114L,
            z == "Quercus Rubra" ~ 115L,
            z == "Populus balsamifera" ~ 116L,
            z == "Populus grandidentata" ~ 117L,
            z == "Abies balsamea" ~ 118L,
            z == "Larix larcina" ~ 119L,
            z == "Pinus strobus" ~ 120L,
            z == "Thuja occidentalis" ~ 121L,
            z == "Tsuga canadensis" ~ 122L,
            z == "Picea mariana" ~ 123L,
            z == "Picea rubens" ~ 124L

        ), .keep = "unused"
    ) %>% dplyr::select(x,y,z)
    return(converted_df)
}

#' Converts species from integer codes to names
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#'
#' @return a dataframe identitical to df except with the column 
#' `z` mutated to the alternative representation of the functional group
#' @param df: A data.frame with a colum named `z`
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_species_int <- function(df){

    
    converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == 0 ~ "Dead Salix",
            z == 1 ~ "Pices (bark)",
            z == 2 ~ "Bare Rock",
            z == 3 ~ "Bare Soil",
            z == 4 ~ "Quartz",
            z == 5 ~ "Equisetum arvense",
            z == 6 ~ "Equisetum sylvaticum",
            z == 7 ~ "Arenaria pseudofrigida",
            z == 8 ~ "Heracleum lanatum",
            z == 9 ~ "Hieracium sp.",
            z == 10 ~ "Iris sp.",
            z == 11 ~ "Lupinus sp.",
            z == 12 ~ "Pedicularis racemosa",
            z == 13 ~ "Pedicularis sudetica",
            z == 14 ~ "Petasites frigida",
            z == 15 ~ "Pestasites frigidus",
            z == 16 ~ "Saxifraga punctata",
            z == 17 ~ "Toefeldia sp.",
            z == 18 ~ "Arctagrostis latifolia",
            z == 19 ~ "Arctophila fulva",
            z == 20 ~ "Calamogrostis sp.",
            z == 21 ~ "Dupontia fisheri",
            z == 22 ~ "Carex sp.",
            z == 23 ~ "Carex aquatilis",
            z == 24 ~ "Eriophorum vaginatum",
            z == 25 ~ "Eriophorum angustifolium",
            z == 26 ~ "Bryoria sp.",
            z == 27 ~ "Cetraria islandica",
            z == 28 ~ "Cetraria laevigata",
            z == 29 ~ "Masonhalea richardsonii",
            z == 30 ~ "Melanelia sp.",
            z == 31 ~ "Peltigera apthosa",
            z == 32 ~ "Peltigers leucophlebia",
            z == 33 ~ "Peltigera malacea",
            z == 34 ~ "Peltigera scabrata",
            z == 35 ~ "Porpidia sp.",
            z == 36 ~ "Rhizocarpon sp.",
            z == 37 ~ "Umbilicaria arctica",
            z == 38 ~ "Umbilicaria hyperborea",
            z == 39 ~ "Icmadophila ericetorum",
            z == 40 ~ "Pilophorus acicularis",
            z == 41 ~ "Stereocaulon sp.",
            z == 42 ~ "Trapelopsis granulosa",
            z == 43 ~ "Alectoria ochroleuca",
            z == 44 ~ "Arctocetraria centrifuga",
            z == 45 ~ "Asahinea chrysantha",
            z == 46 ~ "Cladonia cornuta",
            z == 47 ~ "Cladonia gracilis",
            z == 48 ~ "Cladonia rangiferina",
            z == 49 ~ "Cladonia stygia",
            z == 50 ~ "Hypogymnia austerodes",
            z == 51 ~ "Parmelia omphalodes",
            z == 52 ~ "Parmelis sulcata",
            z == 53 ~ "Unknown",
            z == 54 ~ "Rhizocarpon geographicum",
            z == 55 ~ "Cladonia amaurocraea",
            z == 56 ~ "Cladonia mitis",
            z == 57 ~ "Cladonia steallaris",
            z == 58 ~ "Cladonia sulphurina",
            z == 59 ~ "Cladonia uncialis",
            z == 60 ~ "Dactylina arctica",
            z == 61 ~ "Evernia mesomorpha",
            z == 62 ~ "Flavocetraria cucculata",
            z == 63 ~ "Flavocetraria nivalis",
            z == 64 ~ "Nephroma arcticum",
            z == 65 ~ "Parmeliopsis ambigua",
            z == 66 ~ "Usnea lapponica",
            z == 67 ~ "Usnea scabrata",
            z == 68 ~ "Vulpicida pinastri",
            z == 69 ~ "Aulacomnium palustre",
            z == 70 ~ "Aulacomnium turgidum",
            z == 71 ~ "Ceratadon purpureus",
            z == 72 ~ "Dicranum sp.",
            z == 73 ~ "Plagiomnium sp.",
            z == 74 ~ "Polytrichum juniperinum",
            z == 75 ~ "Polytrichum strictum",
            z == 76 ~ "Polytrichum sp.",
            z == 77 ~ "Racomitrium lanoiginosum",
            z == 78 ~ "Hylocomium splendens",
            z == 79 ~ "Pleurozium schreberi",
            z == 80 ~ "Rhytidum rugosum",
            z == 81 ~ "Tomenthypnum nitens",
            z == 82 ~ "Sphagnum sp.",
            z == 83 ~ "Sphagnum fuscum",
            z == 84 ~ "Alnus sp.",
            z == 85 ~ "Betula nana",
            z == 86 ~ "Arctostaphyllos",
            z == 87 ~ "Rhus typhina",
            z == 88 ~ "Rosa acicularis",
            z == 89 ~ "Rubus sp.",
            z == 90 ~ "Vaccinium uliginosum",
            z == 91 ~ "Salix alaxensis",
            z == 92 ~ "Salix arbusculoides",
            z == 93 ~ "Salix glauca",
            z == 94 ~ "Salix lanata",
            z == 95 ~ "Salix ovalifolia",
            z == 96 ~ "Salix pulchra",
            z == 97 ~ "Salix richardsonii",
            z == 98 ~ "Salix (wooly)",
            z == 99 ~ "Salix phlebophylla",
            z == 100 ~ "Cassiope tetragona",
            z == 101 ~ "Dryas sp.",
            z == 102 ~ "Empetrum nigrum",
            z == 103 ~ "Ledum decumbens",
            z == 104 ~ "Loisleuria procumbens",
            z == 105 ~ "Vaccinium vitis-idea",
            z == 106 ~ "Betula alleghaniensis",
            z == 107 ~ "Betula neoalaskana",
            z == 108 ~ "Betula papyrifera",
            z == 109 ~ "Betula populifolia",
            z == 110 ~ "Acer rubrum",
            z == 111 ~ "Acer pensylvanicum",
            z == 112 ~ "Fagus grandifolia",
            z == 113 ~ "Fraxinus americana",
            z == 114 ~ "Prunus pensylvanica",
            z == 115 ~ "Quercus Rubra",
            z == 116 ~ "Populus balsamifera",
            z == 117 ~ "Populus grandidentata",
            z == 118 ~ "Abies balsamea",
            z == 119 ~ "Larix larcina",
            z == 120 ~ "Pinus strobus",
            z == 121 ~ "Thuja occidentalis",
            z == 122 ~ "Tsuga canadensis",
            z == 123 ~ "Picea mariana",
            z == 124 ~ "Picea rubens"
        ), .keep = "unused") %>%
        dplyr::select(x,y,z)
    return(converted_df)

}


#' Converts genuc integer codes to names
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#'
#' @return a dataframe identitical to df except with the column 
#' `z` mutated to the alternative representation of the functional group
#' @param df: A data.frame with a colum named `z`
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_genus_int <- function(df) {
        converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == 0 ~ "LeafLitter",
            z == 1 ~ "Wood",
            z == 2 ~ "Rock",
            z == 3 ~ "Soil",
            z == 4 ~ "Equisetum",
            z == 5 ~ "Arenia",
            z == 6 ~ "Heracleum",
            z == 7 ~ "Hieracium",
            z == 8 ~ "Iris",
            z == 9 ~ "Lupinus",
            z == 10 ~ "Pedicularis",
            z == 11 ~ "Petasites",
            z == 12 ~ "Saxifraga",
            z == 13 ~ "Toefeldia",
            z == 14 ~ "Arctagrostis",
            z == 15 ~ "Arctophila",
            z == 16 ~ "Calamagrostis",
            z == 17 ~ "Dupontia",
            z == 18 ~ "Carex",
            z == 19 ~ "Eriophorum",
            z == 20 ~ "Bryoria",
            z == 21 ~ "Cetraria",
            z == 22 ~ "Masonhalea",
            z == 23 ~ "Melanelia",
            z == 24 ~ "Peltigera",
            z == 25 ~ "Porpidia",
            z == 26 ~ "Rhizocarpon",
            z == 27 ~ "Umbilicaria",
            z == 28 ~ "Icmadophila",
            z == 29 ~ "Pilophorus",
            z == 30 ~ "Stereocaulon",
            z == 31 ~ "Trapeliopsis",
            z == 32 ~ "Alectoria",
            z == 33 ~ "Arctocetraria",
            z == 34 ~ "Asahinea",
            z == 35 ~ "Cladonia",
            z == 36 ~ "Hypogymnia",
            z == 37 ~ "Parmelia",
            z == 38 ~ "Unknown",
            z == 39 ~ "Dactylina",
            z == 40 ~ "Evernia",
            z == 41 ~ "Flavocetraria",
            z == 42 ~ "Nephroma",
            z == 43 ~ "Parmeliopsis",
            z == 44 ~ "Usnea",
            z == 45 ~ "Vulpicida",
            z == 46 ~ "Aulacomnium",
            z == 47 ~ "Ceratadon",
            z == 48 ~ "Dicranum",
            z == 49 ~ "Plagiomnium",
            z == 50 ~ "Polytrichum",
            z == 51 ~ "Racomitrium",
            z == 52 ~ "Hylocomnium",
            z == 53 ~ "Pleurozium",
            z == 54 ~ "Rhytidium",
            z == 55 ~ "Tomenthypnum",
            z == 56 ~ "Sphagnum",
            z == 57 ~ "Alnus",
            z == 58 ~ "Betula",
            z == 59 ~ "Arctostaphyllos",
            z == 60 ~ "Rhus",
            z == 61 ~ "Rosa",
            z == 62 ~ "Rubus",
            z == 63 ~ "Vaccinium",
            z == 64 ~ "Salix",
            z == 65 ~ "Cassiope",
            z == 66 ~ "Dryas",
            z == 67 ~ "Empetrum",
            z == 68 ~ "Ledum",
            z == 69 ~ "Loisleuria",
            z == 70 ~ "Acer",
            z == 71 ~ "Fagus",
            z == 72 ~ "Prunus",
            z == 73 ~ "Quercus",
            z == 74 ~ "Populus",
            z == 75 ~ "Abies",
            z == 76 ~ "Larix",
            z == 77 ~ "Pinus",
            z == 78 ~ "Thuja",
            z == 79 ~ "Tsuga",
            z == 80 ~ "Picea",
            z == 81 ~ "nan"
        ), .keep = "unused"
    ) %>% dplyr::select(x,y,z)
    return(converted_df)
}


#' Converts genus names to integer codes
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#'
#' @return a dataframe identitical to df except with the column 
#' `z` mutated to the alternative representation of the functional group
#' @param df: A data.frame with a colum named `z`
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
convert_genus_string <- function(df) {
        converted_df <- df %>% dplyr::mutate(
        z = case_when(
            z == "LeafLitter" ~ 0L,
            z == "Wood" ~ 1L,
            z == "Rock" ~ 2L,
            z == "Soil" ~ 3L,
            z == "Equisetum" ~ 4L,
            z == "Arenia" ~ 5L,
            z == "Heracleum" ~ 6L,
            z == "Hieracium" ~ 7L,
            z == "Iris" ~ 8L,
            z == "Lupinus" ~ 9L,
            z == "Pedicularis" ~ 10L,
            z == "Petasites" ~ 11L,
            z == "Saxifraga" ~ 12L,
            z == "Toefeldia" ~ 13L,
            z == "Arctagrostis" ~ 14L,
            z == "Arctophila" ~ 15L,
            z == "Calamagrostis" ~ 16L,
            z == "Dupontia" ~ 17L,
            z == "Carex" ~ 18L,
            z == "Eriophorum" ~ 19L,
            z == "Bryoria" ~ 20L,
            z == "Cetraria" ~ 21L,
            z == "Masonhalea" ~ 22L,
            z == "Melanelia" ~ 23L,
            z == "Peltigera" ~ 24L,
            z == "Porpidia" ~ 25L,
            z == "Rhizocarpon" ~ 26L,
            z == "Umbilicaria" ~ 27L,
            z == "Icmadophila" ~ 28L,
            z == "Pilophorus" ~ 29L,
            z == "Stereocaulon" ~ 30L,
            z == "Trapeliopsis" ~ 31L,
            z == "Alectoria" ~ 32L,
            z == "Arctocetraria" ~ 33L,
            z == "Asahinea" ~ 34L,
            z == "Cladonia" ~ 35L,
            z == "Hypogymnia" ~ 36L,
            z == "Parmelia" ~ 37L,
            z == "Unknown" ~ 38L,
            z == "Dactylina" ~ 39L,
            z == "Evernia" ~ 40L,
            z == "Flavocetraria" ~ 41L,
            z == "Nephroma" ~ 42L,
            z == "Parmeliopsis" ~ 43L,
            z == "Usnea" ~ 44L,
            z == "Vulpicida" ~ 45L,
            z == "Aulacomnium" ~ 46L,
            z == "Ceratadon" ~ 47L,
            z == "Dicranum" ~ 48L,
            z == "Plagiomnium" ~ 49L,
            z == "Polytrichum" ~ 50L,
            z == "Racomitrium" ~ 51L,
            z == "Hylocomnium" ~ 52L,
            z == "Pleurozium" ~ 53L,
            z == "Rhytidium" ~ 54L,
            z == "Tomenthypnum" ~ 55L,
            z == "Sphagnum" ~ 56L,
            z == "Alnus" ~ 57L,
            z == "Betula" ~ 58L,
            z == "Arctostaphyllos" ~ 59L,
            z == "Rhus" ~ 60L,
            z == "Rosa" ~ 61L,
            z == "Rubus" ~ 62L,
            z == "Vaccinium" ~ 63L,
            z == "Salix" ~ 64L,
            z == "Cassiope" ~ 65L,
            z == "Dryas" ~ 66L,
            z == "Empetrum" ~ 67L,
            z == "Ledum" ~ 68L,
            z == "Loisleuria" ~ 69L,
            z == "Acer" ~ 70L,
            z == "Fagus" ~ 71L,
            z == "Prunus" ~ 72L,
            z == "Quercus" ~ 73L,
            z == "Populus" ~ 74L,
            z == "Abies" ~ 75L,
            z == "Larix" ~ 76L,
            z == "Pinus" ~ 77L,
            z == "Thuja" ~ 78L,
            z == "Tsuga" ~ 79L,
            z == "Picea" ~ 80L,
            z == "nan" ~ 81L,

                        ), .keep = "unused"
    ) %>% dplyr::select(x,y,z)
    return(converted_df)
}

#' Converts Functional Group 1 names to integer codes
#'
#' This function assumes that the data is stored in a column
#' named 'z'.  This is to seamlessly match the output of apply_model
#' and rasterToXYZ. 
#'
#' @return a dataframe identitical to df except with the column 
#' `z` mutated to the alternative representation of the functional group
#' @param df: A data.frame with a colum named `z`
#' @param aggregation_level (int/numeric, 0, 1, 2, 3, or 4)
#' @param to (default is "int") a character vector.  One of: "int" 
#' (aliases "Int" and "integer") or "string" (alias "String")
#' @export 
#' @examples Not Yet Implmented
convert_pft_codes <- function(df, aggregation_level, to = "int"){

    # list of functions for converting the data from strings to integers
    to_int = list(
        Functional_group0 = convert_fg0_string,
        Functional_group1 = convert_fg1_string,
        Functional_group2 = convert_fg2_string,
        Genus = convert_genus_string,
        Species = convert_species_string
        )

    # list of functions for converting from integers to strings
    to_string = list(
        Functional_group0 = convert_fg0_int,
        Functional_group1 = convert_fg1_int,
        Functional_group2 = convert_fg2_int,
        Genus = convert_genus_int,
        Species = convert_species_int
    )

    # split by conversion type, then use the appropriate function based on the aggregation level
    if( to == "int" || to =="Int" || to == "integer"){
        result <- to_int[[aggregation_level+1]](df)
        #result$z <- as.integer(result$z)
        return( result )
    } else if( to == "string" || to == "String") {
        return( to_string[[aggregation_level+1]](df) )
    } else {
        # catch invalid 'to' parameter
        stop(paste0("Cannot convert to type ", to))
    }
}


#' Builds a list for quickly converting between taxonomic groups
#' 
#' @param filepath The filepath of the CSV file of taxonomic levels
#' This file should have columns for each aggregation level, starting
#' with the coarsest classification (functional group 0, leftmost) to
#'  species (rightmost). 
#' 
#' @returns a list (tree-like) setup for fast indexing and
#' PFT conversion.  Can be readily read/written to JSON.
build_adjacency_list <- function(filepath) {
    df <- read.csv(filepath, header = TRUE)
    num_rows <- nrow(df)
    num_levels <- ncol(df)
    adjacency_list <- list()

    for(i in seq.int(num_rows)){
        for( j in seq.int(num_levels)){
            counter <- 1
            key <- df[[i,j]]
            values <- vector(mode = "character", length = num_levels)
            while(counter <= num_levels){
                if(counter < j){
                    values[[counter]] <- NA
                } else {
                    values[[counter]] <- df[[i, counter]]
                }
                counter <- counter + 1
            }
            if(!(key %in% names(adjacency_list))){
                adjacency_list[[key]] <- values[2:(num_levels)]
            }
        }
    }

    return(adjacency_list)
}

#' Converts functional group names to integer codes, and back
#' 
#' If provided a number (0, 1, 2, 3, or 4) it will return the
#' corresponding taxonomic level as a string
#' 
#' If provided a string, it will attempt to return the 
#' corresponding integer code (if the string is recognized)
#' 
#' @param pft The string or number to convert
#' @returns the Corresponding int or string
enum_pfts <- function(pft){
    if(pft == 0){
        return("Functional_Group0")
    }

    if(pft == 1){
        return("Functional_Group1")
    }
    if(pft == 2){
        return("Functional_Group2")
    }
    if(pft == 3){
        return("Genus")
    } 
    if(pft == 4){
        return("Species")
    } 

    if(pft == "Functional_Group0"){
        return(0)
    }
    if(pft == "Functional_Group1"){
        return(1)
    } 
    if(pft == "Functional_Group2"){
        return(2)
    }
    if( pft == "Genus"){
        return(3)
    } 
    if( pft == "Species"){
        return(4)
    }
}

# 

#' changes the taxonomic aggregation level
#'
#' Allows for the fast conversion to coarser taxonomic levels.  Note that 
#' there is no way for the function to get finer taxonomic levels. This
#' means that it is impossible to convert, for example, from functional group 0 
#' (the broadest categoties) to species (the finest category) but the reverse is
#' possible.  It also supports mixed levels (the input need not be of the same 
#' level).  
#' 
#' Data that cannot be converted due to insufficient taxonomic information
#' is given an NA value.  If the data in the prediction_vec is not recognized, 
#' the function will throw an error.  Note that the keys are case sensitive.
#' 
#'
#' @return 
#' @param prediction_vec a vector of classes
#' @param aggregation_level The desired aggregation level (0-4).  
#' @param aggregation_key the output of `build_adjacency_list`, or another 
#' list-based conversion key of a similar structure
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
change_aggregation <- function(prediction_vec, aggregation_level, aggregation_key){
    #print(prediction_vec)
    prediction_char <- prediction_vec %>% as.character()
    updated_predictions <- vector("character", length = length(prediction_vec))
    for(i in seq_along(prediction_vec)){
        aggregation_idx <- 5 - aggregation_level
        prediction <- prediction_char[[i]]
        #print(prediction)
        updated_predictions[[i]] <- aggregation_key[[prediction]][[aggregation_idx]]
    }

    return(updated_predictions)
}

