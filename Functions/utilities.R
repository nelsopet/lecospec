#' Function Reads in the data and replace/removes weird values
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
load_model <- function(filepath) {
    return(eval(parse(text = load(filepath))))
}


#' a quick function based on the original code
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
clean_names <- function(variables){
    return(
        stringr::str_remove_all(
            variables,
            "[[:punct:]]| "
            )
        )
}



#'
#' @export


update_filename <- function(prefix){
    if(!file.exists(prefix)){return(prefix)}
    i=1
    repeat {
       f = paste(prefix,i,sep=".")
       if(!file.exists(f)){
           return(f)
           }
       i=i+1
     }
}




ImgChopper <- function(img, quad) {
        tst_img <- brick(img)
        tst_quads <- readOGR(dsn = quad)
        tst_crop <- raster::crop(tst_img, tst_quads)
        tst_mask <- raster::mask(tst_crop, tst_quads)
        # tst_out<-c(tst_crop,tst_mask)
        return(tst_mask)
    }


TileAssembler <- function(dir, out) {
    tiles <- list.files(dir)
    tiles <- grep("Pred", tiles, value = TRUE)
    chunks <- lapply(1:length(tiles), function(x) {
        raster(paste(dir, "/", tiles[x], sep = ""))
    })
    pred_merged <- Reduce(merge, chunks)
    return(pred_merged)
}

# Note to self: use R JSON to save and load adjacency list
# write a function that uses the list and the value to get new value
# then an lapply to the df column to create new level column
# then assemble into a convenient wrapper

# should take predictions file and map it to target type as a data frame

# then aggregate the df
# score against hand-labeled data.
get_log_filename <- function(tile_path) {
    return( 
        new_filename <- gsub(
            ".envi",
            ".log", 
            c(tile_path),
            fixed = TRUE
            )[[1]]
    )
}

crs_from_epsg <- function(epsg_code) {
    target_wkt <- sf::st_crs(epsg_code)[[2]]
    target_crs <- sp::CRS(target_wkt)
}


#' a quick function based on the original code
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
.convert_tile_filename <- function(tile_path) {
    return( 
        new_filename <- gsub(
            "tile_",
            "prediction_", 
            c(tile_path),
            fixed = TRUE
            )[[1]]
    )
}