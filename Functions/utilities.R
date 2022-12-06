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
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
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



#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
ImgChopper <- function(img, quad) {
        tst_img <- brick(img)
        tst_quads <- readOGR(dsn = quad)
        tst_quads<-sf::st_transform(sf::st_as_sf(tst_quads), crs(tst_img))
        tst_crop <- raster::crop(tst_img, tst_quads)
        tst_mask <- raster::mask(tst_crop, tst_quads)
        # tst_out<-c(tst_crop,tst_mask)
        return(tst_mask)
    }

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
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

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
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

build_columnwise_sensor_correction_model <- function(
    left_df, 
    right_df, 
    ignore_cols = NULL,
    verbose = TRUE
) {

    used_cols <- intersect(
        colnames(left_df),
        colnames(right_df)
    )

    if(!is.null(ignore_cols)){
        used_cols <- setdiff(used_cols, ignore_cols)
    }


    models  <- list()

    for(col in used_cols){
        left_vec <- sort(left_df[,col])
        right_vec <- sort(right_df[,col])
        if(is.numeric(left_vec) & is.numeric(right_vec)){
            model <- lm(
                left_vec~right_vec,
                )

            if(verbose){
                print(summary(model))
            }
            models[[col]] <- model
        }
    }

    return(models)
}

apply_sensor_correction_model <- function(
    models,
    data,
    ignore_cols = NULL
){
    used_cols <- colnames(data)
     if(!is.null(ignore_cols)){
        used_cols <- setdiff(used_cols, ignore_cols)
    }

    df_corrected <- as.data.frame(data)

    for(col in used_cols){
        if(!is.null(models[[col]])){
            print(paste0("Correcting ", col))
            model_intercept <- summary(models[[col]])$coefficients[1,1]
            model_slope <- summary(models[[col]])$coefficients[2,1]
            df_corrected[,col] <- model_intercept + (data[,col]*model_slope)
        }
    }

    return(df_corrected)
}