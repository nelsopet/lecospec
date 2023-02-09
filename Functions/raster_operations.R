
#' creates tiles from raster brick and returns the locations on disk
#'
#' Long Description here
#'
#' @return 
#' @param raster_obj: a rasterBrick object (RasterLayer and RasterStack should also work but are not tested) 
#' @param num_x: number of tiles in x direction
#' @param num_y: number of tiles in y direction
#' @param save_path: folder to save the tiles
#' @param cluster: parallel compute cluster (e.g. from parallel::beginCluster()), or NULL
#' @param verbose: (boolean, default: false) determines whether details are printed during evaluation 
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
make_tiles <- function(
    raster_obj,
    num_x = 10,
    num_y = 10,
    save_path = "./", 
    cluster = NULL,
    verbose = FALSE) {

    if(verbose){
        print("Creating tile templates")
    }
    tiles <- SpaDES.tools::splitRaster(
        raster_obj[[1]], 
        nx=num_x,
        ny = num_y, 
        path = save_path)

    gc()

    if(verbose){
        print("Templates created, cropping raster")
    }


    get_random_filename <- function() {
        random_tile_id <- stringi::stri_rand_strings(1,16)
        random_filename <- paste0("tile_", random_tile_id, ".envi")
        return(random_filename)
    }


    crop_and_save_raster <- function(tile) {
        tile_filename <- file.path(save_path, get_random_filename())
            raster::crop(
                raster_obj, 
                tile,
                filename=tile_filename
            )
            gc()
            return(tile_filename)
    }

    tile_filenames <- list()

    if(!is.null(cluster)){
        if(verbose){
            print("Processing using supplied cluster")
        }
        tile_filenames <- parallel::parLapply(cluster, tiles, crop_and_save_raster)
    } else {
        if(verbose){
            print("Processing sequentially")
        }
        tile_filenames <- lapply(tiles, crop_and_save_raster)
    }

    if(verbose){
        print("complete")
    }
    gc()

    return(tile_filenames)
}


#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
handle_empty_tile <- function(tile_raster, save_path = NULL, target_crs = NULL){
    # convert to a raster
    print("Processing empty tile")
    output_raster <- tile_raster[[1]]# %>% raster::as.raster()

    
    if(!is.null(target_crs)){
        raster::crs(output_raster) <- target_crs
    }

    names(output_raster) <- c("predictions")
    raster::dataType(output_raster) <- "INT2U"
    
    print(raster::NAvalue(output_raster))
    if(!is.null(save_path)){
        raster::writeRaster(output_raster, save_path, datatype = "INT2U")
    }

    
      
    print("Empty Input Raster")
    print(tile_raster)
    print("Output Raster")
    print(output_raster)
    return(output_raster)
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
preprocess_raster_to_df <- function(raster_obj, model, band_names=NULL) {

    #unique_levels <- unique(raster::values(raster_obj))
    #mean_of_levels <- mean(unique_levels, na.rm = TRUE)

    filter_value <- mean(raster::values(raster_obj), na.rm = TRUE)# NA or 0 is bad

    if( filter_value == 0 || is.na(filter_value)){
        return(data.frame())
    }

    saved_names <- names(raster_obj)

   # imputed_raster <- raster::approxNA(
   #     raster_obj,
   #     rule = 1
   # )


    if(!is.null(band_names)){
        #try assigning the names to the bands (ignoring extras)
        try({
            names(raster_obj) <- band_names[1:length(names(raster_obj))]
        })
    } else {
        names(raster_obj) <- saved_names
    }

    #print(names(imputed_raster))

    #rm(raster_obj)

    df <- raster::rasterToPoints(raster_obj) %>% as.data.frame()
    if(nrow(df) < 1){
        #return df here as filtering fails later
        return (df)
    }
    print("Converted to Data frame?")
    print(is.data.frame(df))
    df <- remove_noisy_cols(df, max_index = 326) %>% as.data.frame()
    print("Noisy columns removed")
    print(is.data.frame(df))
    df <- filter_bands(df)
    #df <- filter_empty_points(df)
    print("Filtered")
    print(is.data.frame(df))
    gc()
    return(df)
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
datacube_to_csv <- function(raster_path, save_path) {
    datacube <- raster::brick(raster_path)
    df <- raster::rasterToPoints(datacube)
    write.csv(df, save_path)
}


# converts raster to CSV file on disk
preprocess_raster_to_csv <- function(
    raster_filepath,
    model,
    output_filepath = "./converted_data.csv") {

        raster_brick <- raster::brick(raster_filepath)
        df <- preprocess_raster_to_df(raster_brick)
        rm(raster_brick)
        gc()
    write.csv(
        df,
        output_filepath
    )
}

preprocess_raster_to_parquet <- function(
    raster_filepath,
    model_obj,
    output_filepath = "./converted_data.parquet") {
        raster_brick <- raster::brick(raster_filepath)
        df <- preprocess_raster_to_df(raster_brick)
        rm(raster_brick)
        gc()
        arrow::write_parquet(df, output_filepath)
}



#' loads the attribute table for the output (categorical) raster.  
#'
#' Gets the raster attribute table from the 
#'
#' @return 
#' @param aggregation: an integer that specifies the aggregation level for the output raster.  Should be 1 (functional group 1), 2 (functional group 2), 3 (genus), or 4 (species).
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
get_attribute_table <- function(aggregation) {
    attribute_table <- data.frame()
    if(aggregation == 1){
        attribute_table <- read.csv("assets/fg1RAT.csv")
    }
    if(aggregation == 2){
        attribute_table <- read.csv("assets/fg2RAT.csv")
        
    }
    if(aggregation == 3) {
        attribute_table <- read.csv("assets/genusRAT.csv")

    }
    if(aggregation == 4){
        attribute_table <- read.csv("assets/speciesRAT.csv")
    }
}



#' removes empty rows from the data.frame
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
merge_tiles_gdal <- function(
    target_files,
    output_filename,
    cache_vrt = "./temp_cache.vrt",
    return_raster
    ){
        vrt <- gdalUtils::gdalbuildvrt(target_files, cache_vrt, overwrite=TRUE)
        output <- gdalUtils::gdal_translate(
            cache_vrt,
            output_filename,
            ot = "UInt16",
            output_raster = return_raster)
    return(output)
}


#' removes empty rows from the data.frame
#'
#' Long Description here
#'
#' @return 
#' @param file_path: a string
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
calc_num_tiles <- function(file_path, max_size = 1024){
    file_size <- file.info(file_path)$size / (1024 * 1024)
    tile_size <- file_size / max_size
    num_xy <- ceiling(sqrt(tile_size))
    return(num_xy)
}

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
safe_merge <- function(raster_one, raster_two, target_crs = NULL){
    template <- raster::projectRaster(from = raster_two, to= raster_one, alignOnly=TRUE)
    #template is an empty raster that has the projected extent of r2 but is aligned with r1 (i.e. same resolution, origin, and crs of r1)
    r2_aligned <- raster::projectRaster(from = raster_two, to= template)
    return( 
        raster::merge(
            raster_one, 
            r2_aligned,
            datatype='INT2U',
            tolerance = 1.0        ) 
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
project_to_epsg <- function(raster_obj, epsg_code, categorical_raster = FALSE){
    target_wkt <- sf::st_crs(epsg_code)[[2]]
    target_crs <- sp::CRS(target_wkt)
    if(categorical_raster){
        return(raster::projectRaster(
            raster_obj,
            crs=target_crs,
            method="ngb"
        ))
    }
    return(raster::projectRaster(raster_obj, target_crs))
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
assemble_tiles_from_disk <- function(tiles, output_path){
    tiles = grep("Pred", tiles, value = TRUE)
    chunks<-lapply(
        tiles,
        function(x) {
            raster::raster(x)
        })
    pred_merged<-Reduce(raster::merge, chunks)
    #writeRaster(pred_merged, filename = "Output/prediction/")
    return(pred_merged)
}

#' 
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
merge_tiles <- function(input_files, output_path = NULL, target_layer = 1) {

    master_raster <- raster::raster(input_files[[1]])
    raster::dataType(master_raster) <- "INT2U"
    for (input_file in tail(input_files, -1)) {
        new_raster <- raster::raster(input_file)
        raster::dataType(new_raster) <- "INT2U"
        # above is robust against multi-layer images
        master_raster <- safe_merge(
            master_raster,
            new_raster
        )
    }
    if(!is.null(output_path)) {
        raster::writeRaster(master_raster, output_path, datatype='INT2U', overwrite = TRUE)
    }

    raster::dataType(master_raster) <- "INT2U"
    return(master_raster)
}

