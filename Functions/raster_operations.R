
#' creates tiles from raster brick and returns the locations on disk
#'
#' This function is intended to create tiles from large raster files
#' in the estimate_land_cover, but can be used outside it as well.  
#' The default location for this function to save the tiles is the 
#' current working directory.  This is likely to be altered in a 
#' future release for compliance with CRAN guidelines, either requiring
#' the location argument or using a temporary directory
#'
#' @param raster_obj: a rasterBrick object (RasterLayer and RasterStack should
#' also work but are not tested)
#' @param num_x: number of tiles in x direction
#' @param num_y: number of tiles in y direction
#' @param save_path: folder to save the tiles
#' @param cluster: parallel compute cluster (e.g. parallel::beginCluster()),
#' or NULL
#' @param verbose: (boolean, default: false) determines whether details are
#' printed during evaluation 
#' @return a character vector of filenames for the tiles. 
#' Note that these filenames do not include the base path
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


#' An empty tile handler for estimate_land_cover
#' 
#' This function handles empty tiles by creating a raster
#' that can be seamlessly mosaiced with the model predictions
#' but contains all NAs
#' 
#' This ensures that the spatial extent of the output matches
#' the spatial extent of the input, and that raster::mosaic 
#' works correctly
#' 
#' 
#' @param tile_raster a RasterBrick object
#' @param save_path (default NULL).  If provided, the output
#' raster will be saved to this location.  If no path is provided,
#' the file will not be saved.
#' @param target_crs (default NULL) If a CRS is provided, the output 
#' will have that CRS.  This is used primarily for handling the case
#' where the tile_raster CRS is missing or incomplete
#' @param no_data (default -Inf) The No Data value to be used.  
#' @return
#' @export
#' 
handle_empty_tile <- function(
    tile_raster, 
    save_path = NULL, 
    target_crs = NULL, 
    no_data = -Inf){
    # convert to a raster
    print("Processing empty tile")
    output_raster <- tile_raster[[1]]# %>% raster::as.raster()


    if(!is.null(target_crs)){
        raster::crs(output_raster) <- target_crs
    }

    names(output_raster) <- c("predictions")
    raster::dataType(output_raster) <- "INT2U"
    raster::NAvalue(output_raster) <- no_data
    raster::values(output_raster) <- NA

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




#' Performs preprocessing for `estimate_land_cover`
#'
#' Perfroms preprocessing to a raster object (RasterLayer, RasterBrick,
#'  or RasterStack).  This includes checking for empty tiles, adjusting 
#' band names, converting to a data.frame, filtering the columns, and
#' filtering the bands.  
#'
#' @param raster_obj: A raster::RasterLayer, raster::RasterBrick, or 
#' raster::RasterStack
#' @param model the model that will be used for inference (to extract metadata)
#' @param band_names (default NULL, optional) the list of band names for the 
#' raster
#' @return a dataframe of the pre-processed raster.
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
preprocess_raster_to_df <- function(raster_obj, model, band_names=NULL) {

    #unique_levels <- unique(raster::values(raster_obj))
    #mean_of_levels <- mean(unique_levels, na.rm = TRUE)

    filter_value <- mean(
        raster::values(raster_obj),
        na.rm = TRUE)# NA or 0 is bad

    if(filter_value == 0 || is.na(filter_value)) {
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


    df <- raster::rasterToPoints(raster_obj) %>% as.data.frame()
    if(nrow(df) < 1){
        #return df here as filtering fails later
        return (df)
    }
    print("Converted to Data frame?")
    print(is.data.frame(df))
    print(colnames(df))
    df <- remove_noisy_cols(df, max_index = 326) %>% as.data.frame()
    print("Noisy columns removed")
    print(is.data.frame(df))
    df <- filter_bands(df)
    print(colnames(df))

    #df <- filter_empty_points(df)
    print("Filtered")
    print(is.data.frame(df))
    gc()
    return(df)
}


#' Converts a multi-band raster to a CSV file
#'
#' A file conversion function that changes the format from 
#' a raster image (any format supported by the raster package)
#' to a csv file.
#'
#' @return 
#' @param raster_path: The file path to the raster file to be
#' converted
#' @param save_path the filepath where the CSV file should be saved
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
datacube_to_csv <- function(raster_path, save_path) {
    datacube <- raster::brick(raster_path)
    df <- raster::rasterToPoints(datacube)
    write.csv(df, save_path)
}


#' Pre-processes and converts a multi-band raster to a CSV file 
#'
#' The same as datacube_to_csv, but performs the same preprocessing used
#' in estimate_land_cover and preprocess_raster_to_df before writing it to 
#' disk.  
#' 
#' @return 
#' @param raster_path: The file path to the raster file to be converted
#' converted
#' @param model the model the data should be pre-processed to fit
#' @param save_path the filepath where the CSV file should be saved
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
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


#' Pre-processes and converts a multi-band raster to a parquet file 
#'
#' The same as datacube_to_csv, but performs the same preprocessing used
#' in estimate_land_cover and preprocess_raster_to_df before writing it to 
#' disk.  
#' 
#' @return 
#' @param raster_path: The file path to the raster file to be converted
#' converted
#' @param model_obj the model the data should be pre-processed to fit
#' @param save_path the filepath where the parquet file should be saved
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
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
#' Gets the raster attribute table from the raster fromn disk
#'
#' @return 
#' @param aggregation: an integer that specifies the aggregation level for the output 
#' raster.  Should be 1 (functional group 1), 2 (functional group 2), 
#' 3 (genus), or 4 (species).
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



#' Merges rasters into a single raster using GDAL
#'
#' The function combines multiple raster images into a 
#' single file using the Geospatial Data Analytics Library (GDAL).  
#' It uses the typical GDAL rasters to Virtual Raster Tiling (VRT)
#' to a user-provided format.
#'
#' @return 
#' @param target_files: vector of file names that should be combined
#' @param output_filename The filename for the merged image
#' @param cache_vrt the location to save the intermediate representation 
#' Defaults to a file named temp_cache.vrt in the local directory.  NOTE
#' this default is likely to change in a future release to meet CRAN
#' guidelines
#' @param return_raster A boolean value.  To get the result of the data
#' merge as a GDAL dataset (FALSE?) or raster::RasterBrick (TRUE?)
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


#' How many tiles are needed to meet a target size?
#'
#' This function determines the number of tiles the file should be 
#' divided into to have each tile be at most `max_size` megabytes.
#' It assumes equal number of divisions on X and Y
#' axes.
#'
#' @return a numnber (integer) representing the ideal number of 
#' x- and y- axis splits 
#' @param file_path: the file that should be divided
#' @param max_size (default 1024 MB = 1GB).  The maximum size of a 
#' tile.  
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
calc_num_tiles <- function(file_path, max_size = 1024){
    file_size <- file.info(file_path)$size / (1024 * 1024)
    tile_size <- file_size / max_size
    num_xy <- ceiling(sqrt(tile_size))
    return(num_xy)
}

#' Safely merges two rasters of different CRS 
#' 
#' Merges two rasters into a single file.  It does not require them to be 
#' of the same CRS or touching.  The rasters will be merged regardless.  
#' 
#' The output raster will be in the CRS of the first argument.  
#' This function allows a tolerance of 1 unit on the grid alignment
#' to allow for arifacts from projection.
#' 
#' @param raster_one The first raster to be merged.  
#' The CRS of this raster will be used for the output image
#' @param raster_two Will be merged with raster_one
#' @param target_crs depricated.  No longer used
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


#' Projects a raster object based on a given EPSG code
#' 
#' Reprojects a Raster object to a CRS based on the EPSG code
#' of that projection.  Currently uses only nearest-neighbor projection,
#' but in a future release may support linear interpolation.
#' 
#' @param raster_obj the RasterLayer, RasterBrick, or RasterStack to be
#' reprojected
#' @param epsg_code the EPSG code for the desired CRS
#' @param categorical_raster (default FALSE) a flag controlling whether 
#' the output is categorical or continuous.
#' @return the projected raster
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

#' merges multiple rasters into a single image safely
#' 
#' Uses sage_merge to merge multiple rasters (with potentially
#' different CRS and no shared boundary) into a single image.
#' 
#' 
#' 
#' @param input_files A list of input files to be merged
#' @param output_path the location where the output should be saved, or NULL
#' Default is NULL
#' @param target_layer Depricated (no longer used) (default 1)
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



FG1_TO_FGO_MAP <- matrix(
    c(# this makes one long vector, but it's a convenient way of slicing it
        # functional group 1 classes
        c(
            0,1,2,3,4,5,6,7,8,9
        ),
        # functional group 0 classes
        c(
            0,1,2,3,4,5,6,5,6,7
        )
    ),
    nrow = 10,
    ncol = 2
)


#' A fuction to change the taxonomic level of a output raster
#' 
#' Changes the predictions to a coarser taxonomic level
#' Currently only supports changing functional group 1 to 
#' functional group 0
#' 
#' @param ras the raster to reclassify
#' @param target_aggregation = 0 (should not be changed)
#' @param source_aggregation = 1 (should not be changed)
change_prediction_aggregation <- function(ras, target_aggregation = 0, source_aggregation = 1) {

    if(target_aggregation == 0){
        if(source_aggregation == 1){
            return(raster::reclassify(ras, FG1_TO_FGO_MAP))
        }
    }
}