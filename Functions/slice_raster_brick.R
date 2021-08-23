#' Calculate the tile corner XY values in source raster/image grid
#'
#' @param source_n_rows Number of rows in source raster/image
#' @param source_n_cols Number of columns in source raster/image
#'
#' @param slice_n_rows Number of rows in each tile
#' @param slice_n_cols Number of columns in each tile
#'
#' @param slice_overlap Number of pixel overlap in adjacent tiles (in both X and Y directions)
#'
#' @param complete_image If TRUE and the tile size and overlap dimensions do not conform to
#'                       covering the entire source raster/image, an additional row and column
#'                       of tiles will be created that include the excluded pixels but do NOT
#'                       respect the overlap value. If FALSE and the dimensions do not conform,
#'                       the set of tiles will omit some pixels on the right and bottom side
#'                       of the source raster/image
#'
#' @return Data frame with corner cells of one tile in each row, containing
#' variables \['x0', 'x1', 'y0', 'y1'\] for start \(\*0\\) and stop \(\*1)
#' cell numbers in horizontal \(x\*\) and vertical \(y\*\) directions
#'
#' @export
#' @importFrom tidyr expand nesting
calc_slice_corners <- function(source_n_rows, source_n_cols,
                               slice_n_rows, slice_n_cols,
                               slice_overlap = 0, complete_image = FALSE) {
  slice_row_over <- slice_n_rows - slice_overlap
  slice_col_over <- slice_n_cols - slice_overlap

  r1 <- seq(1, slice_row_over * (source_n_rows %/% slice_row_over), by = slice_row_over)
  r1 <- r1[r1 <= (source_n_rows - slice_n_rows + 1)]
  r2 <- r1 + (slice_n_rows - 1)

  if (complete_image && max(r2) < source_n_rows) {
    r1 <- c(r1, source_n_rows - slice_n_rows + 1)
    r2 <- c(r2, source_n_rows)
  }

  c1 <- seq(1, slice_col_over * (source_n_cols %/% slice_col_over), by = slice_col_over)
  c1 <- c1[c1 <= (source_n_cols - slice_n_cols + 1)]
  c2 <- c1 + (slice_n_cols - 1)

  if (complete_image && max(c2) < source_n_rows) {
    c1 <- c(c1, source_n_cols - slice_n_cols + 1)
    c2 <- c(c2, source_n_cols)
  }

  corners_df <- data.frame(x0 = c1, x1 = c2, y0 = r1, y1 = r2)
  corners_df <- tidyr::expand(
    corners_df,
    tidyr::nesting(x0, x1),
    tidyr::nesting(y0, y1)
  )

  corners_df <- corners_df[order(corners_df$y0, corners_df$x0),]

  if (is.na(NROW(corners_df)) || NROW(corners_df) == 0) {
    stop("Error defining tile corners...please investigate.")
  }

  corners_df
}

#' Split a raster brick into tiles with user-specified pixel dimensions
#'
#' \code{split_brick_px} divides a RasterBrick into an arbitrary number of pieces (tiles).
#'
#' This function is parallel-aware, using the same mechanism as used in the \code{raster}
#' package. Specifically, if you start a cluster using \code{\link{beginCluster}}, then
#' this function will automatically use that cluster. It is always a good
#' idea to stop the cluster when finished, using \code{\link{endCluster}}.
#'
#' @param brick   The RasterBrick to be split.
#'
#' @param tile_w_px The width of the output tiles in number of pixels/cells.
#'
#' @param tile_h_px The height of the output tiles in number of pixels/cells.
#'
#' @param overlap The number of pixels images should overlap. Default of 0 means no image
#'                overlap (i.e., no two tiles will contain the same pixel from the source image).
#'                The same overlap value will be used in both x and y directions.
#'
#' @param complete_image If TRUE and the tile size and overlap dimensions do not conform to
#'                       covering the entire source RasterBrick, an additional row and column
#'                       of tiles will be created that include the excluded pixels but do NOT
#'                       respect the overlap value. If FALSE and the dimensions do not conform,
#'                       the set of tiles will omit some pixels on the right and bottom side
#'                       of the source RasterBrick.
#'
#' @param path    Character specifying the directory to which the split tiles will be saved.
#'                If missing, the function will store the tiles in memory. If the path does
#'                not exist, it will be created.
#'
#' @param file_ext Extension for image file type of saved tiles.
#'
#' @param d_type  Datatype of the tiles. Defaults to INT1U. See \code{\link[raster]{dataType}}
#'                for additional information.
#'
#' @param write_only If TRUE, tiles will not be retained in memory.
#'
#' @param write_options File writing options passed to \code{\link[raster]{writeRaster}}'s
#'                      \code{options} argument.
#'
#' @param cl      A cluster object. Optional. This would generally be created using
#'                parallel::makeCluster or equivalent.
#'
#' @return \code{split_brick_px} returns either a list of cropped raster brick tiles or,
#'         if \code{write_only = TRUE}, a list containing \code{TRUE}, an error message,
#'         or a warning message depending on whether the tile was successfully written to
#'         file. The list's length will be equal to the number of tiles, determined by
#'         the source image width/height in pixels and the \code{out_x_px}, \code{out_y_px},
#'         and \code{overlap} argument values.
#'
#' @export
#' @importFrom pbapply pblapply
#' @importFrom raster crop crs<- extent writeRaster nrow ncol nlayers dropLayer
#' @importFrom stringr str_match
slice_raster_brick <- function(brick, tile_w_px = 50L, tile_h_px = 50L, overlap = 0L,
                           complete_image = TRUE, path = NULL, file_ext = "tif",
                           d_type = "INT1U", write_only = FALSE,
                           write_options = c('TFW=YES'), cl = NULL) {
  if (!is.numeric(tile_w_px) || !is.numeric(tile_h_px) || !is.numeric(overlap)) {
    stop("tile_w_px, tile_h_px, and overlap must be numeric")
  }

  if (!is.integer(tile_w_px)) tile_w_px <- as.integer(tile_w_px)
  if (!is.integer(tile_h_px)) tile_h_px <- as.integer(tile_h_px)
  if (!is.integer(overlap)) overlap <- as.integer(overlap)

  if (!is.null(path)) {
    exists <- dir.exists(path)
    if (!exists) dir.create(file.path(path), recursive = TRUE)
  }

  if (!is.null(cl) && !("cluster" %in% class(cl))) {
    stop("argument `cl` must have class `cluster`")
  }

  if (length(overlap) > 1) {
    warning("argument `overlap` contains more than 1 element - only the first will be used.")
    overlap <- overlap[1]
  }

  if (nlayers(brick) == 4) {
    brick <- dropLayer(brick, 4)
  }

  brick_rows <- nrow(brick)
  brick_cols <- ncol(brick)

  cell_df <- calc_tile_corners(source_n_rows = brick_rows,
                               source_n_cols = brick_cols,
                               tile_n_rows = tile_h_px,
                               tile_n_cols = tile_w_px,
                               tile_overlap = overlap,
                               complete_image = complete_image)

  ext <- split(cell_df, 1:NROW(cell_df))

  cat("Calculating extents:")
  ext <- pblapply(ext, function(x) {
    extent(brick, x[["y0"]], x[["y1"]], x[["x0"]], x[["x1"]])
  })

  crop_fun <- function(i, extents, brick, path = NULL, d_type, write_options) {

    brick_cropped <- crop(brick, extents[[i]], datatype = d_type)
    crs(brick_cropped) <- crs(brick)

    if (is.null(path)) {
      return(brick_cropped)
    } else {
      filename <- file.path(path, paste0(
        unique(stringr::str_match(names(brick), "X(.*?)\\..*?")[, 2]),
        "_tile",
        i,
        ".", file_ext)

      )

      out <- tryCatch({
        writeRaster(brick_cropped, filename, overwrite = TRUE,
                    datatype = d_type, options = write_options)
      },
      warning = function(w) return(w),
      error = function(e) return(e)
      )

      if (write_only) {
        if (class(out) %in% c("warning", "error")) {
          out
        } else {
          TRUE
        }
      } else {
        out
      }
    }
  }

  cat("Creating tiles:")
  pblapply(cl = cl,
           X = seq_along(ext), fun = crop_fun, extents = ext,
           brick = brick, path = path, d_type = d_type, write_options = write_options)

  NULL
}