#' Query a Parquet file for a cuboid ROI
#'
#' Retrieve BOLD data for a region specified by 0-based coordinate
#' ranges. The query uses the Z-index range derived from the
#' coordinates for fast filtering and optionally applies an exact
#' coordinate filter.
#'
#' @param parquet_path Path to the Parquet file created by
#'   [neurovec_to_fpar()].
#' @param x_range,y_range,z_range Integer vectors (length 1 or 2)
#'   giving inclusive coordinate ranges in 0-based space.
#' @param exact Logical; if `TRUE`, filter by coordinates after the
#'   initial Z-index range query.
#' @param columns Optional character vector of columns to return. If
#'   `NULL`, all columns are returned.
#'
#' @return An Arrow Table containing the selected rows.
#' @export
read_fpar_coords_roi <- function(parquet_path, x_range, y_range, z_range,
                                 exact = TRUE, columns = NULL) {
  if (!is.character(parquet_path) || length(parquet_path) != 1) {
    stop("parquet_path must be a single string")
  }
  if (!file.exists(parquet_path)) {
    stop("File does not exist: ", parquet_path)
  }
  if (!is.logical(exact) || length(exact) != 1) {
    stop("exact must be a single logical value")
  }
  if (!is.null(columns) && !is.character(columns)) {
    stop("columns must be NULL or a character vector")
  }

  zrange <- coords_to_zindex_range(x_range, y_range, z_range)

  cols_needed <- columns
  if (exact || is.null(columns)) {
    cols_needed <- unique(c(columns, "x", "y", "z", "zindex"))
  }

  tbl <- read_fpar_zindex_range(parquet_path, zrange$min_zindex,
                                zrange$max_zindex, columns = cols_needed)

  if (exact) {
    tbl <- tbl |>
      dplyr::filter(
        x >= as.integer(x_range[1]) & x <= as.integer(x_range[length(x_range)]) &
        y >= as.integer(y_range[1]) & y <= as.integer(y_range[length(y_range)]) &
        z >= as.integer(z_range[1]) & z <= as.integer(z_range[length(z_range)])
      )
  }

  if (!is.null(columns)) {
    tbl <- tbl |>
      dplyr::select(dplyr::all_of(columns))
  }

  tbl
}
