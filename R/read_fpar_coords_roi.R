#' Read voxel data for a coordinate-based ROI
#'
#' Queries a Parquet file produced by `neurovec_to_fpar()` for voxels
#' within the specified coordinate ranges.
#'
#' @param parquet_path Path to the Parquet file.
#' @param x_range Vector of length 1 or 2 giving the x-coordinate range (0-based, inclusive).
#' @param y_range Vector of length 1 or 2 giving the y-coordinate range (0-based, inclusive).
#' @param z_range Vector of length 1 or 2 giving the z-coordinate range (0-based, inclusive).
#' @param columns Optional character vector of columns to return. If `NULL`, all
#'   columns are returned.
#' @param exact If `TRUE` (default), coordinates are filtered exactly after an
#'   initial Z-index range search. When `FALSE`, only the coarse Z-index
#'   filtering is applied which may include voxels outside the requested cuboid
#'   but can be faster for large ROIs.
#' @param max_coord_bits Maximum number of bits per coordinate (default: 10).
#'
#' @return A data.frame containing the filtered rows.
#' @export
read_fpar_coords_roi <- function(parquet_path, x_range, y_range, z_range,
                                 columns = NULL, exact = TRUE,
                                 max_coord_bits = 10) {
  validate_parquet_path(parquet_path)

  # Validate and normalize coordinate ranges
  x_range <- validate_coordinate_range(x_range, "x_range", max_coord_bits)
  y_range <- validate_coordinate_range(y_range, "y_range", max_coord_bits)
  z_range <- validate_coordinate_range(z_range, "z_range", max_coord_bits)

  # Convert coordinate ranges to Z-index range
  zindex_range <- coords_to_zindex_range(x_range, y_range, z_range, max_coord_bits)

  query_cols <- columns
  if (exact) {
    query_cols <- unique(c(query_cols, "x", "y", "z"))
  }

  data <- read_fpar_zindex_range(
    parquet_path,
    zindex_range$min_zindex,
    zindex_range$max_zindex,
    query_cols
  )

  if (exact) {
    data <- data |>
      dplyr::filter(
        x >= x_range[1] & x <= x_range[2] &
          y >= y_range[1] & y <= y_range[2] &
          z >= z_range[1] & z <= z_range[2]
      )

    if (!is.null(columns)) {
      data <- data |> dplyr::select(dplyr::all_of(columns))
    }
  }

  data
}
