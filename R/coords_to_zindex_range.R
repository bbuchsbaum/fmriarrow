#' Convert coordinate ranges to Z-index range
#'
#' Given 0-based coordinate ranges for x, y, and z, compute the
#' minimum and maximum Morton (Z-order) index covering the cuboid
#' defined by the ranges. This is useful for translating ROI
#' queries into Z-index ranges for fast Parquet filtering.
#'
#' @param x_range,y_range,z_range Integer vectors of length 1 or 2
#'   specifying inclusive ranges in 0-based coordinates.
#' @param max_coord_bits Maximum number of bits used to represent
#'   each coordinate. Defaults to 10 (max coordinate 1023).
#'
#' @return A list with elements `min_zindex` and `max_zindex`.
#' @export
coords_to_zindex_range <- function(x_range, y_range, z_range,
                                   max_coord_bits = 10) {
  # Normalize coordinate ranges to length 2
  x_range <- normalize_coord_range(x_range)
  y_range <- normalize_coord_range(y_range)
  z_range <- normalize_coord_range(z_range)

  # Ensure coordinates fall within allowed bounds
  max_coord <- (2^max_coord_bits) - 1L
  validate_coord_bounds(c(x_range, y_range, z_range), max_coord)

  min_z <- compute_zindex(
    x_range[1],
    y_range[1],
    z_range[1],
    max_coord_bits = max_coord_bits
  )

  max_z <- compute_zindex(
    x_range[2],
    y_range[2],
    z_range[2],
    max_coord_bits = max_coord_bits
  )

  list(min_zindex = min_z, max_zindex = max_z)
}
