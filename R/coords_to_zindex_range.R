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
  expand_range <- function(r) {
    if (length(r) == 1) r <- c(r, r)
    if (length(r) != 2) {
      stop("ranges must be length 1 or 2")
    }
    if (any(is.na(r))) {
      stop("ranges cannot contain NA")
    }
    r <- as.integer(r)
    if (r[1] > r[2]) {
      stop("range minimum must be <= maximum")
    }
    if (any(r < 0)) {
      stop("coordinates must be non-negative")
    }
    limit <- bitwShiftL(1L, max_coord_bits)
    if (any(r >= limit)) {
      stop("coordinates exceed range defined by max_coord_bits")
    }
    r
  }

  x_range <- expand_range(x_range)
  y_range <- expand_range(y_range)
  z_range <- expand_range(z_range)

  corners <- expand.grid(
    x = c(x_range[1], x_range[2]),
    y = c(y_range[1], y_range[2]),
    z = c(z_range[1], z_range[2])
  )
  zindices <- compute_zindex(corners$x, corners$y, corners$z,
                             max_coord_bits = max_coord_bits)
  list(min_zindex = min(zindices), max_zindex = max(zindices))
}
