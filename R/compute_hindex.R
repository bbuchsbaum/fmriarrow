#' Compute 3D Hilbert index
#'
#' Compute the exact 3D Hilbert curve index for voxel coordinates.
#'
#' The implementation uses a C++ backend returning a 64-bit Hilbert
#' index (stored in a numeric vector). Coordinates must be
#' non-negative integers and less than `2^max_coord_bits`.
#'
#' @param x,y,z Integer vectors of equal length with 0-based
#'   voxel coordinates.
#' @param max_coord_bits Maximum number of bits used to represent
#'   each coordinate (default 10). Must be a positive integer between
#'   1 and 20.
#'
#' @return Integer vector of Hilbert indices.
#' @export
compute_hindex <- function(x, y, z, max_coord_bits = 10) {
  max_coord_bits <- validate_max_coord_bits(max_coord_bits)
  if (max_coord_bits > 20L) {
    stop("max_coord_bits must be between 1 and 20")
  }
  if (length(x) != length(y) || length(y) != length(z)) {
    stop("x, y and z must have the same length")
  }
  if (!all(x == as.integer(x)) || !all(y == as.integer(y)) || !all(z == as.integer(z))) {
    stop("coordinates must be integer-valued")
  }
  if (any(x < 0 | y < 0 | z < 0)) {
    stop("coordinates must be non-negative")
  }
  limit <- bitwShiftL(1L, max_coord_bits)
  if (any(x >= limit | y >= limit | z >= limit)) {
    stop("coordinates exceed range defined by max_coord_bits")
  }
  x <- as.integer(x)
  y <- as.integer(y)
  z <- as.integer(z)
  compute_hindex_cpp(x, y, z, max_coord_bits)
}

