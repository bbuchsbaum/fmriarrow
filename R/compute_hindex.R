#' Compute 3D Hilbert index
#'
#' This initial implementation approximates a 3D Hilbert curve
#' by interleaving Gray-coded coordinate bits. Coordinates must
#' be non-negative integers and less than 2^`max_coord_bits`.
#'
#' @param x,y,z Integer vectors of equal length with 0-based
#'   voxel coordinates.
#' @param max_coord_bits Maximum number of bits used to represent
#'   each coordinate (default 10).
#'
#' @return Integer vector of Hilbert indices.
#' @export
compute_hindex <- function(x, y, z, max_coord_bits = 10) {
  max_coord_bits <- validate_max_coord_bits(max_coord_bits)
  if (length(x) != length(y) || length(y) != length(z)) {
    stop("x, y and z must have the same length")
  }
  if (any(x < 0 | y < 0 | z < 0)) {
    stop("coordinates must be non-negative")
  }
  limit <- bitwShiftL(1L, max_coord_bits)
  if (any(x >= limit | y >= limit | z >= limit)) {
    stop("coordinates exceed range defined by max_coord_bits")
  }

  gray_code <- function(val) bitwXor(val, bitwShiftR(val, 1L))
  x <- as.integer(x)
  y <- as.integer(y)
  z <- as.integer(z)
  x <- gray_code(x)
  y <- gray_code(y)
  z <- gray_code(z)

  res <- integer(length(x))
  for (i in 0:(max_coord_bits - 1L)) {
    mask <- bitwShiftL(1L, i)
    res <- bitwOr(res, bitwShiftL(bitwAnd(x, mask), 3L * i))
    res <- bitwOr(res, bitwShiftL(bitwAnd(y, mask), 3L * i + 1L))
    res <- bitwOr(res, bitwShiftL(bitwAnd(z, mask), 3L * i + 2L))
  }
  res
}

