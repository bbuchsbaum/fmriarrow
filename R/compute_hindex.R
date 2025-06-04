#' Compute 3D Hilbert index
#'
#' This initial implementation approximates a 3D Hilbert curve
#' by interleaving Gray-coded coordinate bits. For each bit
#' position `i`, the masked bit is shifted right by `i` before
#' being placed at bit position `3*i` (and offsets `+1` and `+2`)
#' in the final Morton-like code. Coordinates must be
#' non-negative integers and less than 2^`max_coord_bits`.
#'
#' @param x,y,z Integer vectors of equal length with 0-based
#'   voxel coordinates.
#' @param max_coord_bits Maximum number of bits used to represent
#'   each coordinate (default 10). Must be a positive integer less
#'   than 31.
#'
#' @return Integer vector of Hilbert indices.
#' @export
compute_hindex <- function(x, y, z, max_coord_bits = 10) {
  max_coord_bits <- validate_max_coord_bits(max_coord_bits)
  if (length(x) != length(y) || length(y) != length(z)) {
    stop("x, y and z must have the same length")
  }
  if (!all(x == as.integer(x)) || !all(y == as.integer(y)) || !all(z == as.integer(z))) {
    stop("coordinates must be integer-valued")
  }
  if (any(x < 0 | y < 0 | z < 0)) {
    stop("coordinates must be non-negative")
  }
  if (!is.numeric(max_coord_bits) || length(max_coord_bits) != 1 ||
      is.na(max_coord_bits) || max_coord_bits != as.integer(max_coord_bits) ||
      max_coord_bits <= 0 || max_coord_bits >= 31) {
    stop("max_coord_bits must be a single positive integer less than 31")
  }
  max_coord_bits <- as.integer(max_coord_bits)
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
    xi <- bitwShiftR(bitwAnd(x, mask), i)
    yi <- bitwShiftR(bitwAnd(y, mask), i)
    zi <- bitwShiftR(bitwAnd(z, mask), i)
    res <- bitwOr(res, bitwShiftL(xi, 3L * i))
    res <- bitwOr(res, bitwShiftL(yi, 3L * i + 1L))
    res <- bitwOr(res, bitwShiftL(zi, 3L * i + 2L))
  }
  res
}

