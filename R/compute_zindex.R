#' Compute 3D Z-order (Morton) index
#'
#' Interleaves bits of the x, y and z coordinates to create a
#' 32-bit unsigned integer Morton code. For each bit position `i`,
#' the corresponding masked bit is right-shifted to position 0 and
#' then left-shifted to bit position `3*i` (and `3*i + 1`, `3*i + 2`
#' for `y` and `z`). Coordinates must be non-negative integers and
#' less than 2^`max_coord_bits`.
#'
#' @param x,y,z Integer vectors of equal length with 0-based
#'   voxel coordinates.
#' @param max_coord_bits Maximum number of bits used to represent
#'   each coordinate (default 10).
#'   For volumes with dimensions exceeding 1024 voxels along any axis,
#'   set `max_coord_bits` accordingly (e.g., `ceiling(log2(max(dim)))`).
#'
#' @return Integer vector of the same length as `x` with the
#'   computed Morton codes.
#' @export
compute_zindex <- function(x, y, z, max_coord_bits = 10) {
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

  x <- as.integer(x)
  y <- as.integer(y)
  z <- as.integer(z)

  compute_zindex_cpp(x, y, z, max_coord_bits)

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
