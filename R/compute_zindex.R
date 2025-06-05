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

  x <- as.double(x)
  y <- as.double(y)
  z <- as.double(z)
  
  res <- numeric(length(x))
  for (i in 0:(max_coord_bits - 1L)) {
    mask <- bitwShiftL(1L, i)
    # Right-shift to get the i-th bit
    xi <- floor(bitwAnd(x, mask) / 2^i)
    yi <- floor(bitwAnd(y, mask) / 2^i)
    zi <- floor(bitwAnd(z, mask) / 2^i)
    
    # Left-shift using multiplication and add to result
    res <- res + xi * (2^(3 * i))
    res <- res + yi * (2^(3 * i + 1))
    res <- res + zi * (2^(3 * i + 2))
  }
  
  as.integer(res)
}

validate_max_coord_bits <- function(max_coord_bits) {
  if (!is.numeric(max_coord_bits) || length(max_coord_bits) != 1 ||
      is.na(max_coord_bits) || max_coord_bits != as.integer(max_coord_bits) ||
      max_coord_bits <= 0 || max_coord_bits >= 31) {
    stop("max_coord_bits must be a single positive integer less than 31")
  }
  max_coord_bits <- as.integer(max_coord_bits)
  max_coord_bits
}
