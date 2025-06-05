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
#' @param as_character Logical; if TRUE return indices as character strings to
#'   avoid precision loss for large values.
#'
#' @return Hilbert indices as either numeric or character vector depending on
#'   `as_character`.
#' @export
compute_hindex <- function(x, y, z, max_coord_bits = 10, as_character = FALSE,
                           implementation = c("cpp", "R")) {
  implementation <- match.arg(implementation)
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
  use_cpp <- identical(implementation, "cpp") &&
    exists("compute_hindex_cpp", mode = "function")
  if (use_cpp) {
    compute_hindex_cpp(x, y, z, max_coord_bits, as_character)
  } else {
    compute_hindex_reference(x, y, z, max_coord_bits, as_character)
  }
}

# Internal helper: rotate and flip a quadrant according to Hilbert rules
hilbert_rotate <- function(n, x, y, z, rx, ry, rz) {
  if (rz == 0L) {
    if (ry == 0L) {
      if (rx == 1L) {
        x <- n - 1L - x
        y <- n - 1L - y
      }
      tmp <- x; x <- y; y <- tmp
    } else {
      if (rx == 0L) {
        x <- n - 1L - x
        y <- n - 1L - y
      }
    }
    tmp <- y; y <- z; z <- tmp
  } else {
    if (ry == 1L) {
      if (rx == 1L) {
        x <- n - 1L - x
        y <- n - 1L - y
      }
      tmp <- x; x <- y; y <- tmp
    } else {
      if (rx == 0L) {
        x <- n - 1L - x
        y <- n - 1L - y
      }
    }
  }
  c(x, y, z)
}

# Reference implementation of the Hilbert index in pure R
hilbert3D_single_ref <- function(x, y, z, nbits) {
  idx <- 0
  n <- bitwShiftL(1L, nbits)
  for (i in seq(from = nbits - 1L, to = 0L)) {
    rx <- bitwAnd(bitwShiftR(x, i), 1L)
    ry <- bitwAnd(bitwShiftR(y, i), 1L)
    rz <- bitwAnd(bitwShiftR(z, i), 1L)
    digit <- bitwShiftL(rx, 2L) + bitwShiftL(ry, 1L) + rz
    idx <- idx * 8 + digit
    coords <- hilbert_rotate(n, x, y, z, rx, ry, rz)
    x <- coords[1]; y <- coords[2]; z <- coords[3]
  }
  idx
}

compute_hindex_reference <- function(x, y, z, max_coord_bits, as_character = FALSE) {
  res <- vapply(seq_along(x), function(i) {
    hilbert3D_single_ref(x[i], y[i], z[i], max_coord_bits)
  }, numeric(1))
  if (as_character) {
    format(res, scientific = FALSE, trim = TRUE)
  } else {
    res
  }
}

