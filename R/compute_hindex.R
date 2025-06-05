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

# --- Bitwise Helper Functions ---

# Computes the binary reflected Gray code of an integer.
# gc(i) = i XOR (i >> 1)
gray_code <- function(i) {
  bitwXor(i, bitwShiftR(i, 1))
}

# Computes the inverse of the binary reflected Gray code.
gray_code_inverse <- function(g) {
  i <- g
  j <- 1
  while (bitwShiftL(1, j) <= i) {
    i <- bitwXor(i, bitwShiftR(g, j))
    j <- j + 1
  }
  i
}

# Counts the number of trailing set bits (1s) in an integer's binary representation.
# This corresponds to g(i) in the paper.
trailing_set_bits <- function(i) {
  i <- as.integer(i)
  if (i == 0) return(0)
  count <- 0
  while (i > 0 && bitwAnd(i, 1) == 1) {
    count <- count + 1
    i <- bitwShiftR(i, 1)
  }
  return(count)
}

# Helper for bit rotation.
right_rotate <- function(val, n, num_bits) {
  n <- n %% num_bits
  if (n == 0) return(val)
  mask <- bitwShiftL(1, n) - 1
  right_part <- bitwAnd(val, mask)
  left_part <- bitwShiftR(val, n)
  return(bitwOr(left_part, bitwShiftL(right_part, num_bits - n)))
}


# --- Hamilton's Hilbert State Helpers (for n=3) ---
N_DIMS <- 3

# Entry point for the ith sub-hypercube (Theorem 2.10, p.13)
# e(i) = gc(2 * floor(i/2))
e_i_lookup <- sapply(0:7, function(i) gray_code(2 * floor(i/2)))
e_i <- function(i) e_i_lookup[i + 1]


# Intra-sub-hypercube direction for ith sub-hypercube (Theorem 2.9, p.12)
# d(0) = 0
# d(i) = g(i) if i is odd
# d(i) = g(i-1) if i is even and > 0
g_i_lookup <- sapply(0:7, trailing_set_bits)
d_i_lookup <- sapply(0:7, function(i) {
  if (i == 0) return(0)
  if (i %% 2 == 1) return(g_i_lookup[i + 1])
  if (i %% 2 == 0) return(g_i_lookup[i]) # g(i-1)
})
d_i <- function(i) d_i_lookup[i + 1]


# --- Main Hilbert Index Calculation (Algorithm 2, p.20) ---

#' Computes 3D Hilbert index based on Hamilton (2006)
#' @param x,y,z Integer vectors of 0-based coordinates.
#' @param max_coord_bits Number of bits per dimension.
#' @return A numeric vector of Hilbert indices.
#' @export
compute_hindex <- function(x, y, z, max_coord_bits) {
  h_indices <- numeric(length(x))
  
  for (k in seq_along(x)) {
    cur_x <- x[k]; cur_y <- y[k]; cur_z <- z[k]
    
    h <- 0
    e <- 0
    d <- 0
    
    for (i in (max_coord_bits - 1):0) {
      # Step 1: Extract octant bit (l)
      b_x <- bitwAnd(bitwShiftR(cur_x, i), 1)
      b_y <- bitwAnd(bitwShiftR(cur_y, i), 1)
      b_z <- bitwAnd(bitwShiftR(cur_z, i), 1)
      l <- bitwOr(bitwShiftL(b_x, 2), bitwOr(bitwShiftL(b_y, 1), b_z))

      # Step 2: Transform l to l'
      l_prime <- right_rotate(bitwXor(l, e), d, N_DIMS)
      
      # Step 3: Calculate w = gc_inverse(l')
      w <- gray_code_inverse(l_prime)
      
      # Step 4: Append w to h
      h <- h * (2^N_DIMS) + w
      
      # Step 5: Update state (e, d)
      e <- bitwXor(e, right_rotate(e_i(w), d, N_DIMS))
      d <- (d + d_i(w) + 1) %% N_DIMS
    }
    h_indices[k] <- h
  }
  
  return(h_indices)
}

#' @param h_indices Numeric vector of Hilbert indices.
#' @param max_coord_bits Number of bits per dimension.
#' @return A data.frame with columns x, y, z.
#' @export
compute_hindex_inverse <- function(h_indices, max_coord_bits) {
  coords <- data.frame(x = integer(length(h_indices)),
                       y = integer(length(h_indices)),
                       z = integer(length(h_indices)))
                       
  for (k in seq_along(h_indices)) {
    h <- h_indices[k]
    
    x <- 0; y <- 0; z <- 0
    e <- 0
    d <- 0
    
    for (i in (max_coord_bits - 1):0) {
      # Extract 3-bit chunk (w) from h
      shift <- i * N_DIMS
      w <- floor(h / (2^shift)) %% 8
      
      # Inverse of Step 3: l' = gc(w)
      l_prime <- gray_code(w)
      
      # Inverse of Step 2: l = T_inverse(l')
      l <- bitwXor(right_rotate(l_prime, N_DIMS - d, N_DIMS), e)

      # Extract coordinate bits from l
      b_z <- bitwAnd(l, 1)
      b_y <- bitwAnd(bitwShiftR(l, 1), 1)
      b_x <- bitwAnd(bitwShiftR(l, 2), 1)
      
      # Append bits to coordinates
      x <- bitwOr(x, bitwShiftL(b_x, i))
      y <- bitwOr(y, bitwShiftL(b_y, i))
      z <- bitwOr(z, bitwShiftL(b_z, i))
      
      # Inverse of Step 5: Update state (e,d) - same as forward
      e <- bitwXor(e, right_rotate(e_i(w), d, N_DIMS))
      d <- (d + d_i(w) + 1) %% N_DIMS
    }
    coords$x[k] <- x
    coords$y[k] <- y
    coords$z[k] <- z
  }
  
  return(coords)
}

