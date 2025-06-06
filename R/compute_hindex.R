#' @useDynLib fmriarrow, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Compute 3D Hilbert curve indices
#' 
#' @param x,y,z Integer vectors of coordinates
#' @param max_coord_bits Number of bits per dimension (e.g., 10 for a 1024x1024x1024 cube)
#' @return A numeric vector of Hilbert indices.
#' @export
compute_hindex <- function(x, y, z, max_coord_bits) {
  # Basic validation
  if (length(x) != length(y) || length(y) != length(z)) {
    stop("x, y and z must have the same length")
  }
  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(z)) {
    stop("coordinates must be numeric")
  }
   if (any(x < 0 | y < 0 | z < 0)) {
    stop("coordinates must be non-negative")
  }
  
  limit <- 2^max_coord_bits
  if (any(x >= limit | y >= limit | z >= limit)) {
    stop("coordinates exceed range defined by max_coord_bits")
  }

  compute_hindex_cpp(as.integer(x), as.integer(y), as.integer(z), as.integer(max_coord_bits))
}

#' Decode Hilbert curve indices back to coordinates
#'
#' @param h_indices A numeric vector of Hilbert indices
#' @param max_coord_bits Number of bits per dimension
#' @return A data.frame with columns x, y and z
#' @export
compute_hindex_inverse <- function(h_indices, max_coord_bits) {
  if (!is.numeric(h_indices)) {
    stop("h_indices must be a numeric vector")
  }
  compute_hindex_cpp_inverse(h_indices, as.integer(max_coord_bits))
}

