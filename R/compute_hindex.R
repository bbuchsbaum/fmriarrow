#' @useDynLib fmriarrow, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# R implementation of the coordinate-to-index algorithm
# This is a helper for testing and understanding the logic. The C++ version is used in practice.
compute_hindex_r <- function(x, y, z, nbits) {
  
  rot_r <- function(N, .x, .y, .z, rx, ry, rz) {
    if (rz == 0) {
        if (ry == 0) {
            if (rx == 1) {
                .x <- N - 1 - .x
                .y <- N - 1 - .y
            }
            # swap x, y
            temp <- .x; .x <- .y; .y <- temp
        } else {
             # swap y, z
             temp <- .y; .y <- .z; .z <- temp
        }
    } else {
        if (ry == 1) {
             if (rx == 0) {
                temp <- .y
                .y <- .x
                .x <- N - 1 - temp
             } else {
                .x <- N - 1 - .x
                .y <- N - 1 - .y
             }
        } else {
            temp <- .z
            .z <- .x
            .x <- temp
        }
    }
    return(list(x = .x, y = .y, z = .z))
  }
  
  N <- 2^nbits
  h_indices <- numeric(length(x))

  for (k in seq_along(x)) {
    cur_x <- x[k]; cur_y <- y[k]; cur_z <- z[k]
    h <- 0

    for (s in (nbits - 1):0) {
      rx <- bitwAnd(bitwShiftR(cur_x, s), 1)
      ry <- bitwAnd(bitwShiftR(cur_y, s), 1)
      rz <- bitwAnd(bitwShiftR(cur_z, s), 1)
      
      digit <- bitwOr(bitwShiftL(rx, 2), bitwOr(bitwShiftL(ry, 1), rz))
      h <- bitwOr(bitwShiftL(h, 3), digit)
      
      new_coords <- rot_r(N, cur_x, cur_y, cur_z, rx, ry, rz)
      cur_x <- new_coords$x
      cur_y <- new_coords$y
      cur_z <- new_coords$z
    }
    h_indices[k] <- h
  }
  
  as.double(h_indices)
}

# R implementation of the index-to-coordinate algorithm
compute_hindex_inverse_r <- function(h_indices, nbits) {
  
  rot_r <- function(N, .x, .y, .z, rx, ry, rz) {
    if (rz == 0) {
        if (ry == 0) {
            if (rx == 1) {
                .x <- N - 1 - .x
                .y <- N - 1 - .y
            }
            # swap x, y
            temp <- .x; .x <- .y; .y <- temp
        } else {
             # swap y, z
             temp <- .y; .y <- .z; .z <- temp
        }
    } else {
        if (ry == 1) {
             if (rx == 0) {
                temp <- .y
                .y <- .x
                .x <- N - 1 - temp
             } else {
                .x <- N - 1 - .x
                .y <- N - 1 - .y
             }
        } else {
            temp <- .z
            .z <- .x
            .x <- temp
        }
    }
    return(list(x = .x, y = .y, z = .z))
  }
  
  N <- 2^nbits
  coords_df <- data.frame(x = integer(length(h_indices)), y = integer(length(h_indices)), z = integer(length(h_indices)))
  
  for (k in seq_along(h_indices)) {
    h <- h_indices[k]
    cur_x <- 0; cur_y <- 0; cur_z <- 0
    
    for (s in (nbits - 1):0) {
        rx <- bitwAnd(bitwShiftR(h, 3 * s + 2), 1)
        ry <- bitwAnd(bitwShiftR(h, 3 * s + 1), 1)
        rz <- bitwAnd(bitwShiftR(h, 3 * s + 0), 1)
      
      # The key insight from the user guide: apply forward rot to partial coords
      new_coords <- rot_r(N, cur_x, cur_y, cur_z, rx, ry, rz)
      cur_x <- new_coords$x
      cur_y <- new_coords$y
      cur_z <- new_coords$z
      
      # Set the coordinate bit
      cur_x <- bitwOr(cur_x, bitwShiftL(rx, s))
      cur_y <- bitwOr(cur_y, bitwShiftL(ry, s))
      cur_z <- bitwOr(cur_z, bitwShiftL(rz, s))
    }
    coords_df$x[k] <- cur_x
    coords_df$y[k] <- cur_y
    coords_df$z[k] <- cur_z
  }
  return(coords_df)
}


#' Compute 3D Hilbert curve indices
#' 
#' @param x,y,z Integer vectors of coordinates
#' @param nbits Number of bits per dimension (e.g., 10 for a 1024x1024x1024 cube)
#' @return A numeric vector of Hilbert indices.
#' @export
compute_hindex <- function(x, y, z, nbits) {
  compute_hindex_cpp(x, y, z, nbits)
}

#' Decode Hilbert curve indices back to coordinates
#'
#' @param h_indices A numeric vector of Hilbert indices
#' @param nbits Number of bits per dimension
#' @return A data.frame with columns x, y and z
#' @export
compute_hindex_inverse <- function(h_indices, nbits) {
  compute_hindex_cpp_inverse(h_indices, nbits)
}

