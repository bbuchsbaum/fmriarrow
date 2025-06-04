#' Internal validation helpers
#'
#' These functions are not exported and used internally to validate
#' common arguments across the package.
#' @noRd
validate_parquet_path <- function(path, mode = "read") {
  if (!is.character(path) || length(path) != 1) {
    stop("parquet_path must be a single string")
  }
  
  if (mode == "read" && !file.exists(path)) {
    stop("Parquet file does not exist: ", path)
  }
  
  if (mode == "write") {
    dir_path <- dirname(path)
    if (!dir.exists(dir_path)) {
      stop("Directory does not exist: ", dir_path)
    }
  }
}

# Validate the max_coord_bits argument
#
# Ensures the value is numeric length 1, integer valued and greater than
# zero. Returns the integer value on success.
#' @noRd
validate_max_coord_bits <- function(max_coord_bits) {
  if (!is.numeric(max_coord_bits) || length(max_coord_bits) != 1 ||
      is.na(max_coord_bits)) {
    stop("max_coord_bits must be a single numeric value")
  }
  int_val <- as.integer(max_coord_bits)
  if (int_val != max_coord_bits) {
    stop("max_coord_bits must be an integer value")
  }
  if (int_val <= 0L) {
    stop("max_coord_bits must be > 0")
  }
  int_val
}

validate_coordinate_range <- function(range, name, max_coord_bits = 10) {
  if (length(range) == 1) {
    range <- c(range, range)
  }
  if (length(range) != 2) {
    stop(name, " must be length 1 or 2")
  }
  if (any(is.na(range))) {
    stop(name, " cannot contain NA")
  }
  range <- as.integer(range)
  if (range[1] > range[2]) {
    stop(name, " minimum must be <= maximum")
  }
  if (any(range < 0)) {
    stop(name, " must be non-negative")
  }
  limit <- bitwShiftL(1L, max_coord_bits)
  if (any(range >= limit)) {
    stop(name, " exceeds range defined by max_coord_bits")
  }
  range
}

validate_zindex_range <- function(min_zindex, max_zindex) {
  if (!is.numeric(min_zindex) || length(min_zindex) != 1 || is.na(min_zindex) ||
      !is.numeric(max_zindex) || length(max_zindex) != 1 || is.na(max_zindex)) {
    stop("min_zindex and max_zindex must be single non-NA numeric values")
  }
  if (min_zindex < 0 || max_zindex < 0) {
    stop("zindex values must be non-negative")
  }
  if (min_zindex > max_zindex) {
    stop("min_zindex must be less than or equal to max_zindex")
  }
  # Use safe 32-bit unsigned integer limit: 2^32 - 1 = 4294967295
  limit <- 4294967295
  if (min_zindex >= limit || max_zindex >= limit) {
    stop("zindex values exceed 32-bit range")
  }
  invisible(TRUE)
}

# Normalize a coordinate range to length 2
#
# Helper for [coords_to_zindex_range()]. Accepts either a single
# coordinate or a two element vector and returns an integer vector of
# length 2.
#' @noRd
normalize_coord_range <- function(range) {
  if (length(range) == 1) {
    range <- c(range, range)
  }
  if (length(range) != 2) {
    stop("coordinate range must be length 1 or 2")
  }
  as.integer(range)
}

# Validate that coordinates lie within 0..max_coord
#
# Used by [coords_to_zindex_range()] to guard against out-of-bounds
# queries.
#' @noRd
validate_coord_bounds <- function(ranges, max_coord) {
  if (any(ranges < 0L) || any(ranges > max_coord)) {
    stop("coordinate values exceed bounds [0, ", max_coord, "]")
  }
  invisible(TRUE)
}

#' Null-coalescing operator
#' @param x First value
#' @param y Second value (returned if x is NULL)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
