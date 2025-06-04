#' Internal validation helpers
#'
#' These functions are not exported and used internally to validate
#' common arguments across the package.
#' @noRd
validate_parquet_path <- function(path) {
  if (!is.character(path) || length(path) != 1) {
    stop("parquet_path must be a single string")
  }
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }
  invisible(TRUE)
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
  limit <- bitwShiftL(1L, 32)
  if (min_zindex >= limit || max_zindex >= limit) {
    stop("zindex values exceed 32-bit range")
  }
  invisible(TRUE)
}
