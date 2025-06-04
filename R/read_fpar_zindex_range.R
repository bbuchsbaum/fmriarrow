#' Read voxel rows by Z-index range
#'
#' Filters a Parquet file produced by `neurovec_to_fpar()` for voxels
#' with Morton indices within the specified range.
#'
#' @param parquet_path Path to the Parquet file.
#' @param min_zindex Minimum Z-index (inclusive).
#' @param max_zindex Maximum Z-index (inclusive).
#' @param columns Optional character vector of columns to return. If `NULL`, all
#'   columns are returned.
#'
#' @return An Arrow Table containing the filtered rows.
#' @export
read_fpar_zindex_range <- function(parquet_path, min_zindex, max_zindex, columns = NULL) {
  if (!is.character(parquet_path) || length(parquet_path) != 1) {
    stop("parquet_path must be a single string")
  }
  if (!file.exists(parquet_path)) {
    stop("File does not exist: ", parquet_path)
  }
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

  ds <- arrow::open_dataset(parquet_path)
  schema_cols <- ds$schema$names
  if (!is.null(columns)) {
    if (!all(columns %in% schema_cols)) {
      missing <- setdiff(columns, schema_cols)
      stop("Invalid column names: ", paste(missing, collapse = ", "))
    }
  } else {
    columns <- schema_cols
  }

  query <- ds |>
    dplyr::filter(zindex >= min_zindex & zindex <= max_zindex) |>
    dplyr::select(dplyr::all_of(columns))

  arrow::collect(query, as_data_frame = FALSE)
}
