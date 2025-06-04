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
#' @param compute If `TRUE` (default), the query is materialized with
#'   `dplyr::compute()` before returning. Set to `FALSE` to return a lazy
#'   query suitable for further filtering.
#'
#' @return An Arrow Table when `compute = TRUE` or a lazy query when
#'   `compute = FALSE`.
#' @export
read_fpar_zindex_range <- function(parquet_path, min_zindex, max_zindex,
                                   columns = NULL, compute = TRUE) {
  validate_parquet_path(parquet_path)
  validate_zindex_range(min_zindex, max_zindex)

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

  if (isTRUE(compute)) {
    query <- dplyr::compute(query)
  }

  query
}
