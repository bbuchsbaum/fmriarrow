#' Read fmriarrow metadata from a Parquet file
#'
#' This is a minimal helper that retrieves the JSON metadata stored
#' by `neurovec_to_fpar()`.
#'
#' @param parquet_path Path to the Parquet file.
#' @return A list with metadata fields.
#' @export
read_fpar_metadata <- function(parquet_path) {
  validate_parquet_path(parquet_path)

  tbl <- arrow::read_parquet(parquet_path, as_data_frame = FALSE,
                             col_select = character(0))
  md <- tbl$schema$metadata
  if (is.null(md) || is.null(md$spatial_metadata)) {
    warning("No spatial metadata found in Parquet file")
    return(list())
  }

  parsed <- tryCatch(jsonlite::fromJSON(md$spatial_metadata),
                     error = function(e) {
                       stop("Failed to parse spatial metadata: ", e$message)
                     })

  parsed
}
