#' Read fmriarrow metadata from a Parquet file
#'
#' This is a minimal helper that retrieves the JSON metadata stored
#' by `neurovec_to_fpar()`.
#'
#' @param parquet_path Path to the Parquet file.
#' @return A list with metadata fields.
#' @export
read_fpar_metadata <- function(parquet_path) {
  tbl <- arrow::read_parquet(parquet_path, as_data_frame = FALSE)
  md <- tbl$schema$metadata
  if (is.null(md) || is.null(md$spatial_metadata)) {
    return(list())
  }
  jsonlite::fromJSON(md$spatial_metadata)
}
