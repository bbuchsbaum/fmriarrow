#' Read fmriarrow metadata from a Parquet file
#'
#' Extracts and parses the comprehensive spatial metadata stored by 
#' `neurovec_to_fpar()` in the Parquet schema metadata.
#'
#' @param parquet_path Path to the Parquet file.
#' @return A list with metadata fields matching the Sprint 2 specification:
#'   \describe{
#'     \item{metadata_schema_version}{Schema version string}
#'     \item{source_info}{List with original_file and neuroim2_space_hash}
#'     \item{spatial_properties}{List with dimensions, voxel_size_mm, affine_matrix, etc.}
#'     \item{acquisition_properties}{List with repetition_time_s and timepoint_count}
#'     \item{data_integrity}{List with voxel_count and bold_value_range}
#'   }
#' @export
read_fpar_metadata <- function(parquet_path) {
  validate_parquet_path(parquet_path)

  # First try to read embedded metadata from Parquet schema
  tryCatch({
    tbl <- arrow::read_parquet(parquet_path, as_data_frame = FALSE,
                               col_select = character(0))
    schema_metadata <- tbl$schema$metadata
    
    if (!is.null(schema_metadata) && !is.null(schema_metadata$spatial_metadata)) {
      parsed_metadata <- jsonlite::fromJSON(schema_metadata$spatial_metadata)
      
      # Validate metadata schema version if present
      if (!is.null(parsed_metadata$metadata_schema_version)) {
        version <- parsed_metadata$metadata_schema_version
        if (!version %in% c("0.1.0", "2.0.0")) {
          warning("Unknown metadata schema version: ", version, 
                  ". Results may not be compatible.")
        }
      }
      
      return(parsed_metadata)
    }
  }, error = function(e) {
    warning("Failed to read embedded metadata: ", e$message)
  })

  # Fallback: try to read from separate metadata file
  metadata_path <- paste0(tools::file_path_sans_ext(parquet_path), "_metadata.json")
  if (file.exists(metadata_path)) {
    tryCatch({
      metadata_json <- readLines(metadata_path, warn = FALSE)
      parsed_metadata <- jsonlite::fromJSON(metadata_json)
      
      # Validate metadata schema version if present
      if (!is.null(parsed_metadata$metadata_schema_version)) {
        version <- parsed_metadata$metadata_schema_version
        if (!version %in% c("0.1.0", "2.0.0")) {
          warning("Unknown metadata schema version: ", version, 
                  ". Results may not be compatible.")
        }
      }
      
      return(parsed_metadata)
    }, error = function(e) {
      warning("Failed to read metadata file: ", e$message)
    })
  }

  warning("No metadata found in Parquet file or associated metadata file")
  list()
}
