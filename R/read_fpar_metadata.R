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

  if (!grepl("\\.(fpar|parquet)$", parquet_path)) {
    warning("File extension should be .fpar or .parquet")
  }

  parsed_metadata <- list()

  # First try sidecar JSON file since schema metadata may be lost during Arrow operations
  metadata_path <- paste0(tools::file_path_sans_ext(parquet_path), "_metadata.json")
  if (file.exists(metadata_path)) {
    tryCatch({
      metadata_json <- readLines(metadata_path, warn = FALSE)
      parsed_metadata <- jsonlite::fromJSON(metadata_json)
    }, error = function(e) {
      warning("Failed to read metadata file: ", e$message)
    })
  }

  # Fallback to reading metadata embedded in the Parquet schema
  if (length(parsed_metadata) == 0) {
    tryCatch({
      reader <- arrow::ParquetFileReader$create(parquet_path)
      schema_metadata <- reader$GetSchema()$metadata

      if (length(schema_metadata) > 0 && !is.null(schema_metadata$spatial_metadata)) {
        spatial_md <- schema_metadata$spatial_metadata
        # The metadata can be stored as a raw byte string, so convert if necessary
        if (is.raw(spatial_md)) {
          spatial_md <- rawToChar(spatial_md)
        }
        parsed_metadata <- jsonlite::fromJSON(spatial_md)
      }
    }, error = function(e) {
      warning("Failed to read embedded metadata: ", e$message)
    })
  }

  if (length(parsed_metadata) == 0) {
    warning("No metadata found in Parquet file or associated metadata file")
  } else if (!is.null(parsed_metadata$metadata_schema_version)) {
    version <- parsed_metadata$metadata_schema_version
    if (!version %in% c("0.1.0", "2.0.0")) {
      warning("Unknown metadata schema version: ", version,
              ". Results may not be compatible.")
    }
  }

  parsed_metadata
}
