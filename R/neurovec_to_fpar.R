#' Convert a NeuroVec object to a Parquet file
#'
#' Convert a `neuroim2::NeuroVec` to a Z-order sorted Parquet file with 
#' comprehensive spatial metadata embedded in the Parquet schema.
#'
#' The function iterates over all voxels of the input object, computes their
#' Morton (Z-order) index using [`compute_zindex()`], extracts the full BOLD
#' time series and writes the resulting table to disk using
#' `arrow::write_parquet()`. Spatial metadata from the NeuroSpace is embedded
#' as JSON in the Parquet schema metadata.
#'
#' @param neuro_vec_obj A `neuroim2::NeuroVec` object.
#' @param output_parquet_path Path to the Parquet file to create.
#' @param subject_id Identifier for the subject being converted.
#' @param session_id Optional session identifier.
#' @param task_id Optional task identifier.
#' @param run_id Optional run identifier.
#' @param reference_space Optional reference space identifier (e.g., "MNI152NLin2009cAsym").
#' @param repetition_time Optional repetition time in seconds.
#' @param ... Additional arguments reserved for future extensions.
#'
#' @return A list containing the extracted `NeuroSpace`, the voxel data and
#'   the generated Arrow table. The Parquet file is written to
#'   `output_parquet_path`. The return value is primarily for testing and
#'   should not be considered part of the final API.
#' @export
neurovec_to_fpar <- function(neuro_vec_obj, output_parquet_path,
                             subject_id, session_id = NULL,
                             task_id = NULL, run_id = NULL,
                             reference_space = NULL, repetition_time = NULL, ...) {
  # Input validation
  if (!inherits(neuro_vec_obj, "NeuroVec")) {
    stop("neuro_vec_obj must inherit from 'NeuroVec'")
  }
  if (!is.character(output_parquet_path) || length(output_parquet_path) != 1) {
    stop("output_parquet_path must be a single string")
  }
  if (!is.character(subject_id) || length(subject_id) != 1 ||
      is.na(subject_id) || nchar(subject_id) == 0) {
    stop("subject_id must be a non-empty string")
  }
  if (!is.null(session_id) &&
      (!is.character(session_id) || length(session_id) != 1)) {
    stop("session_id must be NULL or a single string")
  }
  if (!is.null(task_id) &&
      (!is.character(task_id) || length(task_id) != 1)) {
    stop("task_id must be NULL or a single string")
  }
  if (!is.null(run_id) && (!is.character(run_id) || length(run_id) != 1)) {
    stop("run_id must be NULL or a single string")
  }
  if (!is.null(reference_space) && (!is.character(reference_space) || length(reference_space) != 1)) {
    stop("reference_space must be NULL or a single string")
  }
  if (!is.null(repetition_time) && (!is.numeric(repetition_time) || length(repetition_time) != 1)) {
    stop("repetition_time must be NULL or a single numeric value")
  }

  # Extract NeuroSpace and validate
  space_obj <- neuroim2::space(neuro_vec_obj)
  dims <- dim(space_obj)
  neuro_vec_dims <- dim(neuro_vec_obj)
  
  if (length(dims) < 3) {
    stop("NeuroSpace must have at least 3 spatial dimensions")
  }
  if (length(neuro_vec_dims) < 4) {
    stop("NeuroVec must have at least 4 dimensions (3 spatial + 1 temporal)")
  }

  # Extract metadata from NeuroSpace (CORE-007)
  spacing_vec <- neuroim2::spacing(space_obj)
  affine_matrix <- neuroim2::trans(space_obj)
  
  # Create comprehensive metadata structure per Sprint 2 specification
  metadata <- list(
    metadata_schema_version = "2.0.0",
    source_info = list(
      original_file = deparse(substitute(neuro_vec_obj)),
      neuroim2_space_hash = digest::digest(space_obj, algo = "sha256")
    ),
    spatial_properties = list(
      original_dimensions = as.integer(dims),
      voxel_size_mm = as.numeric(spacing_vec[1:min(3, length(spacing_vec))]),
      affine_matrix = as.matrix(affine_matrix),
      reference_space = reference_space %||% "unknown",
      coordinate_convention = "0-based RAS"
    ),
    acquisition_properties = list(
      repetition_time_s = repetition_time %||% NA_real_,
      timepoint_count = as.integer(neuro_vec_dims[4])
    ),
    data_integrity = list(
      voxel_count = as.integer(prod(dims[1:3])),
      bold_value_range = c(NA_real_, NA_real_)  # Will be computed during iteration
    )
  )

  # Initialize voxel iteration
  n_vox <- prod(dims[1:3])
  if (n_vox == 0) {
    stop("NeuroSpace has zero voxels")
  }
  
  x <- integer(n_vox)
  y <- integer(n_vox)
  z <- integer(n_vox)
  zindex <- integer(n_vox)
  bold <- vector("list", n_vox)
  
  # Track min/max values for data integrity
  min_value <- Inf
  max_value <- -Inf

  # Extract voxel data and compute Z-indices
  for (i in seq_len(n_vox)) {
    # Get 1-based grid coordinates from neuroim2
    coords <- neuroim2::index_to_grid(space_obj, i)
    
    # Convert to 0-based for storage
    x[i] <- coords[1] - 1L
    y[i] <- coords[2] - 1L
    z[i] <- coords[3] - 1L
    
    # Compute Z-order index
    zindex[i] <- compute_zindex(x[i], y[i], z[i])
    
    # Extract BOLD time series (using 1-based coordinates for neuroim2)
    ts_data <- as.numeric(neuroim2::series(neuro_vec_obj, coords[1], coords[2], coords[3]))
    bold[[i]] <- ts_data
    
    # Update min/max for data integrity
    ts_min <- min(ts_data, na.rm = TRUE)
    ts_max <- max(ts_data, na.rm = TRUE)
    if (is.finite(ts_min)) min_value <- min(min_value, ts_min)
    if (is.finite(ts_max)) max_value <- max(max_value, ts_max)
  }
  
  # Update metadata with computed value range
  metadata$data_integrity$bold_value_range <- c(
    if(is.finite(min_value)) min_value else NA_real_,
    if(is.finite(max_value)) max_value else NA_real_
  )

  # Create data frame with proper structure
  voxel_data <- data.frame(
    subject_id = rep(subject_id, n_vox),
    session_id = rep(session_id %||% NA_character_, n_vox),
    task_id = rep(task_id %||% NA_character_, n_vox),
    run_id = rep(run_id %||% NA_character_, n_vox),
    x = x, 
    y = y, 
    z = z,
    zindex = zindex,
    stringsAsFactors = FALSE
  )
  
  # Add BOLD time series as list column
  voxel_data$bold <- I(bold)

  timepoints <- as.integer(neuro_vec_dims[4])

  # Define Arrow schema with explicit types
  schema <- arrow::schema(
    subject_id = arrow::string(),
    session_id = arrow::string(),
    task_id = arrow::string(),
    run_id = arrow::string(),
    x = arrow::uint16(),
    y = arrow::uint16(),
    z = arrow::uint16(),
    zindex = arrow::uint32(),
    bold = arrow::fixed_size_list_of(arrow::float32(), timepoints)
  )

  # Create Arrow table and sort by zindex
  arrow_tbl <- arrow::arrow_table(voxel_data, schema = schema)
  arrow_tbl_sorted <- dplyr::arrange(arrow_tbl, zindex)
  
  # Convert the dplyr query back to a Table
  arrow_tbl_final <- dplyr::collect(arrow_tbl_sorted, as_data_frame = FALSE)

  # Prepare metadata for Parquet schema (CORE-008)
  metadata_json <- jsonlite::toJSON(metadata, auto_unbox = TRUE)

  # Write Parquet file first
  arrow::write_parquet(
    arrow_tbl_final,
    output_parquet_path,
    compression = "zstd",
    write_statistics = TRUE,
    row_group_size = 4096
  )
  
  # Add metadata to the written file
  if (file.exists(output_parquet_path)) {
    # Try a simpler metadata approach - write to a separate metadata file for now
    # Since the Arrow metadata approach seems to have compatibility issues
    metadata_path <- paste0(tools::file_path_sans_ext(output_parquet_path), "_metadata.json")
    writeLines(metadata_json, metadata_path)
    
    # Try the Arrow metadata approach as backup
    tryCatch({
      temp_tbl <- arrow::read_parquet(output_parquet_path, as_data_frame = FALSE)
      metadata_kv <- list(spatial_metadata = metadata_json)
      new_schema <- temp_tbl$schema$WithMetadata(metadata_kv)
      new_tbl <- arrow::arrow_table(as.data.frame(temp_tbl), schema = new_schema)
      arrow::write_parquet(new_tbl, output_parquet_path, compression = "zstd", write_statistics = TRUE, row_group_size = 4096)
    }, error = function(e) {
      warning("Could not embed metadata in Parquet schema: ", e$message)
    })
  }

  # Return diagnostic information (primarily for testing)
  list(
    neuro_vec_obj = neuro_vec_obj,
    output_parquet_path = output_parquet_path,
    subject_id = subject_id,
    session_id = session_id,
    task_id = task_id,
    run_id = run_id,
    reference_space = reference_space,
    repetition_time = repetition_time,
    space = space_obj,
    voxel_data = voxel_data,
    arrow_table = arrow_tbl_final,
    metadata = metadata
  )
}
