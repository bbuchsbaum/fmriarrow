#' Convert a NeuroVec object to a Parquet file
#'
#' Convert a `neuroim2::NeuroVec` to a Z-order sorted Parquet file with 
#' comprehensive spatial metadata embedded in the Parquet schema.
#'
#' The function computes voxel coordinates in a vectorized fashion,
#' obtains all BOLD time series as a matrix and stores them together with
#' Morton (Z-order) indices in a Parquet file using `arrow::write_parquet()`.
#' Spatial metadata from the NeuroSpace is embedded as JSON in the
#' Parquet schema metadata.
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
  max_coord_bits <- max(1L, ceiling(log2(max(dims[1:3]))))
  neuro_vec_dims <- dim(neuro_vec_obj)
  
  if (length(dims) < 3) {
    stop("NeuroSpace must have at least 3 spatial dimensions")
  }
  if (length(neuro_vec_dims) < 4) {
    stop("NeuroVec must have at least 4 dimensions (3 spatial + 1 temporal)")
  }

  # Construct metadata structure per Sprint 2 specification
  metadata <- extract_neurospace_metadata(
    neuro_vec_obj,
    reference_space = reference_space,
    repetition_time = repetition_time
  )

  # Initialize voxel iteration
  n_vox <- prod(dims[1:3])
  if (n_vox == 0) {
    stop("NeuroSpace has zero voxels")
  }
  
  # Compute voxel coordinates all at once (1-based)
  coord_matrix <- arrayInd(seq_len(n_vox), .dim = dims[1:3])

  # Convert to 0-based coordinates
  x <- coord_matrix[, 1] - 1L
  y <- coord_matrix[, 2] - 1L
  z <- coord_matrix[, 3] - 1L

  # Morton indices for all voxels
  zindex <- compute_zindex(x, y, z)

  # Extract all BOLD time series as a matrix (voxels x time)
  bold_matrix <- t(neuroim2::series(neuro_vec_obj, coord_matrix))

  # Track min/max values for data integrity
##<<<<<<< codex/refactor-voxel-loop-with-vectorized-computation
  min_value <- min(bold_matrix, na.rm = TRUE)
  max_value <- max(bold_matrix, na.rm = TRUE)

  bold <- asplit(bold_matrix, 1)
###=======
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
    zindex[i] <- compute_zindex(x[i], y[i], z[i], max_coord_bits = max_coord_bits)
    
    # Extract BOLD time series (using 1-based coordinates for neuroim2)
    ts_data <- as.numeric(neuroim2::series(neuro_vec_obj, coords[1], coords[2], coords[3]))
    bold[[i]] <- ts_data
    
    # Update min/max for data integrity
    ts_min <- min(ts_data, na.rm = TRUE)
    ts_max <- max(ts_data, na.rm = TRUE)
    if (is.finite(ts_min)) min_value <- min(min_value, ts_min)
    if (is.finite(ts_max)) max_value <- max(max_value, ts_max)
  }
##>>>>>>> main
  
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

  # Embed metadata directly in schema
  metadata_json <- jsonlite::toJSON(metadata, auto_unbox = TRUE)
  schema <- schema$WithMetadata(list(spatial_metadata = metadata_json))

  # Create Arrow table with metadata and sort by zindex
  arrow_tbl <- arrow::arrow_table(voxel_data, schema = schema)
  arrow_tbl_sorted <- dplyr::arrange(arrow_tbl, zindex)

  # Convert the dplyr query back to a Table
  arrow_tbl_final <- dplyr::collect(arrow_tbl_sorted, as_data_frame = FALSE)

  # Write Parquet file
  arrow::write_parquet(
    arrow_tbl_final,
    output_parquet_path,
    compression = "zstd",
    write_statistics = TRUE
  )

  # Also store metadata in a sidecar file for maximum compatibility
  if (file.exists(output_parquet_path)) {
    metadata_path <- paste0(tools::file_path_sans_ext(output_parquet_path), "_metadata.json")
    writeLines(metadata_json, metadata_path)
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
