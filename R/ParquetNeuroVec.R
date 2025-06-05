#' ParquetNeuroVec Class
#' 
#' An S4 class that inherits from `neuroim2::NeuroVec` and provides efficient
#' array-like access to 4D fMRI data stored in Parquet format with spatial indexing.
#' 
#' @description 
#' ParquetNeuroVec provides a NeuroVec-compatible interface for accessing fMRI data
#' stored in the optimized Parquet format created by `neurovec_to_fpar()`. It leverages
#' spatial indexing for efficient querying while maintaining full compatibility with
#' neuroim2 workflows.
#' 
#' @importFrom neuroim2 series series_roi
#' 
#' @slot parquet_path character. Path to the .fpar file.
#' @slot metadata list. Cached metadata from the Parquet file.
#' @slot lazy logical. Whether to use lazy loading for data access.
#' 
#' @name ParquetNeuroVec-class
#' @export
setClass("ParquetNeuroVec",
  contains = "NeuroVec",
  slots = c(
    parquet_path = "character",
    metadata = "list", 
    lazy = "logical"
  )
)

#' Create a ParquetNeuroVec Object
#' 
#' @param parquet_path character. Path to the .fpar file
#' @param lazy logical. Whether to use lazy loading (default: TRUE)
#' 
#' @return A ParquetNeuroVec object
#' @export
#' 
#' @examples
#' \dontrun{
#' # Create from an existing .fpar file
#' pvec <- ParquetNeuroVec("path/to/data.fpar")
#' 
#' # Access dimensions
#' dim(pvec)
#' 
#' # Extract time series
#' ts <- series(pvec, 30, 40, 20)
#' 
#' # Extract ROI time series
#' roi_ts <- series_roi(pvec, 25:35, 35:45, 15:25)
#' }
ParquetNeuroVec <- function(parquet_path, lazy = TRUE) {
  # Validate input
  if (!file.exists(parquet_path)) {
    stop("Parquet file not found: ", parquet_path)
  }
  
  if (!grepl("\\.(fpar|parquet)$", parquet_path)) {
    warning("File extension should be .fpar or .parquet")
  }
  
  # Read metadata to extract spatial properties
  metadata <- read_fpar_metadata(parquet_path)
  
  # Extract spatial properties for NeuroSpace construction
  spatial_props <- metadata$spatial_properties
  if (is.null(spatial_props)) {
    stop("Spatial properties not found in metadata")
  }
  
  # Reconstruct NeuroSpace from metadata
  dims <- spatial_props$original_dimensions
  spacing_vec <- spatial_props$voxel_size_mm
  affine_mat <- matrix(spatial_props$affine_matrix, nrow = 4, ncol = 4)
  
  # Create NeuroSpace
  neuro_space <- neuroim2::NeuroSpace(
    dim = dims,
    spacing = spacing_vec,
    trans = affine_mat
  )
  
  # Create data array
  if (lazy) {
    # Minimal placeholder for lazy loading
    empty_data <- array(NA_real_, dim = c(1, 1, 1, 1))
  } else {
    # Load full data eagerly
    data_table <- arrow::read_parquet(parquet_path) |>
      dplyr::arrange(zindex)

    bold_matrix <- do.call(rbind, data_table$bold)
    empty_data <- array(NA_real_, dim = dims)

    coords_matrix <- as.matrix(data_table[, c("x", "y", "z")]) + 1

    for (idx in seq_len(nrow(coords_matrix))) {
      empty_data[
        coords_matrix[idx, 1],
        coords_matrix[idx, 2],
        coords_matrix[idx, 3],
        ] <- bold_matrix[idx, ]
    }
  }
  
  # Create the object using new()
  obj <- new("ParquetNeuroVec",
    space = neuro_space,
    label = basename(parquet_path),
    parquet_path = parquet_path,
    metadata = metadata,
    lazy = lazy
  )
  
  return(obj)
}

#' Extract Time Series from ParquetNeuroVec
#' 
#' @param x ParquetNeuroVec object
#' @param i integer. x-coordinate (1-based)
#' @param j integer. y-coordinate (1-based) 
#' @param k integer. z-coordinate (1-based)
#' @param drop logical. Whether to drop dimensions of length 1
#' 
#' @return numeric vector of time series values
#' @export
#' 
setMethod("series", signature(x = "ParquetNeuroVec", i = "integer"), 
  function(x, i, j, k, drop = TRUE) {
    # Convert 1-based to 0-based coordinates
    x_coord <- i - 1
    y_coord <- j - 1 
    z_coord <- k - 1
    
    # Validate coordinates
    dims <- x@metadata$spatial_properties$original_dimensions
    if (x_coord < 0 || x_coord >= dims[1] ||
        y_coord < 0 || y_coord >= dims[2] ||
        z_coord < 0 || z_coord >= dims[3]) {
      stop("Coordinates out of bounds")
    }
    
    # Query the specific voxel using its Z-order index
    z_idx <- compute_zindex(x_coord, y_coord, z_coord)
    data_table <- arrow::open_dataset(x@parquet_path) |>
      dplyr::filter(zindex == z_idx) |>
      dplyr::collect()
    
    if (nrow(data_table) == 0) {
      # Return NA vector if voxel not found (might be masked out)
      n_timepoints <- dims[4]
      return(rep(NA_real_, n_timepoints))
    }
    
    # Extract BOLD time series
    bold_series <- data_table$bold[[1]]
    
    if (drop && length(bold_series) == 1) {
      return(as.numeric(bold_series))
    } else {
      return(bold_series)
    }
  }
)

#' Extract Time Series from ParquetNeuroVec (Numeric Coordinates)
#' 
#' @param x ParquetNeuroVec object
#' @param i numeric. x-coordinate 
#' @param j numeric. y-coordinate
#' @param k numeric. z-coordinate
#' @param drop logical. Whether to drop dimensions
#' 
#' @return numeric vector of time series values
#' @export
#' 
setMethod("series", signature(x = "ParquetNeuroVec", i = "numeric"),
  function(x, i, j, k, drop = TRUE) {
    # Convert to integer and call integer method
    series(x, as.integer(round(i)), as.integer(round(j)), as.integer(round(k)), drop = drop)
  }
)

#' Extract Time Series Using Matrix Coordinates
#' 
#' @param x ParquetNeuroVec object
#' @param i matrix. Matrix with 3 columns (x, y, z coordinates, 1-based)
#' 
#' @return matrix. Each column is a voxel's time series
#' @export
#' 
setMethod("series", signature(x = "ParquetNeuroVec", i = "matrix"),
  function(x, i) {
    if (ncol(i) != 3) {
      stop("Coordinate matrix must have 3 columns (x, y, z)")
    }

    # Convert 1-based to 0-based coordinates
    coords_0based <- i - 1

    dims <- x@metadata$spatial_properties$original_dimensions
    if (any(coords_0based[, 1] < 0 | coords_0based[, 1] >= dims[1] |
            coords_0based[, 2] < 0 | coords_0based[, 2] >= dims[2] |
            coords_0based[, 3] < 0 | coords_0based[, 3] >= dims[3])) {
      stop("Coordinates out of bounds")
    }

    # Create coordinate ranges for efficient querying
    x_range <- range(coords_0based[, 1])
    y_range <- range(coords_0based[, 2])
    z_range <- range(coords_0based[, 3])

    data_table <- read_fpar_coords_roi(
      x@parquet_path,
      x_range = x_range,
      y_range = y_range,
      z_range = z_range,
      exact = TRUE,
      columns = c("x", "y", "z", "bold")
    ) |> dplyr::collect()

    table_keys <- paste(data_table$x, data_table$y, data_table$z, sep = "_")
    query_keys <- paste(coords_0based[, 1], coords_0based[, 2], coords_0based[, 3], sep = "_")
    match_idx <- match(query_keys, table_keys)

    result_matrix <- matrix(NA_real_,
                           nrow = x@metadata$acquisition_properties$timepoint_count,
                           ncol = nrow(i))

    valid <- which(!is.na(match_idx))
    if (length(valid) > 0) {
      bold_mat <- do.call(rbind, data_table$bold[match_idx[valid]])
      result_matrix[, valid] <- t(bold_mat)
    }

    result_matrix
  }
)

#' Extract ROI Time Series from ParquetNeuroVec based on a bounding box
#' 
#' @param x ParquetNeuroVec object
#' @param i integer. x-coordinate range or single coordinate
#' @param j integer. y-coordinate range or single coordinate
#' @param k integer. z-coordinate range or single coordinate
#' 
#' @return numeric vector. Average time series across the ROI
#' @export
setGeneric("series_roi_bbox", function(x, i, j, k) {
  standardGeneric("series_roi_bbox")
})

#' @rdname series_roi_bbox-methods
#' @export
setMethod("series_roi_bbox", signature(x = "ParquetNeuroVec", i = "integer", j = "integer", k = "integer"),
  function(x, i, j, k) {
    dims <- x@metadata$spatial_properties$original_dimensions
    
    validate_coord <- function(val, name, max_val) {
      if (any(val < 1) || any(val > max_val)) {
        stop(paste0(name, " coordinates are out of bounds"))
      }
      range(val) 
    }
    
    x_range <- validate_coord(i, "x", dims[1])
    y_range <- validate_coord(j, "y", dims[2])
    z_range <- validate_coord(k, "z", dims[3])

    data_table <- read_fpar_coords_roi(
      x@parquet_path,
      x_range = x_range - 1, # to 0-based
      y_range = y_range - 1,
      z_range = z_range - 1,
      columns = "bold"
    )
    
    if (nrow(data_table) == 0) {
      return(rep(0, x@metadata$acquisition_properties$timepoint_count))
    }
    
    # Compute mean time series across all voxels in the ROI
    bold_matrix <- do.call(rbind, data_table$bold)
    colMeans(bold_matrix)
  }
)

#' @rdname series_roi_bbox-methods
setMethod("series_roi_bbox", signature(x = "ParquetNeuroVec", i = "numeric", j = "numeric", k = "numeric"),
  function(x, i, j, k) {
    series_roi_bbox(x, as.integer(i), as.integer(j), as.integer(k))
  }
)

#' Show Method for ParquetNeuroVec
#' 
#' @param object ParquetNeuroVec object
#' @export
#' 
setMethod("show", "ParquetNeuroVec", 
  function(object) {
    cat("ParquetNeuroVec Object\n")
    cat("======================\n\n")
    
    # Basic info
    cat("* Storage Information:\n")
    cat("  File Path:    ", object@parquet_path, "\n")
    cat("  Lazy Loading: ", object@lazy, "\n")
    
    # Spatial properties
    spatial <- object@metadata$spatial_properties
    if (!is.null(spatial)) {
      cat("\n* Spatial Properties:\n")
      cat("  Dimensions:   ", paste(spatial$original_dimensions, collapse = " x "), "\n")
      cat("  Voxel Size:   ", paste(spatial$voxel_size_mm, collapse = " x "), " mm\n")
      if (!is.null(spatial$reference_space)) {
        cat("  Reference:    ", spatial$reference_space, "\n")
      }
    }
    
    # Acquisition properties
    acq <- object@metadata$acquisition_properties
    if (!is.null(acq)) {
      cat("\n* Acquisition Properties:\n")
      if (!is.null(acq$repetition_time_s) && !is.na(acq$repetition_time_s)) {
        cat("  TR:           ", acq$repetition_time_s, " s\n")
      }
      cat("  Time Points:  ", acq$timepoint_count, "\n")
    }
    
    # Data integrity
    integrity <- object@metadata$data_integrity
    if (!is.null(integrity)) {
      cat("\n* Data Properties:\n")
      cat("  Voxel Count:  ", integrity$voxel_count, "\n")
      if (!any(is.na(integrity$bold_value_range))) {
        cat("  Value Range:  [", paste(round(integrity$bold_value_range, 2), collapse = ", "), "]\n")
      }
    }
    
    cat("\n* Access Methods:\n")
    cat("  Time Series:  series(object, x, y, z)\n")
    cat("  ROI Series:   series_roi(object, x_range, y_range, z_range)\n")
    cat("  Matrix Query: series(object, coord_matrix)\n")
    
    cat("\n")
  }
)

.load_data_to_array <- function(x) {
  data_table <- arrow::read_parquet(x@parquet_path)
  
  # Ensure order for correct reconstruction
  data_table <- dplyr::arrange(data_table, zindex)
  
  bold_matrix <- do.call(rbind, data_table$bold)
  
  arr <- array(NA_real_, dim = dim(x@space))
  
  # Get 0-based coordinates and convert to 1-based for array indexing
  coords_matrix <- as.matrix(data_table[, c("x", "y", "z")]) + 1
  
  for (idx in seq_len(nrow(coords_matrix))) {
    arr[
      coords_matrix[idx, 1],
      coords_matrix[idx, 2],
      coords_matrix[idx, 3],
    ] <- bold_matrix[idx, ]
  }
  
  arr
}

setMethod("[", signature(x = "ParquetNeuroVec"),
  function(x, i, j, k, ..., drop = TRUE) {
    # This is a simple but inefficient implementation that loads all data.
    data_arr <- .load_data_to_array(x)
    
    # Handle the case where no indices are provided, returning the full array
    if (missing(i) && missing(j) && missing(k) && missing(..1)) {
       return(data_arr)
    }
    
    # Pass subsetting to the underlying array
    data_arr[i, j, k, ..., drop = drop]
  }
)

#' Get Length of ParquetNeuroVec (Number of Time Points)
#' 
#' @param x ParquetNeuroVec object
#' @return integer. Number of time points
#' @export
#' 
setMethod("length", "ParquetNeuroVec",
  function(x) {
    return(x@metadata$acquisition_properties$timepoint_count)
  }
)

#' Get Dimensions of ParquetNeuroVec
#' 
#' @param x ParquetNeuroVec object
#' @return integer vector. Dimensions of the 4D array
#' @export
#' 
setMethod("dim", "ParquetNeuroVec",
  function(x) {
    return(x@metadata$spatial_properties$original_dimensions)
  }
)

setMethod("as.matrix", signature(x = "ParquetNeuroVec"),
  function(x) {
    # Load all data into a 4D array
    arr <- x[]
    
    # Reshape to a 2D matrix (voxels x time)
    mat_dims <- c(prod(dim(x)[1:3]), dim(x)[4])
    dim(arr) <- mat_dims
    
    arr
  }
)

#' @export
#' @rdname sub_vector-methods
setMethod("sub_vector", signature(x = "ParquetNeuroVec", i = "integer"),
  function(x, i) {
    # Subset the data along the time dimension
    sub_arr <- x[,,,i, drop=FALSE]
    
    # Create a new NeuroSpace with updated dimensions
    old_space <- x@space
    new_dim <- dim(old_space)
    new_dim[4] <- length(i)
    
    new_space <- neuroim2::NeuroSpace(new_dim, spacing=neuroim2::spacing(old_space), 
                                      origin=neuroim2::origin(old_space), trans=neuroim2::trans(old_space))
    
    # Create a new DenseNeuroVec from the subset
    dense_sub_vec <- neuroim2::DenseNeuroVec(sub_arr, new_space)
    
    # Write to a new temp parquet file and return a ParquetNeuroVec
    temp_path <- tempfile(fileext = ".fpar")
    neurovec_to_fpar(dense_sub_vec, temp_path, subject_id="temp")
    ParquetNeuroVec(temp_path)
  }
)

#' @export
#' @importFrom neuroim2 concat
#' @rdname concat-methods
setMethod("concat", signature(x = "ParquetNeuroVec"),
  function(x, y, ...) {
    # Load data from all provided ParquetNeuroVec objects
    all_vecs <- c(list(x, y), list(...))
    
    all_data <- lapply(all_vecs, function(vec) {
      if (!inherits(vec, "ParquetNeuroVec")) {
        stop("All objects to concatenate must be ParquetNeuroVec objects")
      }
      vec[]
    })
    
    # Concatenate along the 4th (time) dimension
    concatenated_data <- do.call(abind::abind, c(all_data, list(along = 4)))
    
    # Create a new NeuroSpace for the combined data
    old_space <- x@space
    new_dim <- dim(old_space)
    new_dim[4] <- dim(concatenated_data)[4]
    
    new_space <- neuroim2::NeuroSpace(new_dim, spacing=neuroim2::spacing(old_space), 
                                      origin=neuroim2::origin(old_space), trans=neuroim2::trans(old_space))
    
    # Create a new DenseNeuroVec
    dense_concat_vec <- neuroim2::DenseNeuroVec(concatenated_data, new_space)
    
    # Write to a new temp parquet file and return a ParquetNeuroVec
    temp_path <- tempfile(fileext = ".fpar")
    neurovec_to_fpar(dense_concat_vec, temp_path, subject_id="temp")
    ParquetNeuroVec(temp_path)
  }
)
