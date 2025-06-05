#' Read Parquet-format fMRI Data into ParquetNeuroVec
#'
#' Reads a .fpar (fMRI Parquet) file into a `ParquetNeuroVec` object,
#' mirroring the `neuroim2::read_vec()` pattern for reading neuroimaging data.
#' This function provides the standard entry point for accessing Parquet-stored
#' fMRI data with array-like syntax and spatial indexing.
#'
#' @param file_name character. Path to the .fpar file to read.
#' @param lazy logical. Whether to use lazy loading for data access (default: TRUE).
#'   Lazy loading is recommended for large datasets to minimize memory usage.
#'
#' @return A `ParquetNeuroVec` object providing NeuroVec-compatible access
#'   to the spatially-indexed fMRI data.
#'
#' @details
#' This function mirrors the `neuroim2::read_vec()` API pattern:
#' - `read_vec()` reads NIfTI/ANALYZE files → `NeuroVec` objects
#' - `read_arrow()` reads .fpar files → `ParquetNeuroVec` objects
#' 
#' The resulting `ParquetNeuroVec` object supports:
#' - Array-like access: `obj[x, y, z, t]`
#' - Time series extraction: `series(obj, x, y, z)`
#' - ROI analysis: `series_roi(obj, x_range, y_range, z_range)`
#' - Standard NeuroVec methods: `dim()`, `length()`, etc.
#'
#' @seealso 
#' - `ParquetNeuroVec()` for direct object creation
#' - `as_parquet_neurovec()` for NeuroVec → ParquetNeuroVec conversion
#' - `neurovec_to_fpar()` for creating .fpar files from NeuroVec objects
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read .fpar file (mirrors read_vec pattern)
#' pvec <- read_arrow("subject_data.fpar")
#' 
#' # Use with familiar NeuroVec-like syntax
#' dims <- dim(pvec)                           # Get dimensions
#' n_timepoints <- length(pvec)                # Get temporal length  
#' 
#' # Extract time series
#' voxel_ts <- series(pvec, 32, 32, 15)        # Single voxel
#' roi_ts <- series_roi(pvec, 30:35, 30:35, 10:20)  # ROI average
#' 
#' # Array subsetting
#' sub_data <- pvec[25:35, 25:35, 10:15, 1:50] # 4D subarray
#' 
#' # Compare with neuroim2 workflow:
#' # neuro_vec <- neuroim2::read_vec("data.nii.gz")  # NIfTI → NeuroVec
#' # pvec <- read_arrow("data.fpar")                  # Parquet → ParquetNeuroVec
#' }
read_arrow <- function(file_name, lazy = TRUE) {
  # Validate input
  if (!is.character(file_name) || length(file_name) != 1) {
    stop("file_name must be a single character string")
  }
  
  if (!file.exists(file_name)) {
    stop("File not found: ", file_name)
  }
  
  # Create ParquetNeuroVec object
  ParquetNeuroVec(file_name, lazy = lazy)
} 