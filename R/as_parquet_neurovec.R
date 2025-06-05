#' Convert NeuroVec to ParquetNeuroVec in One Call
#'
#' This convenience function combines `neurovec_to_fpar()` and `ParquetNeuroVec()`
#' creation into a single function call, mirroring the workflow of converting
#' from standard neuroimaging formats to the optimized Parquet format.
#'
#' @param neuro_vec_obj A `neuroim2::NeuroVec` object to convert.
#' @param output_parquet_path Path where the .fpar file should be created.
#' @param subject_id Identifier for the subject being converted.
#' @param session_id Optional session identifier.
#' @param task_id Optional task identifier.
#' @param run_id Optional run identifier.
#' @param reference_space Optional reference space identifier (e.g., "MNI152NLin2009cAsym").
#' @param repetition_time Optional repetition time in seconds.
#' @param lazy logical. Whether the resulting ParquetNeuroVec should use lazy loading (default: TRUE).
#' @param ... Additional arguments passed to `neurovec_to_fpar()`.
#'
#' @return A `ParquetNeuroVec` object ready for use.
#'
#' @details
#' This function streamlines the workflow by:
#' 1. Writing the NeuroVec data to Parquet format with spatial indexing
#' 2. Creating a ParquetNeuroVec object that provides array-like access
#' 
#' The resulting object maintains full compatibility with `neuroim2::NeuroVec`
#' while leveraging the performance benefits of Parquet's columnar storage
#' and Z-order spatial indexing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create from existing NeuroVec
#' neuro_vec <- neuroim2::read_vec("data.nii.gz")
#' pvec <- as_parquet_neurovec(
#'   neuro_vec, 
#'   "data.fpar", 
#'   subject_id = "sub-01",
#'   task_id = "rest",
#'   repetition_time = 2.0
#' )
#' 
#' # Now use with array-like syntax
#' ts <- series(pvec, 32, 32, 15)
#' roi_ts <- series_roi(pvec, 30:40, 30:40, 10:20)
#' subarray <- pvec[25:35, 25:35, 10:15, 1:50]
#' }
as_parquet_neurovec <- function(neuro_vec_obj, output_parquet_path, subject_id,
                               session_id = NULL, task_id = NULL, run_id = NULL,
                               reference_space = NULL, repetition_time = NULL,
                               lazy = TRUE, ...) {
  
  # Step 1: Convert NeuroVec to Parquet format
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec_obj,
    output_parquet_path = output_parquet_path,
    subject_id = subject_id,
    session_id = session_id,
    task_id = task_id,
    run_id = run_id,
    reference_space = reference_space,
    repetition_time = repetition_time,
    ...
  )
  
  # Step 2: Create ParquetNeuroVec object
  pvec <- ParquetNeuroVec(output_parquet_path, lazy = lazy)
  
  return(pvec)
} 