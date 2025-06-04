#' Convert a NeuroVec object to a Parquet file
#'
#' This is the initial stub for the conversion function described in the
#' project proposal. It currently only performs basic input validation and
#' extracts the associated NeuroSpace object. Later tasks will implement the
#' actual voxel iteration and Parquet writing steps.
#'
#' @param neuro_vec_obj A `neuroim2::NeuroVec` object.
#' @param output_parquet_path Path to the Parquet file to create.
#' @param subject_id Identifier for the subject being converted.
#' @param session_id Optional session identifier.
#' @param task_id Optional task identifier.
#' @param run_id Optional run identifier.
#' @param ... Additional arguments reserved for future extensions.
#'
#' @return A list containing the extracted `NeuroSpace` object and the
#'   supplied identifiers. The return value is primarily for testing
#'   purposes and should not be considered part of the final API.
#' @export
neurovec_to_fpar <- function(neuro_vec_obj, output_parquet_path,
                             subject_id, session_id = NULL,
                             task_id = NULL, run_id = NULL, ...) {
  if (!inherits(neuro_vec_obj, "NeuroVec")) {
    stop("neuro_vec_obj must inherit from 'NeuroVec'")
  }

  space_obj <- neuroim2::space(neuro_vec_obj)

  list(
    neuro_vec_obj = neuro_vec_obj,
    output_parquet_path = output_parquet_path,
    subject_id = subject_id,
    session_id = session_id,
    task_id = task_id,
    run_id = run_id,
    space = space_obj
  )
}
