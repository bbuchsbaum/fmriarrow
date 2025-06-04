#' Convert a NeuroVec object to a Parquet file
#'
#' Convert a `neuroim2::NeuroVec` to a Z-order sorted Parquet file.
#'
#' The function iterates over all voxels of the input object, computes their
#' Morton (Z-order) index using [`compute_zindex()`], extracts the full BOLD
#' time series and writes the resulting table to disk using
#' `arrow::write_parquet()`.
#'
#' @param neuro_vec_obj A `neuroim2::NeuroVec` object.
#' @param output_parquet_path Path to the Parquet file to create.
#' @param subject_id Identifier for the subject being converted.
#' @param session_id Optional session identifier.
#' @param task_id Optional task identifier.
#' @param run_id Optional run identifier.
#' @param ... Additional arguments reserved for future extensions.
#'
#' @return A list containing the extracted `NeuroSpace`, the voxel data and
#'   the generated Arrow table. The Parquet file is written to
#'   `output_parquet_path`. The return value is primarily for testing and
#'   should not be considered part of the final API.
#' @export
neurovec_to_fpar <- function(neuro_vec_obj, output_parquet_path,
                             subject_id, session_id = NULL,
                             task_id = NULL, run_id = NULL, ...) {
  if (!inherits(neuro_vec_obj, "NeuroVec")) {
    stop("neuro_vec_obj must inherit from 'NeuroVec'")
  }

  space_obj <- neuroim2::space(neuro_vec_obj)

  dims <- dim(space_obj)
  if (length(dims) < 3) {
    stop("NeuroSpace must have at least 3 spatial dimensions")
  }

  n_vox <- prod(dims[1:3])
  x <- integer(n_vox)
  y <- integer(n_vox)
  z <- integer(n_vox)
  zindex <- integer(n_vox)
  bold <- vector("list", n_vox)

  for (i in seq_len(n_vox)) {
    coords <- neuroim2::index_to_grid(space_obj, i)
    x[i] <- coords[1] - 1L
    y[i] <- coords[2] - 1L
    z[i] <- coords[3] - 1L
    zindex[i] <- compute_zindex(x[i], y[i], z[i])
    bold[[i]] <- as.numeric(neuroim2::series(neuro_vec_obj,
                                             coords[1], coords[2], coords[3]))
  }

  voxel_data <- data.frame(
    subject_id = rep(subject_id, n_vox),
    session_id = rep(session_id, n_vox),
    task_id = rep(task_id, n_vox),
    run_id = rep(run_id, n_vox),
    x = x, y = y, z = z,
    zindex = zindex,
    stringsAsFactors = FALSE
  )
  voxel_data$bold <- I(bold)

  arrow_tbl <- arrow::arrow_table(voxel_data)
  arrow_tbl <- dplyr::arrange(arrow_tbl, zindex)

  arrow::write_parquet(
    arrow_tbl,
    output_parquet_path,
    row_group_size = 4096,
    compression = "zstd",
    write_statistics = TRUE
  )

  list(
    neuro_vec_obj = neuro_vec_obj,
    output_parquet_path = output_parquet_path,
    subject_id = subject_id,
    session_id = session_id,
    task_id = task_id,
    run_id = run_id,
    space = space_obj,
    voxel_data = voxel_data,
    arrow_table = arrow_tbl
  )
}
