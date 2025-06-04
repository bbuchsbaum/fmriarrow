#' Basic performance sanity check
#'
#' Measures the time required to convert a `neuroim2::NeuroVec` to
#' a Parquet file using [neurovec_to_fpar()] and to query a small
#' region of interest from that Parquet file. For comparison, it also
#' times extraction of the same region directly from the
#' `neuroim2::NeuroVec` using `neuroim2::series()`.
#'
#' @param neuro_vec_obj A `neuroim2::NeuroVec` object to benchmark.
#' @param x_range,y_range,z_range Integer vectors (length 1 or 2)
#'   specifying a cuboid ROI in 0-based coordinates used for the
#'   query benchmark.
#'
#' @return Named numeric vector with elapsed times (in seconds) for
#'   conversion, Parquet ROI query and direct `neuroim2` extraction.
#' @export
performance_sanity_check <- function(neuro_vec_obj,
                                     x_range = 0,
                                     y_range = 0,
                                     z_range = 0) {
  if (!inherits(neuro_vec_obj, "NeuroVec")) {
    stop("neuro_vec_obj must inherit from 'NeuroVec'")
  }

  # Validate coordinate ranges before using them
  x_range <- validate_coordinate_range(x_range, "x_range")
  y_range <- validate_coordinate_range(y_range, "y_range")
  z_range <- validate_coordinate_range(z_range, "z_range")

  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)

  t_conv <- system.time(
    neurovec_to_fpar(neuro_vec_obj, tmp, "perfsubj")
  )["elapsed"]

  t_query <- system.time(
    read_fpar_coords_roi(tmp, x_range, y_range, z_range)
  )["elapsed"]

  neuro_x <- seq.int(x_range[1], x_range[length(x_range)]) + 1L
  neuro_y <- seq.int(y_range[1], y_range[length(y_range)]) + 1L
  neuro_z <- seq.int(z_range[1], z_range[length(z_range)]) + 1L

  t_nv <- system.time({
    for (xx in neuro_x) {
      for (yy in neuro_y) {
        for (zz in neuro_z) {
          neuroim2::series(neuro_vec_obj, xx, yy, zz)
        }
      }
    }
  })["elapsed"]

  unlink(tmp)

  c(
    conversion_time_sec = unname(t_conv),
    fpar_query_time_sec = unname(t_query),
    neuroim2_subset_time_sec = unname(t_nv)
  )
}
