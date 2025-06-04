#' Extract metadata from a NeuroSpace object
#'
#' Internal helper used by [neurovec_to_fpar()] to construct the
#' metadata list described in the Sprint 2 proposal.
#'
#' @param neuro_vec_obj A `neuroim2::NeuroVec` object.
#' @param reference_space Optional reference space identifier.
#' @param repetition_time Optional repetition time in seconds.
#'
#' @return A metadata list following the proposal specification.
#' @noRd
extract_neurospace_metadata <- function(neuro_vec_obj,
                                        reference_space = NULL,
                                        repetition_time = NULL) {
  s <- neuroim2::space(neuro_vec_obj)
  spacing_vec <- neuroim2::spacing(s)

  list(
    metadata_schema_version = "2.0.0",
    source_info = list(
      original_file = deparse(substitute(neuro_vec_obj)),
      neuroim2_space_hash = digest::digest(s, algo = "sha256")
    ),
    spatial_properties = list(
      original_dimensions = as.integer(dim(s)),
      voxel_size_mm = as.numeric(spacing_vec[1:min(3, length(spacing_vec))]),
      affine_matrix = as.matrix(neuroim2::trans(s)),
      reference_space = reference_space %||% "unknown",
      coordinate_convention = "0-based RAS"
    ),
    acquisition_properties = list(
      repetition_time_s = repetition_time %||% NA_real_,
      timepoint_count = as.integer(dim(neuro_vec_obj)[4])
    ),
    data_integrity = list(
      voxel_count = as.integer(prod(dim(s)[1:3])),
      bold_value_range = c(NA_real_, NA_real_)
    )
  )
}
