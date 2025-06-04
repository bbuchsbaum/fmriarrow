test_that("voxel_size_mm always length three", {
  skip_if_not_installed("neuroim2")
  space <- neuroim2::NeuroSpace(dim = c(2, 2, 2, 1), spacing = c(2))
  arr <- array(1, dim = c(2, 2, 2, 1))
  nv <- neuroim2::DenseNeuroVec(arr, space)
  md <- fmriarrow:::extract_neurospace_metadata(nv)
  expect_equal(length(md$spatial_properties$voxel_size_mm), 3)
})
