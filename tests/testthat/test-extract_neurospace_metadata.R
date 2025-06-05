library(testthat)
library(neuroim2)

devtools::load_all()

context("extract_neurospace_metadata")

# These tests require the neuroim2 package for NeuroVec creation
skip_if_not_installed("neuroim2")

# Create a simple NeuroVec for testing
space <- neuroim2::NeuroSpace(c(2,2,1,3))
arr <- array(1, dim = c(2,2,1,3))
nv <- neuroim2::DenseNeuroVec(arr, space)

md <- extract_neurospace_metadata(
  nv,
  reference_space = "TESTSPACE",
  repetition_time = 2
)


test_that("metadata fields are populated", {
  expect_equal(md$metadata_schema_version, "2.0.0")
  expect_equal(md$spatial_properties$original_dimensions, c(2L, 2L, 1L, 3L))
  expect_equal(md$acquisition_properties$timepoint_count, 3L)
})


test_that("optional parameters propagate", {
  expect_equal(md$spatial_properties$reference_space, "TESTSPACE")
  expect_equal(md$acquisition_properties$repetition_time_s, 2)
})


test_that("voxel_size_mm always length three", {
  skip_if_not_installed("neuroim2")
  # Create a valid 3D NeuroSpace first
  space <- neuroim2::NeuroSpace(dim = c(2, 2, 2, 1), spacing = c(2,2,2))
  
  # Manually set the spacing to a single value to test recycling.
  # This bypasses the constructor's validation but creates the test condition.
  space@spacing <- c(2)
  
  arr <- array(1, dim = c(2, 2, 2, 1))
  nv <- neuroim2::DenseNeuroVec(arr, space)
  
  md <- extract_neurospace_metadata(nv)
  expect_equal(length(md$spatial_properties$voxel_size_mm), 3)
  expect_equal(md$spatial_properties$voxel_size_mm, c(2, 2, 2))
})

