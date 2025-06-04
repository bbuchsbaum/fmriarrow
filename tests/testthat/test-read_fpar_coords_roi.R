library(testthat)

context("read_fpar_coords_roi")

skip_if_not_installed("neuroim2")
skip_if_not_installed("arrow")

# Set up test data
space <- neuroim2::NeuroSpace(c(3,3,2,2))
arr <- array(seq_len(prod(c(3,3,2,2))), dim = c(3,3,2,2))
nv <- neuroim2::DenseNeuroVec(arr, space)

tmp <- tempfile(fileext = ".parquet")
neurovec_to_fpar(nv, tmp, "sub01")

test_that("basic coordinate ROI query", {
  # Query a single voxel
  result <- read_fpar_coords_roi(tmp, c(1, 1), c(1, 1), c(1, 1))
  expect_equal(nrow(result), 1)
  expect_equal(result$x[1], 1)
  expect_equal(result$y[1], 1)
  expect_equal(result$z[1], 1)
})

test_that("multi-voxel coordinate ROI query", {
  # Query a 2x2x1 region
  result <- read_fpar_coords_roi(tmp, c(0, 1), c(0, 1), c(0, 0))
  expect_equal(nrow(result), 4)
  expect_true(all(result$x %in% c(0, 1)))
  expect_true(all(result$y %in% c(0, 1)))
  expect_true(all(result$z == 0))
})

test_that("out-of-bounds coordinates fail gracefully", {
  # Query beyond the image bounds
  result <- read_fpar_coords_roi(tmp, c(10, 15), c(10, 15), c(10, 15))
  expect_equal(nrow(result), 0)
})
