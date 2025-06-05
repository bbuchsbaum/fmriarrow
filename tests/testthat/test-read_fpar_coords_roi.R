library(testthat)
library(neuroim2)
library(arrow)
library(dplyr)
devtools::load_all()

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
  result <- read_fpar_coords_roi(tmp, c(1, 1), c(1, 1), c(1, 1), exact = TRUE)
  df <- as.data.frame(result)
  expect_equal(nrow(df), 1)
  expect_equal(df$x[1], 1)
  expect_equal(df$y[1], 1)
  expect_equal(df$z[1], 1)
})

test_that("multi-voxel coordinate ROI query", {
  # Query a 2x2x1 region
  result <- read_fpar_coords_roi(tmp, c(0, 1), c(0, 1), c(0, 0), exact = TRUE)
  df <- as.data.frame(result)
  expect_equal(nrow(df), 4)
  expect_true(all(df$x %in% c(0, 1)))
  expect_true(all(df$y %in% c(0, 1)))
  expect_true(all(df$z == 0))
})

test_that("out-of-bounds coordinates fail gracefully", {
  # Query beyond the image bounds
  result <- read_fpar_coords_roi(tmp, c(10, 15), c(10, 15), c(10, 15), exact = TRUE)
  df <- as.data.frame(result)
  expect_equal(nrow(df), 0)
})

test_that("column selection works with exact filtering", {
  result <- read_fpar_coords_roi(tmp, c(0, 1), c(0, 1), c(0, 0),
                                 columns = "bold", exact = TRUE)
  df <- as.data.frame(result)
  expect_equal(names(df), "bold")
  expect_equal(nrow(df), 4)
})

test_that("approximate filtering returns at least as many rows", {
  exact_res <- read_fpar_coords_roi(tmp, c(0, 1), c(0, 1), c(0, 0), exact = TRUE)
  approx_res <- read_fpar_coords_roi(tmp, c(0, 1), c(0, 1), c(0, 0), exact = FALSE)
  expect_true(nrow(as.data.frame(approx_res)) >= nrow(as.data.frame(exact_res)))
})

test_that("oversized max_coord_bits rejected", {
  expect_error(
    read_fpar_coords_roi(tmp, x_range = c(0, 1024), y_range = 0, z_range = 0, max_coord_bits = 10),
    "exceeds range defined by max_coord_bits"
  )
})
