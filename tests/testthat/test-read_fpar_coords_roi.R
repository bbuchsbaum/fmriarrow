library(testthat)

context("read_fpar_coords_roi")

skip_if_not_installed("neuroim2")
skip_if_not_installed("arrow")

space <- neuroim2::NeuroSpace(c(2,2,1,2))
arr <- array(seq_len(prod(c(2,2,1,2))), dim = c(2,2,1,2))
nv <- neuroim2::DenseNeuroVec(arr, space)

tmp <- tempfile(fileext = ".parquet")
neurovec_to_fpar(nv, tmp, "sub01")

test_that("single voxel roi", {
  tbl <- read_fpar_coords_roi(tmp, 0, 0, 0)
  df <- as.data.frame(tbl)
  expect_equal(nrow(df), 1)
  expect_equal(df$x, 0)
  expect_equal(df$y, 0)
  expect_equal(df$z, 0)
})

test_that("column selection works", {
  tbl <- read_fpar_coords_roi(tmp, c(0,1), c(0,1), c(0,0), columns = "bold")
  df <- as.data.frame(tbl)
  expect_equal(ncol(df), 1)
  expect_equal(nrow(df), 4)
})

test_that("approximate roi", {
  tbl <- read_fpar_coords_roi(tmp, c(0,1), c(0,1), c(0,0), exact = FALSE)
  df <- as.data.frame(tbl)
  expect_equal(nrow(df), 4)
})

test_that("roi returns correct bold series", {
  tbl <- read_fpar_coords_roi(tmp, 1, 0, 0)
  df <- as.data.frame(tbl)
  expect_equal(df$x, 1)
  expect_equal(df$y, 0)
  expect_equal(df$z, 0)
  expect_equal(df$bold[[1]], as.numeric(neuroim2::series(nv, 2,1,1)))
})
