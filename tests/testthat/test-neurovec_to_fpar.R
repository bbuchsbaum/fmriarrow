library(testthat)

context("neurovec_to_fpar")

test_that("input must be a NeuroVec", {
  expect_error(neurovec_to_fpar(list(), "file.parquet", "sub01"))
})

# This test exercises the basic return structure when neuroim2 is available.
test_that("basic invocation", {
  skip_if_not_installed("neuroim2")

  nv <- neuroim2::emptyNeuroVec(c(1,1,1,1))
  res <- neurovec_to_fpar(nv, tempfile(), "sub01")
  expect_true(inherits(res$space, "NeuroSpace"))
})

test_that("voxel extraction works", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")

  space <- neuroim2::NeuroSpace(c(2,2,1,2))
  arr <- array(seq_len(prod(c(2,2,1,2))), dim = c(2,2,1,2))
  nv <- neuroim2::DenseNeuroVec(arr, space)

  res <- neurovec_to_fpar(nv, tempfile(), "sub01")
  df <- res$voxel_data

  expect_equal(nrow(df), 4)
  expect_equal(length(df$bold[[1]]), 2)
  expect_true(all(df$x >= 0 & df$y >= 0 & df$z >= 0))
  expect_equal(sort(unique(df$zindex)), c(0L,1L,2L,3L))
})

test_that("arrow table is sorted by zindex", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")

  space <- neuroim2::NeuroSpace(c(2,2,1,2))
  arr <- array(seq_len(prod(c(2,2,1,2))), dim = c(2,2,1,2))
  nv <- neuroim2::DenseNeuroVec(arr, space)

  res <- neurovec_to_fpar(nv, tempfile(), "sub01")
  tbl <- res$arrow_table

  expect_true(inherits(tbl, "Table"))
  df <- as.data.frame(tbl)
  expect_equal(df$zindex, sort(df$zindex))
})
