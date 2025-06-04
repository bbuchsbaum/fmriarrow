library(testthat)

context("read_fpar_zindex_range")

test_that("basic zindex range query", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")

  space <- neuroim2::NeuroSpace(c(2,2,1,2))
  arr <- array(seq_len(prod(c(2,2,1,2))), dim = c(2,2,1,2))
  nv <- neuroim2::DenseNeuroVec(arr, space)

  tmp <- tempfile(fileext = ".parquet")
  neurovec_to_fpar(nv, tmp, "sub01")

  tbl <- read_fpar_zindex_range(tmp, 1L, 2L)
  expect_true(inherits(tbl, "Table"))
  df <- as.data.frame(tbl)
  expect_equal(sort(unique(df$zindex)), 1:2)
})

test_that("invalid column names trigger error", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")

  space <- neuroim2::NeuroSpace(c(2,2,1,2))
  arr <- array(seq_len(prod(c(2,2,1,2))), dim = c(2,2,1,2))
  nv <- neuroim2::DenseNeuroVec(arr, space)

  tmp <- tempfile(fileext = ".parquet")
  neurovec_to_fpar(nv, tmp, "sub01")

  expect_error(read_fpar_zindex_range(tmp, 0L, 1L, columns = "notacol"))
})
