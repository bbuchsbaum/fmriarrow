library(testthat)

context("read_fpar_zindex_range")

skip_if_not_installed("neuroim2")
skip_if_not_installed("arrow")

# Set up test data
space <- neuroim2::NeuroSpace(c(2,2,1,2))
arr <- array(seq_len(prod(c(2,2,1,2))), dim = c(2,2,1,2))
nv <- neuroim2::DenseNeuroVec(arr, space)

tmp <- tempfile(fileext = ".parquet")
neurovec_to_fpar(nv, tmp, "sub01")

test_that("basic zindex range query", {
  result <- read_fpar_zindex_range(tmp, 0, 1)
  expect_equal(nrow(result), 2)
  expect_true(all(result$zindex %in% c(0, 1)))
})

test_that("single zindex query", {
  result <- read_fpar_zindex_range(tmp, 2, 2)
  expect_equal(nrow(result), 1)
  expect_equal(result$zindex[1], 2)
})

test_that("invalid column names trigger error", {
  expect_error(read_fpar_zindex_range(tmp, 0, 1, columns = c("bad_column")))
})

test_that("empty range returns empty result", {
  result <- read_fpar_zindex_range(tmp, 100, 200)
  expect_equal(nrow(result), 0)
})

test_that("invalid zindex range fails", {
  expect_error(read_fpar_zindex_range(tmp, 5, 2))  # min > max
})
