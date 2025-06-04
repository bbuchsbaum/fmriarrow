library(testthat)

context("neurovec_to_fpar")

test_that("input must be a NeuroVec", {
  expect_error(neurovec_to_fpar(list(), "file.parquet", "sub01"))
})

test_that("scan identifiers validated", {
  skip_if_not_installed("neuroim2")
  # Create a minimal NeuroVec with 1x1x1x1 dimensions
  space <- neuroim2::NeuroSpace(c(1,1,1,1))
  arr <- array(1, dim = c(1,1,1,1))
  nv <- neuroim2::DenseNeuroVec(arr, space)
  
  expect_error(neurovec_to_fpar(nv, tempfile(), NA_character_))
  expect_error(neurovec_to_fpar(nv, tempfile(), "sub01", session_id = 1))
})

# This test exercises the basic return structure when neuroim2 is available.
test_that("basic invocation", {
  skip_if_not_installed("neuroim2")

  # Create a minimal NeuroVec with 1x1x1x1 dimensions
  space <- neuroim2::NeuroSpace(c(1,1,1,1))
  arr <- array(1, dim = c(1,1,1,1))
  nv <- neuroim2::DenseNeuroVec(arr, space)
  
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

test_that("arrow table has expected columns", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")

  # Create a minimal NeuroVec with 1x1x1x1 dimensions
  space <- neuroim2::NeuroSpace(c(1,1,1,1))
  arr <- array(1, dim = c(1,1,1,1))
  nv <- neuroim2::DenseNeuroVec(arr, space)
  
  res <- neurovec_to_fpar(nv, tempfile(), "sub01")
  tbl <- res$arrow_table
  expected <- c("subject_id", "session_id", "task_id", "run_id",
                "x", "y", "z", "zindex", "bold")
  expect_equal(sort(tbl$schema$names), sort(expected))
})

test_that("arrow table has expected data types", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")

  space <- neuroim2::NeuroSpace(c(1,1,1,2))
  arr <- array(1, dim = c(1,1,1,2))
  nv <- neuroim2::DenseNeuroVec(arr, space)

  res <- neurovec_to_fpar(nv, tempfile(), "sub01")
  sch <- res$arrow_table$schema

  expect_true(inherits(sch$GetFieldByName("x")$type, "UInt16Type"))
  expect_true(inherits(sch$GetFieldByName("y")$type, "UInt16Type"))
  expect_true(inherits(sch$GetFieldByName("z")$type, "UInt16Type"))
  expect_true(inherits(sch$GetFieldByName("zindex")$type, "UInt32Type"))
  expect_true(inherits(sch$GetFieldByName("bold")$type, "FixedSizeListType"))
})

test_that("parquet file written and sorted", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")

  space <- neuroim2::NeuroSpace(c(2,2,1,2))
  arr <- array(seq_len(prod(c(2,2,1,2))), dim = c(2,2,1,2))
  nv <- neuroim2::DenseNeuroVec(arr, space)

  tmp <- tempfile(fileext = ".parquet")
  neurovec_to_fpar(nv, tmp, "sub01")

  expect_true(file.exists(tmp))
  tbl <- arrow::read_parquet(tmp)
  df <- as.data.frame(tbl)
  expect_equal(df$zindex, sort(df$zindex))
})

test_that("metadata stored and retrievable", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")

  space <- neuroim2::NeuroSpace(c(2,2,1,1))
  arr <- array(1, dim = c(2,2,1,1))
  nv <- neuroim2::DenseNeuroVec(arr, space)

  tmp <- tempfile(fileext = ".parquet")
  neurovec_to_fpar(nv, tmp, "sub01", reference_space = "test")

  md <- read_fpar_metadata(tmp)
  expect_equal(md$spatial_properties$original_dimensions, c(2,2,1,1))
  expect_equal(md$spatial_properties$reference_space, "test")
})
