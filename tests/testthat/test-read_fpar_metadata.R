library(testthat)

context("read_fpar_metadata")

skip_if_not_installed("neuroim2")
skip_if_not_installed("arrow")

space <- neuroim2::NeuroSpace(c(2, 2, 1, 3))
arr <- array(seq_len(prod(c(2, 2, 1, 3))), dim = c(2, 2, 1, 3))
nv <- neuroim2::DenseNeuroVec(arr, space)

tmp <- tempfile(fileext = ".parquet")

neurovec_to_fpar(nv, tmp, "sub01", reference_space = "TESTSPACE", repetition_time = 2)

md <- read_fpar_metadata(tmp)

test_that("metadata fields round trip", {
  expect_equal(md$spatial_properties$original_dimensions, c(2L, 2L, 1L, 3L))
  expect_equal(md$spatial_properties$reference_space, "TESTSPACE")
  expect_equal(md$spatial_properties$coordinate_convention, "0-based RAS")
  expect_equal(md$acquisition_properties$repetition_time_s, 2)
  expect_equal(md$acquisition_properties$timepoint_count, 3L)
  expect_equal(md$data_integrity$voxel_count, 4L)
  expect_length(md$data_integrity$bold_value_range, 2)
  expect_true(all(is.finite(md$data_integrity$bold_value_range)))
})

test_that("warning for non-standard extension", {
  tmp2 <- tempfile(fileext = ".data")
  file.copy(tmp, tmp2)
  expect_warning(read_fpar_metadata(tmp2), "File extension should")
})

test_that("raw schema metadata parsed", {
  skip_if_not_installed("arrow")

  json_md <- jsonlite::toJSON(list(test_value = 123), auto_unbox = TRUE)
  tbl <- arrow::arrow_table(dummy = 1L)
  sch <- tbl$schema$WithMetadata(list(spatial_metadata = charToRaw(json_md)))
  tbl <- tbl$with_schema(sch)
  raw_tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tbl, raw_tmp)

  md_raw <- read_fpar_metadata(raw_tmp)
  expect_equal(md_raw$test_value, 123)
})
