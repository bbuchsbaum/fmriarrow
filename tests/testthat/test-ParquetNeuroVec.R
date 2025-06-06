library(testthat)
library(neuroim2)
library(arrow)
library(dplyr)

devtools::load_all()

test_that("ParquetNeuroVec class creation and basic functionality", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create test data
  dims <- c(10, 10, 8, 5)
  data <- array(rnorm(prod(dims)), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  # Create temporary parquet file
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_neurovec.fpar")
  
  # Convert to parquet format
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec,
    output_parquet_path = parquet_path,
    subject_id = "test_subject",
    task_id = "test_task",
    run_id = "01",
    session_id = "ses01"
  )
  
  # Test ParquetNeuroVec creation
  expect_no_error(pvec <- ParquetNeuroVec(parquet_path))
  
  # Test class inheritance
  expect_s4_class(pvec, "ParquetNeuroVec")
  expect_s4_class(pvec, "NeuroVec")  # Should inherit from NeuroVec
  
  # Test basic properties
  expect_equal(dim(pvec), dims)
  expect_equal(length(pvec), dims[4])
  expect_true(file.exists(pvec@parquet_path))
  expect_equal(pvec@parquet_path, parquet_path)
  expect_true(pvec@lazy)
  
  # Test metadata access
  expect_type(pvec@metadata, "list")
  expect_true("spatial_properties" %in% names(pvec@metadata))
  expect_true("acquisition_properties" %in% names(pvec@metadata))
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec series method with integer coordinates", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create test data with known values
  dims <- c(5, 5, 3, 4)
  data <- array(1:prod(dims), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  # Create temporary parquet file
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_series.fpar")
  
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec,
    output_parquet_path = parquet_path,
    subject_id = "test_subject"
  )
  
  pvec <- ParquetNeuroVec(parquet_path)
  
  # Test series extraction at specific coordinates
  ts <- series(pvec, 1L, 1L, 1L)
  expect_type(ts, "double")
  expect_length(ts, dims[4])
  
  # Test series extraction with numeric coordinates (should convert to integer)
  ts_numeric <- series(pvec, 1.0, 1.0, 1.0)
  expect_equal(ts, ts_numeric)
  
  # Test bounds checking
  expect_error(series(pvec, 0L, 1L, 1L), "out of bounds")
  expect_error(series(pvec, dims[1] + 1L, 1L, 1L), "out of bounds")
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec series method with matrix coordinates", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create small test dataset
  dims <- c(4, 4, 2, 3)
  data <- array(rnorm(prod(dims)), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_matrix_series.fpar")
  
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec,
    output_parquet_path = parquet_path,
    subject_id = "test_subject"
  )
  
  pvec <- ParquetNeuroVec(parquet_path)
  
  # Test matrix coordinate extraction
  coord_matrix <- matrix(c(1, 1, 1,
                          2, 2, 1,
                          3, 3, 2), 
                        nrow = 3, ncol = 3, byrow = TRUE)
  
  series_matrix <- series(pvec, coord_matrix)
  expect_true(is.matrix(series_matrix))
  expect_equal(nrow(series_matrix), dims[4])
  expect_equal(ncol(series_matrix), nrow(coord_matrix))

  # Out of bounds matrix should error
  oob_matrix <- matrix(c(dims[1] + 1, 1, 1), ncol = 3)
  expect_error(series(pvec, oob_matrix), "out of bounds")
  
  # Test error for invalid matrix dimensions
  invalid_matrix <- matrix(c(1, 1), nrow = 1, ncol = 2)
  expect_error(series(pvec, invalid_matrix), "3 columns")
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec series_roi_bbox methods", {
  # Setup: Create a NeuroVec and Parquet file
  dims <- c(10, 10, 10, 5)
  neuro_vec <- neuroim2::NeuroVec(array(runif(prod(dims)), dims), neuroim2::NeuroSpace(dims))
  parquet_path <- tempfile(fileext = ".fpar")
  neurovec_to_fpar(neuro_vec_obj = neuro_vec, output_parquet_path = parquet_path, subject_id = "test_subject")
  pvec <- ParquetNeuroVec(parquet_path)
  
  # Test with a 3x3x3 ROI
  roi_ts <- series_roi_bbox(pvec, 2:4, 2:4, 2:4)
  expect_equal(length(roi_ts), 5)
  
  # Test with a single voxel ROI
  single_voxel_ts <- series_roi_bbox(pvec, 5, 5, 5)
  expect_equal(length(single_voxel_ts), 5)
  
  # Test that it computes the mean correctly
  v1 <- series(pvec, 2, 2, 2)
  v2 <- series(pvec, 2, 2, 3)
  mean_ts <- (v1 + v2) / 2
  roi_mean_ts <- series_roi_bbox(pvec, 2, 2, 2:3)
  expect_equal(roi_mean_ts, mean_ts)
  
  # Test out-of-bounds error
  expect_error(series_roi_bbox(pvec, dims[1] + 1L, 1L, 1L), "out of bounds")
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec array subsetting", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create small test dataset for easier verification
  dims <- c(3, 3, 2, 4)
  data <- array(1:prod(dims), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_subset.fpar")
  
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec,
    output_parquet_path = parquet_path,
    subject_id = "test_subject"
  )
  
  pvec <- ParquetNeuroVec(parquet_path)
  
  # Test that subsetting returns an array with the correct dimensions
  subset_data <- pvec[1:2, 1:2, 1, 1:2, drop = FALSE]
  expect_true(is.array(subset_data))
  expect_equal(dim(subset_data), c(2, 2, 1, 2))
  expect_equal(subset_data, neuro_vec[1:2, 1:2, 1, 1:2, drop = FALSE])

  # Test subsetting a single value
  expect_true(is.numeric(pvec[1, 1, 1, 1]))
  expect_equal(pvec[1,1,1,1], neuro_vec[1,1,1,1])

  # Test subsetting a time series
  ts_data <- pvec[1, 1, 1, , drop = FALSE]
  expect_true(is.array(ts_data))
  expect_equal(dim(ts_data), c(1,1,1,4))
  expect_equal(ts_data, neuro_vec[1,1,1,, drop = FALSE])
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec show method", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create test data
  dims <- c(5, 5, 3, 10)
  data <- array(rnorm(prod(dims)), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_show.fpar")
  
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec,
    output_parquet_path = parquet_path,
    subject_id = "test_subject",
    repetition_time = 2.0
  )
  
  pvec <- ParquetNeuroVec(parquet_path)
  
  # Test show method produces output without error
  expect_output(show(pvec), "ParquetNeuroVec Object")
  expect_output(show(pvec), "Storage Information")
  expect_output(show(pvec), "Spatial Properties")
  expect_output(show(pvec), "Access Methods")
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec lazy vs non-lazy loading", {
  # Setup: Create a NeuroVec and Parquet file
  neuro_vec <- neuroim2::NeuroVec(array(rnorm(10*10*10*5), c(10,10,10,5)), 
                                  neuroim2::NeuroSpace(c(10,10,10,5)))
  parquet_path <- tempfile(fileext = ".fpar")
  neurovec_to_fpar(neuro_vec_obj = neuro_vec, output_parquet_path = parquet_path, subject_id = "test_subject")
  
  # Lazy loading (default)
  pvec_lazy <- ParquetNeuroVec(parquet_path)
  expect_true(pvec_lazy@lazy)
  
  # Eager loading
  pvec_eager <- ParquetNeuroVec(parquet_path, lazy = FALSE)
  expect_false(pvec_eager@lazy)
  
  # Check that data dimensions are correct after eager loading
  # We access data via a method, not directly from a slot
  all_data <- series_roi_bbox(pvec_eager, 1:10, 1:10, 1:10)
  expect_equal(length(all_data), 5)
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec error handling", {
  skip_if_not_installed("neuroim2")
  
  # Test non-existent file
  expect_error(ParquetNeuroVec("nonexistent.fpar"), "not found")
  
  # Test invalid file extension (should warn but not error)
  temp_dir <- tempdir()
  invalid_path <- file.path(temp_dir, "test.txt")
  file.create(invalid_path)
  
  expect_warning(
    expect_error(ParquetNeuroVec(invalid_path), "Spatial properties"),
    "File extension"
  )
  
  # Cleanup
  unlink(invalid_path)
})

test_that("ParquetNeuroVec can be converted to a matrix", {
  # Setup
  dims <- c(5, 5, 5, 5)
  neuro_vec <- neuroim2::NeuroVec(array(rnorm(prod(dims)), dims), neuroim2::NeuroSpace(dims))
  parquet_path <- tempfile(fileext = ".fpar")
  neurovec_to_fpar(neuro_vec_obj = neuro_vec, output_parquet_path = parquet_path, subject_id = "test_subject")
  pvec <- ParquetNeuroVec(parquet_path)
  
  # Test as.matrix conversion
  mat <- as.matrix(pvec)
  expect_equal(dim(mat), c(prod(dims[1:3]), dims[4]))
  
  # Compare with original data - use expect_equivalent for float tolerance
  orig_mat <- as.matrix(neuro_vec)
  expect_equivalent(mat, orig_mat, tolerance = 1e-6)
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec can be subset like an array", {
  # Setup
  dims <- c(5, 5, 5, 5)
  neuro_vec <- neuroim2::NeuroVec(array(rnorm(prod(dims)), dims), neuroim2::NeuroSpace(dims))
  parquet_path <- tempfile(fileext = ".fpar")
  neurovec_to_fpar(neuro_vec_obj = neuro_vec, output_parquet_path = parquet_path, subject_id = "test_subject")
  pvec <- ParquetNeuroVec(parquet_path)
  
  # Test that subsetting returns an array with the correct dimensions
  subset_data <- pvec[1:2, 1:2, 1, 1:2, drop = FALSE]
  expect_true(is.array(subset_data))
  expect_equal(dim(subset_data), c(2, 2, 1, 2))
  expect_equal(subset_data, neuro_vec[1:2, 1:2, 1, 1:2, drop = FALSE])

  # Test subsetting a single value
  expect_true(is.numeric(pvec[1, 1, 1, 1]))
  expect_equal(pvec[1,1,1,1], neuro_vec[1,1,1,1])

  # Test subsetting a time series
  ts_data <- pvec[1, 1, 1, , drop = FALSE]
  expect_true(is.array(ts_data))
  expect_equal(dim(ts_data), c(1,1,1,5))
  expect_equal(ts_data, neuro_vec[1,1,1,, drop = FALSE])
  
  # Cleanup
  unlink(parquet_path)
})

test_that("sub_vector extracts a subset of time points", {
  # Setup
  dims <- c(5, 5, 5, 10)
  neuro_vec <- neuroim2::NeuroVec(array(rnorm(prod(dims)), dims), neuroim2::NeuroSpace(dims))
  parquet_path <- tempfile(fileext = ".fpar")
  neurovec_to_fpar(neuro_vec_obj = neuro_vec, output_parquet_path = parquet_path, subject_id = "test_subject")
  pvec <- ParquetNeuroVec(parquet_path)

  # Extract sub-vector
  pvec_sub <- sub_vector(pvec, 1:5)
  
  # Check dimensions
  expect_s4_class(pvec_sub, "ParquetNeuroVec")
  expect_equal(dim(pvec_sub), c(5, 5, 5, 5))
  
  # Check data consistency
  expect_equivalent(pvec_sub[1,1,1,], neuro_vec[1,1,1,1:5], tolerance = 1e-6)
  
  # Cleanup
  unlink(parquet_path)
})

test_that("concat combines two ParquetNeuroVec objects", {
  # Setup for vec 1
  dims1 <- c(5, 5, 5, 5)
  neuro_vec1 <- neuroim2::NeuroVec(array(rnorm(prod(dims1)), dims1), neuroim2::NeuroSpace(dims1))
  parquet_path1 <- tempfile(fileext = ".fpar")
  neurovec_to_fpar(neuro_vec1, parquet_path1, "sub1")
  pvec1 <- ParquetNeuroVec(parquet_path1)
  
  # Setup for vec 2
  dims2 <- c(5, 5, 5, 8)
  neuro_vec2 <- neuroim2::NeuroVec(array(rnorm(prod(dims2)), dims2), neuroim2::NeuroSpace(dims2))
  parquet_path2 <- tempfile(fileext = ".fpar")
  neurovec_to_fpar(neuro_vec2, parquet_path2, "sub2")
  pvec2 <- ParquetNeuroVec(parquet_path2)
  
  # Concatenate
  pvec_cat <- concat(pvec1, pvec2)
  
  # Check dimensions
  expect_s4_class(pvec_cat, "ParquetNeuroVec")
  expect_equal(dim(pvec_cat)[4], dims1[4] + dims2[4])
  
  # Check data
  expect_equivalent(pvec_cat[1,1,1,1:5], pvec1[1,1,1,], tolerance = 1e-6)
  expect_equivalent(pvec_cat[1,1,1,6:13], pvec2[1,1,1,], tolerance = 1e-6)
  
  # Cleanup
  unlink(parquet_path1)
  unlink(parquet_path2)
}) 