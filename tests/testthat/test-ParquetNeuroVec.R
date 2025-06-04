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
  
  # Test error for invalid matrix dimensions
  invalid_matrix <- matrix(c(1, 1), nrow = 1, ncol = 2)
  expect_error(series(pvec, invalid_matrix), "3 columns")
  
  # Cleanup
  unlink(parquet_path)
})

test_that("ParquetNeuroVec series_roi methods", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create test data
  dims <- c(6, 6, 4, 5)
  data <- array(runif(prod(dims)), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_roi.fpar")
  
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec,
    output_parquet_path = parquet_path,
    subject_id = "test_subject"
  )
  
  pvec <- ParquetNeuroVec(parquet_path)
  
  # Test ROI series extraction with coordinate ranges
  roi_ts <- series_roi(pvec, 1L:3L, 1L:3L, 1L:2L)
  expect_type(roi_ts, "double")
  expect_length(roi_ts, dims[4])
  
  # Test ROI series with matrix coordinates
  coord_matrix <- matrix(c(1, 1, 1,
                          1, 2, 1,
                          2, 1, 1,
                          2, 2, 1), 
                        nrow = 4, ncol = 3, byrow = TRUE)
  
  roi_ts_matrix <- series_roi(pvec, coord_matrix)
  expect_type(roi_ts_matrix, "double")
  expect_length(roi_ts_matrix, dims[4])
  
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
  
  # Test basic subsetting
  subset_data <- pvec[1:2, 1:2, 1, 1:2]
  expect_true(is.array(subset_data))
  expect_equal(dim(subset_data), c(2, 2, 1, 2))
  
  # Test single voxel extraction
  single_voxel <- pvec[1, 1, 1, ]
  expect_length(single_voxel, dims[4])
  
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
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create small test dataset
  dims <- c(3, 3, 2, 3)
  data <- array(rnorm(prod(dims)), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_lazy.fpar")
  
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec,
    output_parquet_path = parquet_path,
    subject_id = "test_subject"
  )
  
  # Test lazy loading (default)
  pvec_lazy <- ParquetNeuroVec(parquet_path, lazy = TRUE)
  expect_true(pvec_lazy@lazy)
  
  # Test non-lazy loading
  pvec_eager <- ParquetNeuroVec(parquet_path, lazy = FALSE)
  expect_false(pvec_eager@lazy)
  
  # Both should provide the same interface
  expect_equal(dim(pvec_lazy), dim(pvec_eager))
  expect_equal(length(pvec_lazy), length(pvec_eager))
  
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