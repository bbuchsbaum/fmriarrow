test_that("as_parquet_neurovec works correctly", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create test data
  dims <- c(5, 5, 3, 4)
  data <- array(rnorm(prod(dims)), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  # Create temporary file
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_convenience.fpar")
  
  # Test as_parquet_neurovec in one call
  expect_no_error(
    pvec <- as_parquet_neurovec(
      neuro_vec,
      parquet_path,
      subject_id = "test_subject",
      task_id = "rest",
      repetition_time = 2.0
    )
  )
  
  # Verify it created both file and object
  expect_true(file.exists(parquet_path))
  expect_s4_class(pvec, "ParquetNeuroVec")
  expect_s4_class(pvec, "NeuroVec")
  
  # Test object properties
  expect_equal(dim(pvec), dims)
  expect_equal(length(pvec), dims[4])
  expect_true(pvec@lazy)  # default lazy = TRUE
  
  # Test metadata was preserved
  metadata <- pvec@metadata
  expect_equal(metadata$acquisition_properties$repetition_time_s, 2.0)
  expect_equal(metadata$spatial_properties$original_dimensions, dims)
  
  # Test non-lazy version
  pvec_eager <- as_parquet_neurovec(
    neuro_vec,
    file.path(temp_dir, "test_eager.fpar"),
    subject_id = "test_subject",
    lazy = FALSE
  )
  expect_false(pvec_eager@lazy)
  
  # Cleanup
  unlink(parquet_path)
  unlink(file.path(temp_dir, "test_eager.fpar"))
})

test_that("read_arrow works correctly", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create test data and .fpar file first
  dims <- c(4, 4, 2, 3)
  data <- array(runif(prod(dims)), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_read_arrow.fpar")
  
  # Create .fpar file using neurovec_to_fpar
  neurovec_to_fpar(
    neuro_vec_obj = neuro_vec,
    output_parquet_path = parquet_path,
    subject_id = "test_subject",
    session_id = "ses01"
  )
  
  # Test read_arrow function
  expect_no_error(pvec <- read_arrow(parquet_path))
  
  # Verify object properties
  expect_s4_class(pvec, "ParquetNeuroVec")
  expect_s4_class(pvec, "NeuroVec")
  expect_equal(dim(pvec), dims)
  expect_equal(length(pvec), dims[4])
  expect_true(pvec@lazy)  # default lazy = TRUE
  
  # Test lazy = FALSE option
  pvec_eager <- read_arrow(parquet_path, lazy = FALSE)
  expect_false(pvec_eager@lazy)
  expect_equal(dim(pvec_eager), dims)
  
  # Test error handling
  expect_error(read_arrow("nonexistent.fpar"), "File not found")
  expect_error(read_arrow(c("file1.fpar", "file2.fpar")), "single character string")
  
  # Cleanup
  unlink(parquet_path)
})

test_that("convenience functions maintain data fidelity", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create test data with known values
  dims <- c(3, 3, 2, 4)
  data <- array(1:prod(dims), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  temp_dir <- tempdir()
  parquet_path <- file.path(temp_dir, "test_fidelity.fpar")
  
  # Use as_parquet_neurovec for conversion
  pvec <- as_parquet_neurovec(
    neuro_vec,
    parquet_path,
    subject_id = "test_subject"
  )
  
  # Test round-trip fidelity via read_arrow
  pvec2 <- read_arrow(parquet_path)
  
  # Both objects should be identical
  expect_equal(dim(pvec), dim(pvec2))
  expect_equal(length(pvec), length(pvec2))
  
  # Test data fidelity at voxel level
  original_ts <- as.numeric(series(neuro_vec, 2, 2, 1))
  parquet_ts1 <- series(pvec, 2, 2, 1)
  parquet_ts2 <- series(pvec2, 2, 2, 1)
  
  # All should be identical
  expect_equal(original_ts, parquet_ts1)
  expect_equal(parquet_ts1, parquet_ts2)
  
  # Test correlation
  correlation1 <- cor(original_ts, parquet_ts1)
  correlation2 <- cor(original_ts, parquet_ts2)
  expect_equal(correlation1, 1.0, tolerance = 1e-10)
  expect_equal(correlation2, 1.0, tolerance = 1e-10)
  
  # Cleanup
  unlink(parquet_path)
})

test_that("workflow comparison: old vs new", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("arrow")
  
  # Create test data
  dims <- c(4, 4, 2, 3)
  data <- array(rnorm(prod(dims)), dim = dims)
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))
  neuro_vec <- neuroim2::NeuroVec(data, space)
  
  temp_dir <- tempdir()
  path1 <- file.path(temp_dir, "old_workflow.fpar")
  path2 <- file.path(temp_dir, "new_workflow.fpar")
  
  # Old workflow (2 steps)
  neurovec_to_fpar(neuro_vec, path1, "test_subject")
  pvec1 <- ParquetNeuroVec(path1)
  
  # New workflow (1 step)
  pvec2 <- as_parquet_neurovec(neuro_vec, path2, "test_subject")
  
  # Both should produce equivalent results
  expect_equal(dim(pvec1), dim(pvec2))
  expect_equal(length(pvec1), length(pvec2))
  
  # Test time series extraction
  ts1 <- series(pvec1, 2, 2, 1)
  ts2 <- series(pvec2, 2, 2, 1)
  expect_equal(ts1, ts2)
  
  # Test that read_arrow can read both files
  pvec1_read <- read_arrow(path1)
  pvec2_read <- read_arrow(path2)
  
  expect_equal(dim(pvec1_read), dim(pvec2_read))
  expect_equal(series(pvec1_read, 2, 2, 1), series(pvec2_read, 2, 2, 1))
  
  # Cleanup
  unlink(path1)
  unlink(path2)
}) 