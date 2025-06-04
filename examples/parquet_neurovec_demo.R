# ParquetNeuroVec Demo
# ==================
# 
# This script demonstrates the ParquetNeuroVec S4 class which provides 
# neuroim2::NeuroVec-compatible array-like access to fMRI data stored 
# in Parquet format.

library(fmriarrow)
library(neuroim2)

# 1. Create sample fMRI data
# --------------------------
cat("Creating sample 4D fMRI data...\n")
dims <- c(64, 64, 30, 100)  # 64x64x30 voxels, 100 timepoints
data <- array(rnorm(prod(dims), mean = 1000, sd = 200), dim = dims)
space <- neuroim2::NeuroSpace(
  dim = dims, 
  spacing = c(3, 3, 3),  # 3mm isotropic voxels
  origin = c(-96, -132, -78)  # MNI-like origin
)
neuro_vec <- neuroim2::NeuroVec(data, space)

cat("Original NeuroVec dimensions:", paste(dim(neuro_vec), collapse = " x "), "\n")

# 2. Convert to Parquet format
# -----------------------------
cat("\nConverting to Parquet format with spatial indexing...\n")
temp_dir <- tempdir()
parquet_path <- file.path(temp_dir, "demo_fmri.fpar")

neurovec_to_fpar(
  neuro_vec_obj = neuro_vec,
  output_parquet_path = parquet_path,
  subject_id = "sub-01",
  session_id = "ses-func",
  task_id = "rest",
  run_id = "01",
  reference_space = "MNI152NLin2009cAsym",
  repetition_time = 2.0
)

cat("Parquet file created:", parquet_path, "\n")
cat("File size:", round(file.info(parquet_path)$size / 1024^2, 2), "MB\n")

# 3. Create ParquetNeuroVec object
# --------------------------------
cat("\nCreating ParquetNeuroVec object...\n")
pvec <- ParquetNeuroVec(parquet_path)

# Display object information
show(pvec)

# 4. Demonstrate array-like access
# ---------------------------------
cat("\n=== Array-like Access Examples ===\n")

# Get dimensions (should match original)
cat("ParquetNeuroVec dimensions:", paste(dim(pvec), collapse = " x "), "\n")
cat("Number of timepoints:", length(pvec), "\n")

# Extract single voxel time series
cat("\nExtracting time series for voxel (32, 32, 15)...\n")
voxel_ts <- series(pvec, 32, 32, 15)
cat("Time series length:", length(voxel_ts), "\n")
cat("Value range: [", round(min(voxel_ts), 2), ", ", round(max(voxel_ts), 2), "]\n")

# Extract multiple voxel time series using matrix coordinates
cat("\nExtracting time series for 5 voxels using matrix coordinates...\n")
coord_matrix <- matrix(c(
  30, 30, 15,
  32, 32, 15, 
  34, 34, 15,
  30, 32, 16,
  32, 30, 16
), nrow = 5, ncol = 3, byrow = TRUE)

multi_ts <- series(pvec, coord_matrix)
cat("Result dimensions:", paste(dim(multi_ts), collapse = " x "), "\n")
cat("(", nrow(multi_ts), "timepoints x", ncol(multi_ts), "voxels )\n")

# Array subsetting - extract a small 4D block
cat("\nExtracting 4D subarray [30:32, 30:32, 14:16, 1:10]...\n")
sub_array <- pvec[30:32, 30:32, 14:16, 1:10]
cat("Subarray dimensions:", paste(dim(sub_array), collapse = " x "), "\n")

# ROI-based analysis
cat("\nComputing ROI time series for region [28:35, 28:35, 13:17]...\n")
roi_ts <- series_roi(pvec, 28:35, 28:35, 13:17)
cat("ROI time series length:", length(roi_ts), "\n")
cat("ROI mean signal: ", round(mean(roi_ts), 2), "\n")

# 5. Comparison with original NeuroVec
# -------------------------------------
cat("\n=== Verification Against Original ===\n")

# Compare a single voxel
original_ts <- as.numeric(series(neuro_vec, 32, 32, 15))
parquet_ts <- series(pvec, 32, 32, 15)
correlation <- cor(original_ts, parquet_ts)

cat("Voxel (32,32,15) correlation between original and Parquet:", round(correlation, 6), "\n")

# Compare array subsetting
original_sub <- neuro_vec[30:32, 30:32, 14:16, 1:10]
parquet_sub <- pvec[30:32, 30:32, 14:16, 1:10]
sub_correlation <- cor(as.vector(original_sub), as.vector(parquet_sub))

cat("Subarray correlation:", round(sub_correlation, 6), "\n")

# 6. Performance considerations
# -----------------------------
cat("\n=== Performance Notes ===\n")
cat("* ParquetNeuroVec uses spatial indexing for efficient queries\n")
cat("* Z-order (Morton) indexing provides good spatial locality\n")
cat("* Lazy loading minimizes memory usage\n")
cat("* Column store format enables fast time series extraction\n")

# Cleanup
cat("\nCleaning up temporary files...\n")
unlink(parquet_path)
unlink(paste0(tools::file_path_sans_ext(parquet_path), "_metadata.json"))

cat("Demo completed successfully!\n") 