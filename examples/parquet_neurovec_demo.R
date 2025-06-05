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

# 2. Compare Workflow: Old vs New
# --------------------------------
cat("\n=== Workflow Comparison ===\n")
temp_dir <- tempdir()
path_old <- file.path(temp_dir, "demo_old_workflow.fpar")
path_new <- file.path(temp_dir, "demo_new_workflow.fpar")

# OLD WORKFLOW (2 steps)
cat("Old workflow (2 steps):\n")
cat("  Step 1: neurovec_to_fpar() - Write to disk\n")
cat("  Step 2: ParquetNeuroVec() - Create object\n")
start_time <- Sys.time()
neurovec_to_fpar(
  neuro_vec_obj = neuro_vec,
  output_parquet_path = path_old,
  subject_id = "sub-01",
  task_id = "rest", 
  repetition_time = 2.0
)
pvec_old <- ParquetNeuroVec(path_old)
old_time <- as.numeric(Sys.time() - start_time)

# NEW WORKFLOW (1 step)
cat("\nNew workflow (1 step):\n")
cat("  as_parquet_neurovec() - Convert in one call\n")
start_time <- Sys.time()
pvec_new <- as_parquet_neurovec(
  neuro_vec_obj = neuro_vec,
  output_parquet_path = path_new,
  subject_id = "sub-01",
  task_id = "rest",
  repetition_time = 2.0
)
new_time <- as.numeric(Sys.time() - start_time)

cat("Time comparison - Old:", round(old_time, 3), "s, New:", round(new_time, 3), "s\n")

# 3. Demonstrate read_arrow (mirrors read_vec)
# ---------------------------------------------
cat("\n=== Reading Data: neuroim2 vs fmriarrow Pattern ===\n")
cat("neuroim2 pattern:\n")
cat("  neuro_vec <- read_vec('data.nii.gz')     # NIfTI → NeuroVec\n")
cat("fmriarrow pattern:\n")
cat("  pvec <- read_arrow('data.fpar')          # Parquet → ParquetNeuroVec\n")

# Demonstrate read_arrow
pvec_read <- read_arrow(path_new)
cat("\nread_arrow() successfully loaded data:\n")
cat("  Dimensions:", paste(dim(pvec_read), collapse = " x "), "\n")
cat("  File size: ", round(file.info(path_new)$size / 1024^2, 2), "MB\n")

# 4. Verify all three approaches give identical results
# -----------------------------------------------------
cat("\n=== Data Fidelity Verification ===\n")

# Test single voxel
test_coord <- c(32, 32, 15)
original_ts <- as.numeric(series(neuro_vec, test_coord[1], test_coord[2], test_coord[3]))
old_ts <- series(pvec_old, test_coord[1], test_coord[2], test_coord[3])
new_ts <- series(pvec_new, test_coord[1], test_coord[2], test_coord[3])
read_ts <- series(pvec_read, test_coord[1], test_coord[2], test_coord[3])

cat("Correlation with original NeuroVec:\n")
cat("  Old workflow: ", round(cor(original_ts, old_ts), 10), "\n")
cat("  New workflow: ", round(cor(original_ts, new_ts), 10), "\n")
cat("  read_arrow:   ", round(cor(original_ts, read_ts), 10), "\n")

# 5. Show ParquetNeuroVec capabilities
# ------------------------------------
show(pvec_new)

# 6. Demonstrate array-like access
# ---------------------------------
cat("\n=== Array-like Access Examples ===\n")

# Extract single voxel time series
cat("Extracting time series for voxel (32, 32, 15)...\n")
voxel_ts <- series(pvec_new, 32, 32, 15)
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

multi_ts <- series(pvec_new, coord_matrix)
cat("Result dimensions:", paste(dim(multi_ts), collapse = " x "), "\n")
cat("(", nrow(multi_ts), "timepoints x", ncol(multi_ts), "voxels )\n")

# Array subsetting - extract a small 4D block
cat("\nExtracting 4D subarray [30:32, 30:32, 14:16, 1:10]...\n")
sub_array <- pvec_new[30:32, 30:32, 14:16, 1:10]
cat("Subarray dimensions:", paste(dim(sub_array), collapse = " x "), "\n")

# ROI-based analysis
cat("\nComputing ROI time series for region [28:35, 28:35, 13:17]...\n")
roi_ts <- series_roi(pvec_new, 28:35, 28:35, 13:17)
cat("ROI time series length:", length(roi_ts), "\n")
cat("ROI mean signal: ", round(mean(roi_ts), 2), "\n")

# 7. API Summary
# --------------
cat("\n=== Complete fmriarrow API Summary ===\n")
cat("# Convert from NeuroVec to ParquetNeuroVec:\n")
cat("pvec <- as_parquet_neurovec(neuro_vec, 'data.fpar', 'sub-01')\n")
cat("\n# Read existing .fpar files:\n")
cat("pvec <- read_arrow('data.fpar')\n")
cat("\n# Array-like access:\n")
cat("ts <- series(pvec, x, y, z)                    # Single voxel\n")
cat("multi_ts <- series(pvec, coord_matrix)         # Multiple voxels\n")
cat("roi_ts <- series_roi(pvec, x_range, y_range, z_range)  # ROI average\n")
cat("subarray <- pvec[x_idx, y_idx, z_idx, t_idx]   # 4D subsetting\n")

# 8. Performance considerations
# -----------------------------
cat("\n=== Performance Notes ===\n")
cat("* as_parquet_neurovec() - One-step conversion for convenience\n")
cat("* read_arrow() - Mirrors neuroim2::read_vec() API pattern\n")
cat("* ParquetNeuroVec uses spatial indexing for efficient queries\n")
cat("* Z-order (Morton) indexing provides good spatial locality\n")
cat("* Lazy loading minimizes memory usage\n")
cat("* Column store format enables fast time series extraction\n")

# Cleanup
cat("\nCleaning up temporary files...\n")
unlink(path_old)
unlink(path_new)
unlink(paste0(tools::file_path_sans_ext(path_old), "_metadata.json"))
unlink(paste0(tools::file_path_sans_ext(path_new), "_metadata.json"))

cat("Demo completed successfully!\n") 