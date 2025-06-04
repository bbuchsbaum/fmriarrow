# fmriarrow

This package provides helper functions for storing fMRI time series in
Apache Parquet files and querying them efficiently. The Parquet files
use 0-based voxel coordinates and a Morton (Z-order) index so that
spatially nearby voxels are stored contiguously on disk.

## Basic workflow

```r
library(neuroim2)
library(fmriarrow)

# create or load a NeuroVec object
nv <- neuroim2::emptyNeuroVec(c(2, 2, 1, 2))

# convert to Parquet
outfile <- tempfile(fileext = ".parquet")
neurovec_to_fpar(nv, outfile, subject_id = "subj01")

# read metadata back
md <- read_fpar_metadata(outfile)

# query a small ROI (0-based coordinates)
roi <- read_fpar_coords_roi(outfile, c(0, 1), c(0, 1), c(0, 0))
```

Coordinates stored in the Parquet file are **0-based**, whereas
`neuroim2` works with 1-based indices. When querying by coordinates you
must supply the 0-based values.

The query helpers operate on a single Parquet file at a time. For
cohort-level analyses you can use `arrow::open_dataset()` on a directory
of such files.
