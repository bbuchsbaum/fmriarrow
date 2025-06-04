# Proposal: An Optimized fMRI Data Format using Apache Arrow/Parquet for Fast, Spatially-Aware Time Series Access in R

**Version:** 2.0  
**Date:** December 2024  
**Lead:** AI Assistant (in collaboration with User and Reviewers)

## Executive Summary

This proposal outlines a file format and associated data access patterns for 4D fMRI datasets, optimized for extremely low-latency read access of spatially nearby time series. The design leverages Apache Parquet's columnar storage with Z-order curve indexing to ensure spatial locality translates to data contiguity on disk, enabling rapid querying of specific 3D regions without materializing entire files in RAM.

**Key Innovation:** Integration with the `neuroim2` R package for seamless conversion from standard neuroimaging formats, combined with Arrow/Parquet backend for performance-optimized spatial queries.

## Core Architecture

### Input: neuroim2 Integration

The system uses `neuroim2::NeuroVec` objects as the primary input interface:
- Leverages existing, well-structured library for NIfTI I/O and spatial transformations
- Provides robust `NeuroSpace` metadata for spatial reference information
- Allows focus on Parquet optimization rather than reimplementing neuroimaging I/O

### Storage: Parquet with Spatial Indexing

**Primary Table Schema:**
```
Arrow Table (one file per fMRI scan)
├── subject_id   : string (dictionary-encoded)
├── session_id   : string (nullable, for multi-session studies)  
├── task_id      : string (nullable, e.g., "rest", "memory")
├── run_id       : string (nullable, e.g., "01", "02")
├── x            : uint16 (0-based voxel coordinates)
├── y            : uint16
├── z            : uint16  
├── zindex       : uint32 (Z-order/Morton index, PRIMARY SORT KEY)
└── bold         : fixed_size_list<float32>[T] (complete time series)
```

**Spatial Indexing Strategy:**
- **Z-order (Morton) Indexing:** Interleaves bits of (x,y,z) coordinates to generate `zindex`
- Ensures voxels near in 3D space are near in 1D storage order
- Enables efficient spatial range queries via Parquet's predicate pushdown
- Row groups sized to contain spatially contiguous blocks (~4K-16K voxels)

**File Organization (Multi-Run Support):**
```
cohort_data/
├── sub-01/
│   ├── sub-01_ses-pre_task-rest_run-01_bold.fpar
│   ├── sub-01_ses-pre_task-memory_run-01_bold.fpar
│   └── ...
└── sub-02/
    └── ...
```

### Essential Metadata

Stored as Parquet schema metadata, derived from `neuroim2::NeuroSpace`:

```json
{
  "metadata_schema_version": "2.0.0",
  "source_info": {
    "original_file": "sub-01_task-rest_bold.nii.gz",
    "neuroim2_space_hash": "sha256:abc123..."
  },
  "spatial_properties": {
    "original_dimensions": [91, 109, 91, 150],
    "voxel_size_mm": [2.0, 2.0, 2.0],
    "affine_matrix": [[-2.0, 0.0, 0.0, 90.0], ...],
    "reference_space": "MNI152NLin2009cAsym",
    "coordinate_convention": "0-based RAS"
  },
  "acquisition_properties": {
    "repetition_time_s": 0.72,
    "timepoint_count": 150
  },
  "data_integrity": {
    "voxel_count": 500000,
    "bold_value_range": [-3500.5, 7800.1]
  }
}
```

## Core Implementation (MVP)

### Sprint 1: NeuroVec-to-Parquet Conversion
**Primary Function:** `neurovec_to_fpar(neuro_vec_obj, output_path, subject_id, ...)`

1. **Input Processing:**
   - Validate `neuroim2::NeuroVec` object
   - Extract spatial metadata from `neuroim2::space(neuro_vec_obj)`
   - Get dimensions, spacing, affine transformation

2. **Spatial Indexing:**
   - Iterate through all voxels using `neuroim2::index_to_grid()`
   - Convert 1-based coordinates to 0-based for Z-index computation
   - Compute `zindex = compute_zindex(x, y, z)`

3. **Data Extraction:**
   - Extract BOLD time series via `neuroim2::series()` or array access
   - Collect scan identifiers (subject_id, task_id, run_id, session_id)

4. **Parquet Writing:**
   - Create Arrow table with specified schema
   - Sort by `zindex` for spatial locality
   - Write with optimized parameters (compression=zstd, statistics=true)

### Sprint 2: Metadata Integration & Spatial Querying
**Core Functions:** 
- `read_fpar_metadata(parquet_path)` - Extract spatial metadata
- `read_fpar_coords_roi(parquet_path, x_range, y_range, z_range)` - Spatial queries

### Sprint 3: Testing & Documentation
- Comprehensive validation against original `NeuroVec` data
- Performance benchmarking vs traditional approaches
- User documentation for 0-based vs 1-based coordinate systems

## Advanced Features (Post-MVP)

### 1. Annotation Layers
Separate, linkable Parquet files for derived data:
- **ROI Segmentations:** `atlas_aal.parquet` with `(zindex, roi_label)`
- **Statistical Maps:** `glm_task_contrast.parquet` with `(zindex, beta, t_stat, p_value)`
- **Voxel Properties:** Custom analyses linked via `zindex`

**Usage:** `dplyr::left_join(bold_data, roi_data, by = "zindex")`

### 2. Pre-computed Analysis Acceleration
Store GLM coefficients, ICA weights, or embeddings as additional columns:
```
├── glm_task_coeff_cond1  : float32
├── glm_task_residual_var : float32  
└── ica_component_weights : fixed_size_list<float32>[N_components]
```

### 3. Multiscale Hierarchical Views
Coarser resolution layers for fast overview queries:
- Block-level summaries (e.g., 4mm³ blocks from 2mm³ voxels)
- Hierarchical `block_zindex` computed by bit-shifting voxel `zindex`
- Enable coarse-to-fine exploration workflows

### 4. Cohort-Level Operations
- Partitioned datasets by `subject_id` using `arrow::open_dataset()`
- Cross-subject queries: "DMN voxels for all resting-state subjects"
- Efficient filtering: `task_id == "rest" & zindex %in% dmn_indices`

## Key Benefits

1. **Performance:** Sub-second access to spatially contiguous regions
2. **Memory Efficiency:** Query without loading entire 4D volumes
3. **R Ecosystem Integration:** Native `tidyverse`, `arrow`, `duckdb` compatibility  
4. **Neuroimaging Integration:** Seamless `neuroim2` input/output workflows
5. **Extensibility:** Modular annotation layers and pre-computed features
6. **Reproducibility:** Standardized format with comprehensive metadata
7. **Scalability:** Handles single subjects to large cohorts

## Implementation Strategy

**Phase 1 (Sprints 1-3):** Core single-scan conversion and querying using `neuroim2`
**Phase 2:** Multi-subject cohort support and partitioning
**Phase 3:** Annotation layers and analysis acceleration features  
**Phase 4:** Advanced querying APIs and cloud-native features

This design establishes a robust foundation for modern fMRI data analysis workflows, bridging the gap between traditional neuroimaging tools and contemporary data science infrastructure.