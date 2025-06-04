


Using `neuroim2`'s `NeuroVec` (and its ecosystem like `NeuroSpace`) as the primary S4 interface for the *input* data makes a lot of sense. It leverages an existing, well-structured library for handling the complexities of neuroimaging data (like NIfTI I/O, spatial transformations, different in-memory representations) and allows us to focus the new development on the Parquet-based storage and fast Z-indexed access.


see neuroim2_cheatsheet.md for more details on neuroim2.



Here's how we can revise the 3 sprints to explicitly reflect this:

**Core Goal:** To create a set of R functions that can take a `neuroim2::NeuroVec` object (representing a single fMRI scan/run), along with necessary scan identifiers, and convert it into our optimized Parquet format. Then, implement functions to query this Parquet format for spatially-local time series.

**Assumptions for Core MVP:**
*   The input `neuroim2::NeuroVec` object is already loaded into R (e.g., via `neuroim2::read_vec()`).
*   The `NeuroSpace` associated with the input `NeuroVec` provides accurate dimensional, spacing, origin, and affine transformation information in a common reference space (e.g., MNI).
*   Scan identifiers (`subject_id`, `session_id`, `task_id`, `run_id`) are passed as arguments to the conversion function.

---

**Updated Implementation Sprints for Core Functionality (MVP) using `neuroim2` Facade**

**Sprint 1: Foundational `NeuroVec`-to-Parquet Conversion & Z-Indexing**

*   **Goal:** Convert a single `neuroim2::NeuroVec` object into a Parquet file sorted by Z-order index, including scan-identifying columns and essential spatial metadata derived from its `NeuroSpace`.
*   **Tickets/Tasks:**
    1.  **`[CORE-001]` Project Setup & Dependencies:**
        *   Initialize R project (e.g., with `renv`).
        *   Add `neuroim2`, `arrow`, `dplyr` as dependencies.
    2.  **`[CORE-002]` Implement Z-Order Indexing Function:** (No change from previous plan)
        *   Create an R function `compute_zindex(x, y, z, max_coord_bits=10)` returning `uint32` Z-order code.
        *   Include unit tests.
    3.  **`[CORE-003]` Main Conversion Function Signature & Input Processing:**
        *   Define `neurovec_to_fpar(neuro_vec_obj, output_parquet_path, subject_id, session_id=NULL, task_id=NULL, run_id=NULL, ...)`.
        *   Validate `neuro_vec_obj` is a `neuroim2::NeuroVec` (or a compatible derivative like `DenseNeuroVec`).
        *   Access `NeuroSpace` via `neuroim2::space(neuro_vec_obj)`.
    4.  **`[CORE-004]` Voxel Iteration & Data Extraction from `NeuroVec`:**
        *   Get spatial dimensions (`D1`, `D2`, `D3`) from `dim(space(neuro_vec_obj))`.
        *   Iterate from voxel index 1 to `prod(D1,D2,D3)`.
        *   For each 1D voxel index, get its `x,y,z` grid coordinates using `neuroim2::index_to_grid(space(neuro_vec_obj), voxel_1d_index)`. These are 1-based; convert to 0-based for `compute_zindex`.
        *   Compute `zindex` using the 0-based `x,y,z`.
        *   Extract the full BOLD time series for the current `x,y,z` (1-based) from `neuro_vec_obj` (e.g., using `neuroim2::series(neuro_vec_obj, x, y, z)` or direct array access like `neuro_vec_obj[x,y,z,]` if it's a `DenseNeuroVec`).
        *   Store `subject_id`, `session_id`, `task_id`, `run_id`, 0-based `x,y,z`, `zindex`, and the `bold` series.
    5.  **`[CORE-005]` Create Arrow Table & Sort by `zindex`:**
        *   Collect the data from `CORE-004` (e.g., into a list of lists or a data.frame).
        *   Convert to an `arrow::Table`.
        *   Define Arrow schema:
            *   `subject_id: string` (or dictionary-encoded), `session_id: string` (nullable), `task_id: string` (nullable), `run_id: string` or `uint8` (nullable).
            *   `x,y,z: uint16` (0-based).
            *   `zindex: uint32`.
            *   `bold: fixed_size_list<float32>[T]`. `T` is `dim(neuro_vec_obj)[4]`.
        *   Sort the Arrow Table by the `zindex` column.
    6.  **`[CORE-006]` Write Sorted Table to Parquet:**
        *   Use `arrow::write_parquet()` to save the sorted table to `output_parquet_path`.
        *   Specify `row_group_size` (e.g., 4096), `compression` (e.g., "zstd"), `write_statistics=TRUE`.
*   **Definition of Done (Sprint 1):**
    *   A function `neurovec_to_fpar()` exists.
    *   It takes a `neuroim2::NeuroVec` object and scan identifiers, producing a Parquet file.
    *   The Parquet file contains the specified columns, including scan IDs and 0-based coordinates.
    *   The Parquet file is sorted by `zindex`.
    *   Schema and sorting can be externally verified.

**Sprint 2: `NeuroSpace`-derived Metadata & Basic Parquet Spatial Querying**

*   **Goal:** Embed essential spatial metadata (derived from the input `NeuroVec`'s `NeuroSpace`) into the Parquet file's metadata, and implement basic ROI retrieval functions that operate *on the generated Parquet file*.
*   **Tickets/Tasks:**
    1.  **`[CORE-007]` Extract Core Metadata from `NeuroSpace`:**
        *   Inside `neurovec_to_fpar()`:
            *   Let `s <- space(neuro_vec_obj)`.
            *   Extract `original_dimensions = dim(s)`.
            *   Extract `voxel_size_mm = spacing(s)[1:min(3, length(spacing(s)))]`.
            *   Extract `affine_matrix = trans(s)`.
            *   Extract `reference_space` (this might need to be an explicit parameter or a convention, as `NeuroSpace` doesn't formally store this string).
            *   Potentially `origin = origin(s)[1:min(3, length(origin(s)))]`.
        *   Populate the Parquet file-level metadata structure (as defined in the proposal).
    2.  **`[CORE-008]` Embed Metadata into Parquet File:**
        *   Modify the Parquet writing step (`CORE-006`) to include the extracted metadata as Arrow Table schema metadata.
        *   Consider limitations of Parquet schema metadata (often string-based key-value) vs. a sidecar JSON (more flexible for complex structures like matrices). For MVP, Arrow schema metadata is fine.
    3.  **`[CORE-009]` Implement Parquet Metadata Retrieval Function:**
        *   Create `read_fpar_metadata(parquet_path)` that reads a Parquet file's schema metadata and returns it (e.g., as an R list).
    4.  **`[CORE-010]` Implement Basic Z-Index Range Query Function (on Parquet file):** (No change from previous plan – this function queries the Parquet file directly)
        *   `read_fpar_zindex_range(parquet_path, min_z, max_z)`.
    5.  **`[CORE-011]` Helper for Coordinate-to-Z-Index Range (for Parquet querying):** (No change)
    6.  **`[CORE-012]` Implement Query by Voxel Coordinates (Cuboid ROI, on Parquet file):** (No change – this function queries the Parquet file)
        *   `read_fpar_coords_roi(parquet_path, x_coords_0based, y_coords_0based, z_coords_0based)`. Note: coordinates are 0-based to match Parquet storage.
*   **Definition of Done (Sprint 2):**
    *   Core spatial metadata derived from `NeuroSpace` is stored in and retrievable from the Parquet file.
    *   Can query and retrieve BOLD data for a specified cuboid ROI (defined by 0-based coordinates) from a *single* Parquet file.
    *   Basic validation of retrieved data shape and content.

**Sprint 3: Refinements, Testing, and Documentation for Single-Scan Core**

*   **Goal:** Solidify the `NeuroVec`-to-Parquet conversion and the Parquet querying functionality for single scans, ensuring robustness and usability.
*   **Tickets/Tasks:**
    1.  **`[CORE-013]` Add Robust Error Handling & Input Validation:**
        *   To `neurovec_to_fpar()`: check `neuro_vec_obj` type, validity of scan IDs.
        *   To query functions: check `parquet_path` existence, valid query parameters.
    2.  **`[CORE-014]` Comprehensive Unit & Integration Testing:**
        *   `compute_zindex` (already started).
        *   `neurovec_to_fpar()`:
            *   Create a sample `neuroim2::DenseNeuroVec`.
            *   Convert it.
            *   Verify output Parquet schema, sorting, number of rows.
            *   Verify metadata content against original `NeuroSpace`.
            *   Read back a few sample voxel time series from Parquet and compare with `neuroim2::series()` on the original `NeuroVec`.
        *   Metadata storage and retrieval tests.
        *   ROI querying functions: test with known ROIs, verify correct voxels and their BOLD series are returned.
    3.  **`[CORE-015]` Performance Sanity Checks:**
        *   Conversion time for a moderately sized `NeuroVec`.
        *   Parquet ROI query time vs. `neuroim2::series()` or direct subsetting on the original `NeuroVec` for a comparable ROI.
    4.  **`[CORE-016]` Write Basic Usage Documentation (README/Vignette):**
        *   How to use `neurovec_to_fpar()`.
        *   How to use `read_fpar_coords_roi()` and `read_fpar_metadata()`.
        *   Explanation of the 0-based coordinate system in the Parquet file vs. `neuroim2`'s 1-based system.
        *   Clarify that current querying functions operate on one Parquet file at a time.
    5.  **`[CORE-017]` Code Review & Refactoring:** (No change)
    6.  **`[CORE-018]` (Optional if time) Initial Hilbert Curve Implementation:** (No change)
*   **Definition of Done (Sprint 3):**
    *   Core functions for converting a single `neuroim2::NeuroVec` to Parquet and querying that Parquet file are robust and well-tested.
    *   Basic documentation enables a user to perform these core tasks.
    *   The core MVP for single-scan processing, leveraging `neuroim2` as input, is stable.

---

This updated plan directly integrates `neuroim2` as the starting point. The main work involves extracting the necessary data and metadata from `neuroim2` objects, performing the Z-order transformation, and then structuring everything for efficient Parquet storage and subsequent Arrow-based querying. The add-ons would then build upon these Parquet files.