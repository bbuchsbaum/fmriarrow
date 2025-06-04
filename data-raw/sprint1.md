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
        **Function Signature:** `read_fpar_metadata(parquet_path) -> list`
        
        **Purpose:** Extract and parse spatial metadata from Parquet file schema metadata.
        
        **Detailed Implementation:**
        *   **Input Validation:**
            *   Check if `parquet_path` exists and is readable
            *   Verify file has `.fpar` or `.parquet` extension
            *   Handle both relative and absolute paths
        *   **Core Logic:**
            *   Use `arrow::read_parquet(parquet_path, col_select = character(0))` to read only schema
            *   Extract schema metadata via `schema(parquet_table)$metadata`
            *   Parse JSON-encoded metadata fields back to R objects
            *   Convert string representations back to appropriate types (numeric vectors, matrices)
        *   **Return Structure:** Named list with elements:
            *   `metadata_schema_version`: character
            *   `source_info`: list with `original_file`, `neuroim2_space_hash`
            *   `spatial_properties`: list with `original_dimensions`, `voxel_size_mm`, `affine_matrix`, `reference_space`, `coordinate_convention`
            *   `acquisition_properties`: list with `repetition_time_s`, `timepoint_count`
            *   `data_integrity`: list with `voxel_count`, `bold_value_range`
        *   **Error Handling:**
            *   Missing metadata fields → warning + return partial list with NA for missing elements
            *   Corrupted JSON → stop with descriptive error
            *   Schema version mismatch → warning about potential compatibility issues
        *   **Testing Requirements:**
            *   Test with valid `.fpar` file created by `neurovec_to_fpar()`
            *   Test with missing metadata
            *   Test with corrupted metadata
            *   Verify affine matrix reconstruction matches original `NeuroSpace`
    4.  **`[CORE-010]` Implement Basic Z-Index Range Query Function:**
        **Function Signature:** `read_fpar_zindex_range(parquet_path, min_zindex, max_zindex, columns = NULL) -> arrow::Table`
        
        **Purpose:** Efficiently query BOLD data for voxels within a specific Z-index range using Parquet predicate pushdown.
        
        **Detailed Implementation:**
        *   **Input Validation:**
            *   Check `parquet_path` exists and is valid `.fpar` file
            *   Validate `min_zindex <= max_zindex`
            *   Ensure both Z-index values are non-negative integers < 2^32
            *   Validate `columns` is NULL or character vector of valid column names
        *   **Core Logic:**
            *   Open Parquet file as Arrow dataset: `arrow::open_dataset(parquet_path)`
            *   Apply Z-index filter: `dplyr::filter(zindex >= min_zindex & zindex <= max_zindex)`
            *   Select specified columns or all if `columns = NULL`
            *   Return as Arrow table for memory efficiency
        *   **Optimization Features:**
            *   Leverage Parquet row group statistics for efficient row group pruning
            *   Use lazy evaluation - don't collect() unless explicitly requested
            *   Support column projection to avoid reading unnecessary data
        *   **Return Value:** Arrow table with filtered rows, sorted by `zindex`
        *   **Error Handling:**
            *   Invalid Z-index range → stop with helpful message
            *   Empty result set → return empty Arrow table with correct schema
            *   Invalid column names → stop with list of available columns
        *   **Testing Requirements:**
            *   Test with various Z-index ranges (small, large, single voxel)
            *   Verify predicate pushdown is working (check query plan)
            *   Test column selection functionality
            *   Compare results with manual filtering for accuracy
            *   Test edge cases: min=max, range outside data bounds
    5.  **`[CORE-011]` Helper for Coordinate-to-Z-Index Range Conversion:**
        **Function Signature:** `coords_to_zindex_range(x_range, y_range, z_range, max_coord_bits = 10) -> list(min_zindex, max_zindex)`
        
        **Purpose:** Convert 3D coordinate ranges to Z-index ranges for efficient Parquet querying.
        
        **Detailed Implementation:**
        *   **Input Validation:**
            *   Validate ranges: `x_range = c(min_x, max_x)` where min_x <= max_x (same for y, z)
            *   Ensure all coordinates are non-negative integers
            *   Check coordinates fit within `max_coord_bits` (default 10 → max coord 1023)
            *   Handle both inclusive ranges `c(1, 5)` and single values `5` (converted to `c(5, 5)`)
        *   **Core Algorithm:**
            *   Convert coordinate ranges to 0-based indexing
            *   Compute Z-index for all 8 corner points of the bounding box:
                ```r
                corners <- expand.grid(
                  x = c(x_range[1], x_range[2]),
                  y = c(y_range[1], y_range[2]), 
                  z = c(z_range[1], z_range[2])
                )
                zindices <- apply(corners, 1, function(p) compute_zindex(p[1], p[2], p[3]))
                ```
            *   Return `list(min_zindex = min(zindices), max_zindex = max(zindices))`
        *   **Optimization Notes:**
            *   This gives a conservative bounding range - may include voxels outside the exact cuboid
            *   More precise filtering can be done post-query on `x`, `y`, `z` columns
            *   Document this "overestimation" behavior clearly
        *   **Testing Requirements:**
            *   Test with single voxel (point query)
            *   Test with small cuboid (2x2x2)
            *   Test with large region
            *   Verify Z-index range actually contains all voxels in coordinate range
            *   Test edge cases at coordinate space boundaries
    6.  **`[CORE-012]` Implement Query by Voxel Coordinates (Cuboid ROI):**
        **Function Signature:** `read_fpar_coords_roi(parquet_path, x_range, y_range, z_range, exact = TRUE, columns = NULL) -> arrow::Table`
        
        **Purpose:** Query BOLD data for a cuboid ROI defined by coordinate ranges, with options for exact vs approximate filtering.
        
        **Detailed Implementation:**
        *   **Input Validation:**
            *   Validate `parquet_path` exists and is readable `.fpar` file
            *   Check coordinate ranges are valid (min <= max, non-negative)
            *   Validate `exact` is logical
            *   Validate `columns` is NULL or character vector
        *   **Two-Stage Filtering Approach:**
            *   **Stage 1 - Z-index filter (fast):**
                ```r
                zrange <- coords_to_zindex_range(x_range, y_range, z_range)
                data <- read_fpar_zindex_range(parquet_path, zrange$min_zindex, zrange$max_zindex, columns)
                ```
            *   **Stage 2 - Exact coordinate filter (if `exact = TRUE`):**
                ```r
                if (exact) {
                  data <- data |>
                    dplyr::filter(
                      x >= x_range[1] & x <= x_range[2] &
                      y >= y_range[1] & y <= y_range[2] &
                      z >= z_range[1] & z <= z_range[2]
                    )
                }
                ```
        *   **Performance Options:**
            *   `exact = FALSE`: Only Z-index filtering (faster, may include extra voxels)
            *   `exact = TRUE`: Additional coordinate filtering (precise ROI, slightly slower)
        *   **Return Value:** Arrow table with BOLD data for specified ROI, sorted by `zindex`
        *   **Error Handling:**
            *   Invalid coordinate ranges → stop with helpful message about 0-based indexing
            *   ROI completely outside data bounds → return empty table with warning
            *   Memory exhaustion for very large ROIs → provide estimate and suggest chunking
        *   **Memory Management:**
            *   For large ROIs, consider returning lazy Arrow table rather than collected data
            *   Provide memory usage estimates in verbose mode
            *   Support streaming/chunked processing for massive ROIs
        *   **Testing Requirements:**
            *   Test small ROI (single voxel, 2x2x2 cube)
            *   Test large ROI (significant portion of brain)
            *   Compare `exact = TRUE` vs `exact = FALSE` results
            *   Verify coordinate system handling (0-based storage vs user expectations)
            *   Test ROIs at data boundaries
            *   Performance testing: time vs traditional `neuroim2` subsetting
            *   Test with different column selections
        *   **Usage Examples:**
            ```r
            # Single voxel
            voxel_data <- read_fpar_coords_roi("scan.fpar", c(45, 45), c(54, 54), c(36, 36))
            
            # Small ROI around seed coordinate
            roi_data <- read_fpar_coords_roi("scan.fpar", c(40, 50), c(50, 60), c(30, 40))
            
            # Fast approximate query for very large ROI
            approx_data <- read_fpar_coords_roi("scan.fpar", c(0, 90), c(0, 108), c(0, 90), exact = FALSE)
            
            # Get only BOLD time series, no coordinates
            bold_only <- read_fpar_coords_roi("scan.fpar", c(40, 50), c(50, 60), c(30, 40), columns = "bold")
            ```
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
    5.  **`[CORE-017]` Code Review & Refactoring:**
        **Purpose:** Ensure code quality, maintainability, and consistency across all implemented functions.
        
        **Detailed Review Checklist:**
        *   **Code Style & Standards:**
            *   Follow `tidyverse` style guide for function/variable naming
            *   Consistent indentation and spacing
            *   Use roxygen2 documentation for all exported functions
            *   Follow R package conventions for file organization
        *   **Function Design Review:**
            *   **Single Responsibility:** Each function should have one clear purpose
            *   **Interface Consistency:** Similar functions should have similar parameter patterns
            *   **Error Messages:** Clear, actionable error messages with suggested fixes
            *   **Return Values:** Consistent return types and structures
        *   **Performance & Memory Review:**
            *   Identify potential memory bottlenecks in `neurovec_to_fpar()`
            *   Review Arrow table handling - avoid unnecessary copies
            *   Check for efficient use of lazy evaluation in query functions
            *   Profile memory usage with medium-sized test datasets
        *   **Code Duplication & Refactoring Opportunities:**
            *   Extract common validation logic into helper functions:
                ```r
                validate_parquet_path <- function(path) { ... }
                validate_coordinate_range <- function(range, name) { ... }
                validate_zindex_range <- function(min_z, max_z) { ... }
                ```
            *   Consolidate metadata handling functions
            *   Create consistent error handling patterns
        *   **Documentation Review:**
            *   All exported functions have complete roxygen2 documentation
            *   Examples are runnable and demonstrate key features
            *   Parameter descriptions are clear about data types and ranges
            *   Document coordinate system conventions (0-based vs 1-based)
        *   **Testing Coverage Review:**
            *   Ensure all public functions have unit tests
            *   Test edge cases and error conditions
            *   Integration tests cover full workflow: NeuroVec → Parquet → Query
            *   Performance regression tests
        *   **Dependencies Review:**
            *   Minimize external dependencies where possible
            *   Ensure all `importFrom()` statements in NAMESPACE are necessary
            *   Check for any licensing conflicts
        *   **Refactoring Actions:**
            *   Create utility functions for common operations
            *   Standardize parameter validation patterns
            *   Extract metadata handling into separate module
            *   Implement consistent logging/verbose output patterns
        *   **Final Integration Test:**
            *   End-to-end workflow test with real neuroimaging data
            *   Cross-platform compatibility check (if applicable)
            *   Memory stress test with large datasets
            *   Performance benchmark vs traditional approaches
        **Deliverables:**
        *   Refactored code with improved organization
        *   Updated documentation reflecting any API changes
        *   Performance and memory profiling report
        *   Identified technical debt and future improvement opportunities
    6.  **`[CORE-018]` (Optional if time) Initial Hilbert Curve Implementation:** (No change)
*   **Definition of Done (Sprint 3):**
    *   Core functions for converting a single `neuroim2::NeuroVec` to Parquet and querying that Parquet file are robust and well-tested.
    *   Basic documentation enables a user to perform these core tasks.
    *   The core MVP for single-scan processing, leveraging `neuroim2` as input, is stable.

---

This updated plan directly integrates `neuroim2` as the starting point. The main work involves extracting the necessary data and metadata from `neuroim2` objects, performing the Z-order transformation, and then structuring everything for efficient Parquet storage and subsequent Arrow-based querying. The add-ons would then build upon these Parquet files.