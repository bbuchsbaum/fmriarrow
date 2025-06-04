# Sprint 2: NeuroSpace-derived Metadata & Basic Parquet Spatial Querying

**Sprint Goal:** Embed essential spatial metadata (derived from the input `NeuroVec`'s `NeuroSpace`) into the Parquet file's metadata, and implement basic ROI retrieval functions that operate on the generated Parquet files from Sprint 1.

**Duration:** 1-2 weeks (estimate)

**Prerequisites from Sprint 1:**
- ✅ Working `neurovec_to_fpar()` function (CORE-001 through CORE-006)
- ✅ Functional Z-order indexing via `compute_zindex()`
- ✅ Basic Parquet file creation with sorted `zindex` column
- ✅ R package structure with dependencies configured

**Sprint 2 Focus:** Building the metadata and querying layer on top of the conversion foundation from Sprint 1.

---

## Sprint 2 Tasks

### **`[SPRINT2-001]` Extract Core Metadata from `NeuroSpace`**

**Purpose:** Enhance `neurovec_to_fpar()` to extract comprehensive spatial metadata from `neuroim2::NeuroSpace` objects.

**Implementation Details:**
*   **Integration Point:** Modify existing `neurovec_to_fpar()` function from Sprint 1
*   **Metadata Extraction Logic:**
    ```r
    # Inside neurovec_to_fpar() after validating neuro_vec_obj
    s <- space(neuro_vec_obj)
    
    metadata <- list(
      metadata_schema_version = "2.0.0",
      source_info = list(
        original_file = deparse(substitute(neuro_vec_obj)),  # or passed parameter
        neuroim2_space_hash = digest::digest(s, algo = "sha256")
      ),
      spatial_properties = list(
        original_dimensions = as.integer(dim(s)),
        voxel_size_mm = as.numeric(spacing(s)[1:min(3, length(spacing(s)))]),
        affine_matrix = as.matrix(trans(s)),
        reference_space = reference_space %||% "unknown",  # parameter or default
        coordinate_convention = "0-based RAS"
      ),
      acquisition_properties = list(
        repetition_time_s = repetition_time %||% NA_real_,  # parameter
        timepoint_count = as.integer(dim(neuro_vec_obj)[4])
      ),
      data_integrity = list(
        voxel_count = as.integer(prod(dim(s)[1:3])),
        bold_value_range = c(NA_real_, NA_real_)  # computed during conversion
      )
    )
    ```
*   **New Function Parameters:** Add `reference_space = NULL`, `repetition_time = NULL` to `neurovec_to_fpar()`
*   **Value Range Computation:** During voxel iteration, track min/max BOLD values for data integrity

**Acceptance Criteria:**
- `neurovec_to_fpar()` successfully extracts all metadata fields from `NeuroSpace`
- Metadata structure matches proposal specification
- Function handles missing/optional metadata gracefully

### **`[SPRINT2-002]` Embed Metadata into Parquet File**

**Purpose:** Store the extracted metadata as Parquet schema metadata for later retrieval.

**Implementation Details:**
*   **Arrow Schema Metadata:** Convert metadata list to JSON strings for schema storage
    ```r
    # Convert complex objects to JSON strings
    metadata_json <- lapply(metadata, function(x) {
      if (is.list(x) || is.matrix(x)) {
        jsonlite::toJSON(x, auto_unbox = TRUE)
      } else {
        as.character(x)
      }
    })
    
    # Create schema with metadata
    schema <- arrow::schema(
      subject_id = arrow::string(),
      session_id = arrow::string(),
      task_id = arrow::string(), 
      run_id = arrow::string(),
      x = arrow::uint16(),
      y = arrow::uint16(),
      z = arrow::uint16(),
      zindex = arrow::uint32(),
      bold = arrow::list_of(arrow::float32())
    )
    
    # Add metadata to schema
    schema <- schema$WithMetadata(metadata_json)
    ```
*   **Parquet Writing:** Modify `arrow::write_parquet()` call to use schema with metadata
*   **Fallback Option:** If schema metadata proves problematic, implement sidecar JSON file option

**Acceptance Criteria:**
- Metadata successfully embedded in Parquet file schema
- No impact on file reading performance  
- Metadata survives round-trip (write → read → extract)

### **`[SPRINT2-003]` Implement Parquet Metadata Retrieval Function**

**Purpose:** Create standalone function to extract and parse metadata from existing `.fpar` files.

**Function Signature:** `read_fpar_metadata(parquet_path) -> list`

**Implementation Details:**
*   **File Validation:**
    ```r
    read_fpar_metadata <- function(parquet_path) {
      # Input validation
      if (!file.exists(parquet_path)) {
        stop("Parquet file not found: ", parquet_path)
      }
      
      if (!grepl("\\.(fpar|parquet)$", parquet_path)) {
        warning("File extension should be .fpar or .parquet")
      }
    ```
*   **Metadata Extraction:**
    ```r
      # Read schema only (no data)
      table_schema <- arrow::read_parquet(parquet_path, col_select = character(0))$schema
      raw_metadata <- table_schema$metadata
      
      # Parse JSON fields back to R objects
      parsed_metadata <- lapply(raw_metadata, function(x) {
        tryCatch({
          parsed <- jsonlite::fromJSON(x)
          if (is.character(parsed) && length(parsed) == 1) parsed else parsed
        }, error = function(e) x)  # Return original if JSON parsing fails
      })
    ```
*   **Error Handling:** Graceful handling of missing fields, corrupted JSON, version mismatches
*   **Return Structure:** Consistent with proposal metadata schema

**Testing Requirements:**
- Test with files created by enhanced `neurovec_to_fpar()`  
- Test with missing metadata fields
- Test with corrupted/invalid JSON
- Verify numeric arrays and matrices reconstruct correctly

### **`[SPRINT2-004]` Implement Basic Z-Index Range Query Function**

**Purpose:** Create the foundational spatial query function using Z-index ranges and Parquet predicate pushdown.

**Function Signature:** `read_fpar_zindex_range(parquet_path, min_zindex, max_zindex, columns = NULL) -> arrow::Table`

**Implementation Details:**
*   **Lazy Arrow Dataset Approach:**
    ```r
    read_fpar_zindex_range <- function(parquet_path, min_zindex, max_zindex, columns = NULL) {
      # Input validation
      validate_parquet_path(parquet_path)
      validate_zindex_range(min_zindex, max_zindex)
      
      # Open as Arrow dataset for predicate pushdown
      dataset <- arrow::open_dataset(parquet_path)
      
      # Apply Z-index filter (leverages Parquet row group statistics)
      filtered_data <- dataset |>
        dplyr::filter(zindex >= min_zindex & zindex <= max_zindex)
      
      # Column selection if specified
      if (!is.null(columns)) {
        filtered_data <- filtered_data |> dplyr::select(all_of(columns))
      }
      
      # Return Arrow table (lazy - doesn't collect unless needed)
      return(filtered_data |> dplyr::compute())
    }
    ```
*   **Performance Features:**
    - Leverages Parquet row group min/max statistics for efficient skipping
    - Supports column projection to minimize I/O
    - Returns Arrow table for memory efficiency
*   **Validation Helpers:** Create reusable validation functions

**Testing Requirements:**
- Test with various Z-index ranges (point queries, large regions)
- Verify predicate pushdown effectiveness (profile query plans)
- Test column selection functionality
- Edge cases: empty results, invalid ranges

### **`[SPRINT2-005]` Helper for Coordinate-to-Z-Index Range Conversion**

**Purpose:** Bridge between user-friendly coordinate queries and efficient Z-index filtering.

**Function Signature:** `coords_to_zindex_range(x_range, y_range, z_range, max_coord_bits = 10) -> list(min_zindex, max_zindex)`

**Implementation Details:**
*   **Conservative Bounding Box Approach:**
    ```r
    coords_to_zindex_range <- function(x_range, y_range, z_range, max_coord_bits = 10) {
      # Validate and normalize ranges
      x_range <- normalize_coord_range(x_range, "x_range")
      y_range <- normalize_coord_range(y_range, "y_range") 
      z_range <- normalize_coord_range(z_range, "z_range")
      
      # Check coordinate bounds
      max_coord <- (2^max_coord_bits) - 1
      validate_coord_bounds(c(x_range, y_range, z_range), max_coord)
      
      # Compute Z-indices for all 8 corners of bounding box
      corners <- expand.grid(
        x = c(x_range[1], x_range[2]),
        y = c(y_range[1], y_range[2]),
        z = c(z_range[1], z_range[2])
      )
      
      zindices <- apply(corners, 1, function(corner) {
        compute_zindex(corner[1], corner[2], corner[3], max_coord_bits)
      })
      
      list(min_zindex = min(zindices), max_zindex = max(zindices))
    }
    ```
*   **Helper Functions:** `normalize_coord_range()`, `validate_coord_bounds()`
*   **Documentation:** Clear explanation of "overestimation" - may include extra voxels outside exact cuboid

### **`[SPRINT2-006]` Implement Query by Voxel Coordinates (Cuboid ROI)**

**Purpose:** User-facing function for spatial ROI queries with coordinate-based interface.

**Function Signature:** `read_fpar_coords_roi(parquet_path, x_range, y_range, z_range, exact = TRUE, columns = NULL) -> arrow::Table`

**Implementation Details:**
*   **Two-Stage Filtering Strategy:**
    ```r
    read_fpar_coords_roi <- function(parquet_path, x_range, y_range, z_range, 
                                   exact = TRUE, columns = NULL) {
      # Stage 1: Fast Z-index filtering (conservative bounding box)
      zrange <- coords_to_zindex_range(x_range, y_range, z_range)
      data <- read_fpar_zindex_range(parquet_path, zrange$min_zindex, zrange$max_zindex, columns)
      
      # Stage 2: Exact coordinate filtering (if requested)
      if (exact) {
        data <- data |>
          dplyr::filter(
            x >= x_range[1] & x <= x_range[2] &
            y >= y_range[1] & y <= y_range[2] &
            z >= z_range[1] & z <= z_range[2]
          )
      }
      
      return(data)
    }
    ```
*   **Performance Trade-offs:**
    - `exact = FALSE`: Faster, may include extra voxels (useful for large ROIs)
    - `exact = TRUE`: Precise, slightly slower due to additional filtering
*   **Memory Management:** Consider lazy evaluation for very large ROIs
*   **Usage Examples in Documentation:**
    ```r
    # Single voxel query
    voxel <- read_fpar_coords_roi("scan.fpar", c(45, 45), c(54, 54), c(36, 36))
    
    # ROI query with only BOLD time series
    roi_bold <- read_fpar_coords_roi("scan.fpar", c(40, 50), c(50, 60), c(30, 40), 
                                   columns = c("zindex", "bold"))
    ```

---

## Definition of Done (Sprint 2)

**Core Deliverables:**
1. **Enhanced `neurovec_to_fpar()`:** Embeds comprehensive `NeuroSpace`-derived metadata in Parquet files
2. **`read_fpar_metadata()`:** Reliable extraction of metadata from `.fpar` files  
3. **`read_fpar_zindex_range()`:** Efficient low-level spatial querying via Z-index ranges
4. **`coords_to_zindex_range()`:** Helper for coordinate-to-Z-index conversion
5. **`read_fpar_coords_roi()`:** User-friendly coordinate-based ROI querying

**Quality Gates:**
- All functions have comprehensive input validation and error handling
- Metadata round-trip testing: `NeuroSpace` → Parquet → extracted metadata consistency
- Spatial query accuracy: ROI results match expected coordinate ranges
- Performance: Queries demonstrate predicate pushdown effectiveness
- Documentation: Clear examples showing 0-based coordinate conventions

**Integration Readiness:**
- Functions work seamlessly with Parquet files created in Sprint 1
- Arrow table outputs are compatible with downstream `dplyr` operations
- Foundation established for Sprint 3 testing and Sprint 4 cohort extensions

This completes the core spatial querying infrastructure, ready for comprehensive testing and documentation in Sprint 3.