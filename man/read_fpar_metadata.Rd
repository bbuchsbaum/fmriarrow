% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fpar_metadata.R
\name{read_fpar_metadata}
\alias{read_fpar_metadata}
\title{Read fmriarrow metadata from a Parquet file}
\usage{
read_fpar_metadata(parquet_path)
}
\arguments{
\item{parquet_path}{Path to the Parquet file.}
}
\value{
A list with metadata fields matching the Sprint 2 specification:
  \describe{
    \item{metadata_schema_version}{Schema version string}
    \item{source_info}{List with original_file and neuroim2_space_hash}
    \item{spatial_properties}{List with dimensions, voxel_size_mm, affine_matrix, etc.}
    \item{acquisition_properties}{List with repetition_time_s and timepoint_count}
    \item{data_integrity}{List with voxel_count and bold_value_range}
  }
}
\description{
Extracts and parses the comprehensive spatial metadata stored by 
`neurovec_to_fpar()` in the Parquet schema metadata.
}
