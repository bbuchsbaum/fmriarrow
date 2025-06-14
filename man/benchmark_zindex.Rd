% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark_zindex.R
\name{benchmark_zindex}
\alias{benchmark_zindex}
\title{Benchmark z-index computation implementations}
\usage{
benchmark_zindex(n = 1e+05, max_coord_bits = 10)
}
\arguments{
\item{n}{Number of random coordinate triples to benchmark with.}

\item{max_coord_bits}{Maximum coordinate bits for random values.}
}
\value{
A `microbenchmark` object with timing results.
}
\description{
Runs a simple microbenchmark comparing the pure R implementation
`compute_zindex` with the C++ helper `compute_zindex_cpp`.
}
