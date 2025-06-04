#' Benchmark z-index computation implementations
#'
#' Runs a simple microbenchmark comparing the pure R implementation
#' `compute_zindex` with the C++ helper `compute_zindex_cpp`.
#'
#' @param n Number of random coordinate triples to benchmark with.
#' @param max_coord_bits Maximum coordinate bits for random values.
#'
#' @return A `microbenchmark` object with timing results.
#' @export
benchmark_zindex <- function(n = 1e5, max_coord_bits = 10) {
  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    stop("microbenchmark package required for benchmarking")
  }
  limit <- bitwShiftL(1L, max_coord_bits) - 1L
  x <- sample.int(limit + 1L, n, replace = TRUE) - 1L
  y <- sample.int(limit + 1L, n, replace = TRUE) - 1L
  z <- sample.int(limit + 1L, n, replace = TRUE) - 1L
  microbenchmark::microbenchmark(
    R = compute_zindex(x, y, z, max_coord_bits),
    cpp = compute_zindex_cpp(x, y, z, max_coord_bits),
    times = 5L
  )
}
