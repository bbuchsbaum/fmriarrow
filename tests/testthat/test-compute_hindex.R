library(testthat)
devtools::load_all()

context("compute_hindex")

test_that("basic hilbert index values are correct", {
  nbits <- 2
  expect_equal(compute_hindex(0, 0, 0, nbits), 0)
  expect_equal(compute_hindex(0, 0, 1, nbits), 1)
  expect_equal(compute_hindex(0, 1, 1, nbits), 2)
  expect_equal(compute_hindex(0, 1, 0, nbits), 3)
  expect_equal(compute_hindex(1, 1, 0, nbits), 4)
  expect_equal(compute_hindex(1, 1, 1, nbits), 5)
  expect_equal(compute_hindex(1, 0, 1, nbits), 6)
  expect_equal(compute_hindex(1, 0, 0, nbits), 7)
})

test_that("vectorized input works for both R and C++", {
  nbits <- 2
  x <- c(0, 1, 0, 1)
  y <- c(0, 0, 1, 1)
  z <- c(0, 0, 0, 0)
  expected <- c(0, 7, 3, 4)
  expect_equal(compute_hindex(x, y, z, nbits), expected)
  # Test the internal R implementation for consistency
  expect_equal(compute_hindex_r(x, y, z, nbits), expected)
})

test_that("round trip works for R implementation", {
  nbits <- 6
  x <- 1:4
  y <- c(0, 1, 2, 3)
  z <- c(3, 2, 1, 0)
  h <- compute_hindex_r(x, y, z, nbits)
  coords <- compute_hindex_inverse_r(h, nbits)
  expect_equal(coords$x, x)
  expect_equal(coords$y, y)
  expect_equal(coords$z, z)
})

test_that("round trip works for C++ implementation", {
  nbits <- 6
  x <- 1:4
  y <- c(0, 1, 2, 3)
  z <- c(3, 2, 1, 0)
  h_cpp <- compute_hindex(x, y, z, nbits)
  coords_cpp <- compute_hindex_inverse(h_cpp, nbits)
  expect_equal(coords_cpp$x, x)
  expect_equal(coords_cpp$y, y)
  expect_equal(coords_cpp$z, z)
})

test_that("adjacent indices are neighbors", {
  nbits <- 4
  coords <- expand.grid(x=0:(2^nbits-1), y=0:(2^nbits-1), z=0:(2^nbits-1))
  
  h <- compute_hindex(coords$x, coords$y, coords$z, nbits)
  o <- order(h)
  co <- coords[o,]
  dists <- rowSums(abs(co[-1,] - co[-nrow(co),]))
  expect_true(all(dists == 1))
})

test_that("round trip works for larger nbits values", {
  nbits <- 12
  set.seed(123)
  coords <- data.frame(x=sample(0:(2^nbits-1), 100), 
                       y=sample(0:(2^nbits-1), 100), 
                       z=sample(0:(2^nbits-1), 100))
  
  # Test C++ roundtrip (the main function)
  h <- compute_hindex(coords$x, coords$y, coords$z, nbits)
  coords2 <- compute_hindex_inverse(h, nbits)
  expect_equal(coords, coords2)
  
  # Test R roundtrip for consistency
  h_r <- compute_hindex_r(coords$x, coords$y, coords$z, nbits)
  coords2_r <- compute_hindex_inverse_r(h_r, nbits)
  expect_equal(coords, coords2_r)
})

test_that("input validation works", {
  expect_error(compute_hindex(0, 0, 1024, nbits = 10))
  expect_error(compute_hindex(-1, 0, 0, nbits = 10))
})

test_that("character mode round trip", {
  idx_chr <- compute_hindex(1:4, c(0, 1, 2, 3), c(3, 2, 1, 0),
                           max_coord_bits = 6, as_character = TRUE)
  decoded <- compute_hindex_cpp_inverse(idx_chr, nbits = 6)
  expect_equal(decoded$x, 1:4)
  expect_equal(decoded$y, c(0, 1, 2, 3))
  expect_equal(decoded$z, c(3, 2, 1, 0))
})

test_that("single point wrapper works", {
  val <- hilbert3D_single(1, 2, 3, 4)
  direct <- compute_hindex_cpp(1L, 2L, 3L, 4L)
  expect_equal(val, direct)
})

test_that("hilbert3D and its inverse are mutual for full ranges", {
  nbits_values <- 1:5
  for (nbits in nbits_values) {
    n <- 2^nbits
    coords <- expand.grid(x = 0:(n - 1),
                          y = 0:(n - 1),
                          z = 0:(n - 1))
    idx <- compute_hindex_cpp(coords$x, coords$y, coords$z,
                              nbits, as_character = TRUE)
    decoded <- compute_hindex_cpp_inverse(idx, nbits)
    expect_equal(decoded$x, coords$x)
    expect_equal(decoded$y, coords$y)
    expect_equal(decoded$z, coords$z)
  }
})



