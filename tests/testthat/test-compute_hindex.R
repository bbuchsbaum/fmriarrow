library(testthat)
devtools::load_all()

context("compute_hindex")

test_that("basic hilbert index values", {
  expect_equal(compute_hindex(0, 0, 0, max_coord_bits=2), 0)
  expect_equal(compute_hindex(1, 0, 0, max_coord_bits=2), 1)
  expect_equal(compute_hindex(0, 1, 0, max_coord_bits=2), 2)
  expect_equal(compute_hindex(0, 0, 1, max_coord_bits=2), 4)
  expect_equal(compute_hindex(1, 1, 1, max_coord_bits=2), 7)
})

test_that("vectorized input works", {
  x <- c(0, 1, 0, 1)
  y <- c(0, 0, 1, 1)
  z <- c(0, 0, 0, 0)
  expect_equal(compute_hindex(x, y, z, max_coord_bits = 2), c(0, 1, 2, 3))
  expect_equal(compute_hindex_cpp(x, y, z, max_coord_bits = 2), c(0, 1, 2, 3))
})

test_that("round trip works", {
  x <- 1:4
  y <- c(0, 1, 2, 3)
  z <- c(3, 2, 1, 0)
  h <- compute_hindex(x, y, z, max_coord_bits = 6)
  coords <- compute_hindex_inverse(h, max_coord_bits = 6)
  expect_equal(coords$x, x)
  expect_equal(coords$y, y)
  expect_equal(coords$z, z)
})

test_that("cpp round trip works", {
  x <- 1:4
  y <- c(0, 1, 2, 3)
  z <- c(3, 2, 1, 0)
  h_cpp <- compute_hindex_cpp(x, y, z, max_coord_bits = 6)
  coords_cpp <- compute_hindex_cpp_inverse(h_cpp, max_coord_bits = 6)
  expect_equal(coords_cpp$x, x)
  expect_equal(coords_cpp$y, y)
  expect_equal(coords_cpp$z, z)
})

test_that("adjacent indices are neighbors", {
  #skip("Skipping adjacency tests until logic is fixed")
  max_val <- 2^4 -1 
  coords <- expand.grid(x=0:max_val, y=0:max_val, z=0:max_val)
  
  # Test R implementation
  h <- compute_hindex(coords$x, coords$y, coords$z, 4)
  o <- order(h)
  co <- coords[o,]
  dists <- sqrt(diff(co$x)^2 + diff(co$y)^2 + diff(co$z)^2)
  expect_true(all(dists == 1))
  
  # Test C++ implementation
  h_cpp <- compute_hindex_cpp(coords$x, coords$y, coords$z, 4)
  o_cpp <- order(h_cpp)
  co_cpp <- coords[o_cpp,]
  dists_cpp <- sqrt(diff(co_cpp$x)^2 + diff(co_cpp$y)^2 + diff(co_cpp$z)^2)
  expect_true(all(dists_cpp == 1))
})

test_that("round trip works for larger nbits values", {
  #skip("Skipping adjacency tests until logic is fixed")
  nbits <- 12
  coords <- data.frame(x=sample(0:(2^nbits-1), 100), 
                       y=sample(0:(2^nbits-1), 100), 
                       z=sample(0:(2^nbits-1), 100))
  
  # Test R roundtrip
  h <- compute_hindex(coords$x, coords$y, coords$z, nbits)
  coords2 <- compute_hindex_inverse(h, nbits)
  expect_equal(coords, coords2)
  
  # Test C++ roundtrip
  h_cpp <- compute_hindex_cpp(coords$x, coords$y, coords$z, nbits)
  coords2_cpp <- compute_hindex_cpp_inverse(h_cpp, nbits)
  expect_equal(coords, coords2_cpp)
})

test_that("character mode round trip", {
  idx_chr <- compute_hindex(1:4, c(0, 1, 2, 3), c(3, 2, 1, 0),
                           max_coord_bits = 6, as_character = TRUE)
  decoded <- compute_hindex_cpp_inverse(idx_chr, nbits = 6)
  expect_equal(decoded$x, 1:4)
  expect_equal(decoded$y, c(0, 1, 2, 3))
  expect_equal(decoded$z, c(3, 2, 1, 0))
})

test_that("input validation", {
  expect_error(compute_hindex(-1, 0, 0))
  expect_error(compute_hindex(0, 0, 1024, max_coord_bits = 10))
  expect_error(compute_hindex(0, 0, 0, max_coord_bits = 21))
  expect_error(compute_hindex(0.5, 0, 0))
  expect_error(compute_hindex(0, 0, 0, max_coord_bits = 0))
  expect_error(compute_hindex(0, 0, 0, max_coord_bits = 31))

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



