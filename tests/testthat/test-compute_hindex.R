library(testthat)

context("compute_hindex")

test_that("basic hilbert index values", {
  expect_equal(compute_hindex(0, 0, 0), 0)
  expect_equal(compute_hindex(1, 1, 1), 7)
  expect_equal(compute_hindex(2, 1, 0, max_coord_bits = 2), 33)
})

test_that("vectorized input works", {
  x <- c(0, 1, 0, 1)
  y <- c(0, 0, 1, 1)
  z <- c(0, 0, 0, 0)
  expect_equal(compute_hindex(x, y, z, max_coord_bits = 2), c(0, 1, 4, 5))
})

test_that("character mode round trip", {
  idx_chr <- compute_hindex(1:4, c(0,1,2,3), c(3,2,1,0),
                           max_coord_bits = 6, as_character = TRUE)
  decoded <- compute_hindex_cpp_inverse(idx_chr, nbits = 6)
  expect_equal(decoded$x, 1:4)
  expect_equal(decoded$y, c(0,1,2,3))
  expect_equal(decoded$z, c(3,2,1,0))
})

test_that("input validation", {
  expect_error(compute_hindex(-1, 0, 0))
  expect_error(compute_hindex(0, 0, 1024, max_coord_bits = 10))
  expect_error(compute_hindex(0, 0, 0, max_coord_bits = 21))
  expect_error(compute_hindex(0.5, 0, 0))
  expect_error(compute_hindex(0, 0, 0, max_coord_bits = 0))
  expect_error(compute_hindex(0, 0, 0, max_coord_bits = 31))

})

