library(testthat)
devtools::load_all()

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

test_that("adjacent indices are neighbors", {
  for (nbits in 2:5) {
    n <- 2^nbits
    coords <- expand.grid(x = 0:(n - 1), y = 0:(n - 1), z = 0:(n - 1))
    idx <- compute_hindex_cpp(coords$x, coords$y, coords$z, nbits)

    expect_equal(length(unique(idx)), length(idx))

    sorted <- coords[order(idx), ]
    dists <- sapply(2:nrow(sorted), function(i) {
      sum(abs(sorted[i, ] - sorted[i - 1, ]))
    })
    expect_true(all(dists == 1))
  }
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

test_that("round trip works for larger nbits values", {
  nbits_values <- c(8, 12, 20)
  set.seed(123)
  for (nbits in nbits_values) {
    limit <- 2^nbits
    coords <- data.frame(
      x = sample.int(limit, 50) - 1L,
      y = sample.int(limit, 50) - 1L,
      z = sample.int(limit, 50) - 1L
    )
    idx <- compute_hindex_cpp(coords$x, coords$y, coords$z,
                              nbits, as_character = TRUE)
    decoded <- compute_hindex_cpp_inverse(idx, nbits)
    expect_equal(decoded$x, coords$x)
    expect_equal(decoded$y, coords$y)
    expect_equal(decoded$z, coords$z)
  }
})



