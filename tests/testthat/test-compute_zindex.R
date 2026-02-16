library(testthat)

context("compute_zindex")

test_that("basic zindex values", {
  expect_equal(compute_zindex(0, 0, 0), 0L)
  expect_equal(compute_zindex(1, 0, 0), 1L)
  expect_equal(compute_zindex(0, 1, 0), 2L)
  expect_equal(compute_zindex(0, 0, 1), 4L)
  expect_equal(compute_zindex(1, 1, 1), 7L)
  expect_equal(compute_zindex(2, 0, 0), 8L)
  expect_equal(compute_zindex(3, 0, 0), 9L)
  expect_equal(compute_zindex(2, 1, 0), 10L)
  expect_equal(compute_zindex(1, 2, 3), 53L)
})

test_that("vectorized input works", {
  x <- c(0, 1, 0, 1)
  y <- c(0, 0, 1, 1)
  z <- c(0, 0, 0, 0)
  expect_equal(compute_zindex(x, y, z), c(0L, 1L, 2L, 3L))
  expect_equal(compute_zindex_cpp(x, y, z), c(0L, 1L, 2L, 3L))
})

test_that("input validation", {
  expect_error(compute_zindex(-1, 0, 0))
  expect_error(compute_zindex(0, 0, 1024, max_coord_bits = 10))
  expect_error(compute_zindex(1024, 0, 0, max_coord_bits = 10))
  expect_error(compute_zindex(0.5, 0, 0))
  expect_error(compute_zindex(0, 0, 0, max_coord_bits = 0))
  expect_error(compute_zindex(0, 0, 0, max_coord_bits = 31))
})

test_that("invalid max_coord_bits triggers errors", {
  expect_error(compute_zindex(0, 0, 0, max_coord_bits = 0))
  expect_error(compute_zindex(0, 0, 0, max_coord_bits = 1.5))
  expect_error(compute_zindex(0, 0, 0, max_coord_bits = c(10, 11)))
})

test_that("max_coord_bits greater than 10 works", {
  val <- compute_zindex(1024, 0, 0, max_coord_bits = 11)
  expect_equal(as.double(val), as.double(bitwShiftL(1L, 30)))
})

test_that("R and C++ Z-index implementations agree for coords 0:10", {
  # Test across a grid of coordinates including values >= 2
  # where the original C++ bug manifested
  coords <- expand.grid(x = 0:10, y = 0:10, z = 0:10)

  r_result <- compute_zindex(coords$x, coords$y, coords$z)
  cpp_result <- compute_zindex_cpp(
    as.integer(coords$x),
    as.integer(coords$y),
    as.integer(coords$z)
  )

  expect_equal(r_result, cpp_result)
})

test_that("R and C++ Z-index agree at max coordinate (1023)", {
  r_result <- compute_zindex(1023, 1023, 1023, max_coord_bits = 10)
  cpp_result <- compute_zindex_cpp(1023L, 1023L, 1023L, max_coord_bits = 10L)

  expect_equal(r_result, cpp_result)
})

test_that("compute_zindex rejects mismatched coordinate lengths", {
  expect_error(compute_zindex(c(0, 1), c(0), c(0)), "same length")
})

test_that("compute_zindex_cpp rejects mismatched coordinate lengths", {
  expect_error(compute_zindex_cpp(c(0L, 1L), 0L, 0L), "same length")
})
