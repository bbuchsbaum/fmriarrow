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
})

test_that("input validation", {
  expect_error(compute_zindex(-1, 0, 0))
  expect_error(compute_zindex(0, 0, 1024, max_coord_bits = 10))
  expect_error(compute_zindex(0, 0, 0, max_coord_bits = 11))
  expect_error(compute_zindex(0.5, 0, 0))
  expect_error(compute_zindex(0, 0, 0, max_coord_bits = 0))
  expect_error(compute_zindex(0, 0, 0, max_coord_bits = 31))

})
