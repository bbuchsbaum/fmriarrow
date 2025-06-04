library(testthat)

context("coords_to_zindex_range")

test_that("single voxel range", {
  zr <- coords_to_zindex_range(1, 0, 0)
  expect_equal(zr$min_zindex, compute_zindex(1,0,0))
  expect_equal(zr$max_zindex, compute_zindex(1,0,0))
})

test_that("small cuboid", {
  zr <- coords_to_zindex_range(c(0,1), c(0,1), c(0,0))
  expect_equal(zr$min_zindex, 0L)
  expect_equal(zr$max_zindex, 3L)
})

test_that("new implementation matches expand.grid approach", {
  x_range <- c(1L, 3L)
  y_range <- c(2L, 4L)
  z_range <- c(0L, 1L)

  # Expected result using previous expand.grid logic
  corners <- expand.grid(
    x = c(x_range[1], x_range[2]),
    y = c(y_range[1], y_range[2]),
    z = c(z_range[1], z_range[2])
  )

  zindices <- compute_zindex(
    corners$x,
    corners$y,
    corners$z
  )

  expected <- list(min_zindex = min(zindices), max_zindex = max(zindices))

  result <- coords_to_zindex_range(x_range, y_range, z_range)

  expect_equal(result, expected)
})
