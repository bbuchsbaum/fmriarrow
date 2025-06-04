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

test_that("oversized max_coord_bits fails", {
  expect_error(coords_to_zindex_range(0, 0, 0, max_coord_bits = 11))
})
