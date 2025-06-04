library(testthat)

context("neurovec_to_fpar")

test_that("input must be a NeuroVec", {
  expect_error(neurovec_to_fpar(list(), "file.parquet", "sub01"))
})

# This test exercises the basic return structure when neuroim2 is available.
test_that("basic invocation", {
  skip_if_not_installed("neuroim2")

  nv <- neuroim2::emptyNeuroVec(c(1,1,1,1))
  res <- neurovec_to_fpar(nv, tempfile(), "sub01")
  expect_true(inherits(res$space, "NeuroSpace"))
})
