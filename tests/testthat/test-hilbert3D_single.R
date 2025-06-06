library(testthat)

devtools::load_all()

context("hilbert3D_single and round-trip consistency")

# Test 1: round-trip for full grids across multiple nbits

test_that("full grid round-trips through Hilbert index", {
  for (nbits in 1:4) {
    max_val <- 2^nbits - 1
    coords <- expand.grid(x = 0:max_val, y = 0:max_val, z = 0:max_val)
    idx <- compute_hindex_cpp(coords$x, coords$y, coords$z, nbits)
    rt <- compute_hindex_cpp_inverse(as.character(idx), nbits)
    expect_equal(rt$x, coords$x)
    expect_equal(rt$y, coords$y)
    expect_equal(rt$z, coords$z)
    idx_back <- compute_hindex_cpp(rt$x, rt$y, rt$z, nbits)
    expect_equal(idx_back, idx)
  }
})

# Test 2: hilbert3D_single matches vectorized computation

test_that("hilbert3D_single is consistent with compute_hindex_cpp", {
  set.seed(123)
  for (nbits in 1:5) {
    max_val <- 2^nbits - 1
    x <- sample(0:max_val, 50, replace = TRUE)
    y <- sample(0:max_val, 50, replace = TRUE)
    z <- sample(0:max_val, 50, replace = TRUE)
    for (as_char in c(TRUE, FALSE)) {
      h_single <- mapply(function(xi, yi, zi) {
        hilbert3D_single(xi, yi, zi, nbits, as_character = as_char)
      }, x, y, z)
      h_vector <- compute_hindex_cpp(x, y, z, nbits, as_character = as_char)
      expect_equal(h_single, h_vector)
    }
  }
})
