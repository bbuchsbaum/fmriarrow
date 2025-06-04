#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector compute_zindex_cpp(IntegerVector x, IntegerVector y, IntegerVector z, int max_coord_bits = 10) {
  int n = x.size();
  if (y.size() != n || z.size() != n) {
    stop("x, y and z must have the same length");
  }
  int limit = 1 << max_coord_bits;
  for (int i = 0; i < n; ++i) {
    if (x[i] < 0 || y[i] < 0 || z[i] < 0) {
      stop("coordinates must be non-negative");
    }
    if (x[i] >= limit || y[i] >= limit || z[i] >= limit) {
      stop("coordinates exceed range defined by max_coord_bits");
    }
  }
  IntegerVector res(n);
  for (int i = 0; i < n; ++i) {
    unsigned int val = 0;
    for (int b = 0; b < max_coord_bits; ++b) {
      unsigned int mask = 1u << b;
      val |= (x[i] & mask) << (3 * b);
      val |= (y[i] & mask) << (3 * b + 1);
      val |= (z[i] & mask) << (3 * b + 2);
    }
    res[i] = (int)val;
  }
  return res;
}
