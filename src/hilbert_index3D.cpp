#include <Rcpp.h>
using namespace Rcpp;

// Helper: rotate and flip a quadrant appropriately
inline void rot(int int_n, int &x, int &y, int &z, int rx, int ry, int rz) {
  if (rz == 0) {
    if (ry == 0) {
      if (rx == 1) {
        x = int_n - 1 - x;
        y = int_n - 1 - y;
      }
      std::swap(x, y);
    }
    std::swap(y, z);
  }
}

// Convert (x,y,z) to Hilbert index
uint64_t hilbert3D(int x, int y, int z, int nbits) {
  uint64_t index = 0;
  int n = 1 << nbits;
  for (int i = nbits - 1; i >= 0; i--) {
    int rx = (x >> i) & 1;
    int ry = (y >> i) & 1;
    int rz = (z >> i) & 1;
    int digit = (rx << 2) | (ry << 1) | rz;
    index = (index << 3) | digit;
    rot(n, x, y, z, rx, ry, rz);
  }
  return index;
}

// Inverse Hilbert index to (x,y,z)
void hilbert3D_inverse(uint64_t index, int nbits, int &x, int &y, int &z) {
  x = y = z = 0;
  int n = 1 << nbits;
  for (int i = 0; i < nbits; i++) {
    int digit = (index >> (3 * (nbits - i - 1))) & 7;
    int rx = (digit >> 2) & 1;
    int ry = (digit >> 1) & 1;
    int rz = digit & 1;
    rot(n, x, y, z, rx, ry, rz);
    x |= rx << i;
    y |= ry << i;
    z |= rz << i;
  }
}

// [[Rcpp::export]]
NumericVector compute_hindex_cpp(IntegerVector x, IntegerVector y, IntegerVector z, int nbits) {
  int n = x.size();
  if (y.size() != n || z.size() != n) {
    stop("x, y, and z must have the same length");
  }
  if (nbits <= 0 || nbits > 20) {
    stop("nbits must be between 1 and 20 (for uint64_t output)");
  }
  int limit = 1 << nbits;
  for (int i = 0; i < n; ++i) {
    if (x[i] < 0 || y[i] < 0 || z[i] < 0) {
      stop("coordinates must be non-negative");
    }
    if (x[i] >= limit || y[i] >= limit || z[i] >= limit) {
      stop("coordinates exceed range defined by nbits");
    }
  }
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = static_cast<double>(hilbert3D(x[i], y[i], z[i], nbits));
  }
  return out;
}

// [[Rcpp::export]]
DataFrame compute_hindex_cpp_inverse(NumericVector index, int nbits) {
  int n = index.size();
  IntegerVector x(n), y(n), z(n);
  for (int i = 0; i < n; ++i) {
    int xi, yi, zi;
    hilbert3D_inverse(static_cast<uint64_t>(index[i]), nbits, xi, yi, zi);
    x[i] = xi;
    y[i] = yi;
    z[i] = zi;
  }
  return DataFrame::create(Named("x") = x,
                           Named("y") = y,
                           Named("z") = z);
}

