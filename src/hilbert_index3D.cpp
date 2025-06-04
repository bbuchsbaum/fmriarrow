#include <Rcpp.h>
#include <cstdlib>
#include <cerrno>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// Helper: rotate and flip a quadrant appropriately
// Rotate and flip a quadrant according to canonical 3D Hilbert rules
inline void rot(int n, int &x, int &y, int &z, int rx, int ry, int rz) {
  if (rz == 0) {
    if (ry == 0) {
      if (rx == 1) {
        x = n - 1 - x;
        y = n - 1 - y;
      }
      std::swap(x, y);
    } else {
      if (rx == 0) {
        x = n - 1 - x;
        y = n - 1 - y;
      }
    }
    std::swap(y, z);
  } else {
    if (ry == 1) {
      if (rx == 1) {
        x = n - 1 - x;
        y = n - 1 - y;
      }
      std::swap(x, y);
    } else {
      if (rx == 0) {
        x = n - 1 - x;
        y = n - 1 - y;
      }
    }
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

//' Compute 3D Hilbert curve indices
//' 
//' @param x,y,z Integer vectors of coordinates
//' @param nbits Number of bits per dimension (1-21)
//' @param as_character Return indices as character strings to preserve precision
//' @return Numeric or character vector of Hilbert indices
//' @examples
//' compute_hindex_cpp(IntegerVector::create(0,1,2),
//'                    IntegerVector::create(0,1,2),
//'                    IntegerVector::create(0,1,2), 4)
// [[Rcpp::export]]
SEXP compute_hindex_cpp(IntegerVector x, IntegerVector y, IntegerVector z,
                        int nbits, bool as_character = false) {
  int n = x.size();
  if (y.size() != n || z.size() != n) {
    stop("x, y, and z must have the same length");
  }
  if (nbits <= 0 || nbits > 21) {
    stop("nbits must be between 1 and 21");
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

  if (as_character) {
    CharacterVector out(n);
#ifdef _OPENMP
    if (n > 10000) {
#pragma omp parallel for
      for (int i = 0; i < n; ++i) {
        uint64_t idx = hilbert3D(x[i], y[i], z[i], nbits);
        out[i] = std::to_string(idx);
      }
    } else
#endif
    {
      for (int i = 0; i < n; ++i) {
        uint64_t idx = hilbert3D(x[i], y[i], z[i], nbits);
        out[i] = std::to_string(idx);
      }
    }
    return out;
  } else {
    NumericVector out(n);
#ifdef _OPENMP
    if (n > 10000) {
#pragma omp parallel for
      for (int i = 0; i < n; ++i) {
        out[i] = static_cast<double>(hilbert3D(x[i], y[i], z[i], nbits));
      }
    } else
#endif
    {
      for (int i = 0; i < n; ++i) {
        out[i] = static_cast<double>(hilbert3D(x[i], y[i], z[i], nbits));
      }
    }
    return out;
  }
}

//' Decode Hilbert curve indices back to coordinates
//'
//' @param index Character vector of Hilbert indices
//' @param nbits Number of bits per dimension (1-21)
//' @return A data.frame with columns x, y and z
// [[Rcpp::export]]
DataFrame compute_hindex_cpp_inverse(CharacterVector index, int nbits) {
  int n = index.size();
  IntegerVector x(n), y(n), z(n);
  for (int i = 0; i < n; ++i) {
    std::string s = Rcpp::as<std::string>(index[i]);
    char* endptr;
    errno = 0;
    uint64_t idx = std::strtoull(s.c_str(), &endptr, 10);
    if (errno != 0 || *endptr != '\0') {
      stop("Invalid index string at position " + std::to_string(i));
    }
    int xi, yi, zi;
    hilbert3D_inverse(idx, nbits, xi, yi, zi);
    x[i] = xi;
    y[i] = yi;
    z[i] = zi;
  }
  return DataFrame::create(Named("x") = x,
                           Named("y") = y,
                           Named("z") = z);
}

//' Convenience wrapper for a single 3D point
//'
//' @param x,y,z Integer coordinates
//' @param nbits Number of bits per dimension (1-21)
//' @param as_character Return index as character string
//'
//' @return A length-one vector containing the Hilbert index
// [[Rcpp::export]]
SEXP hilbert3D_single(int x, int y, int z, int nbits,
                      bool as_character = false) {
  IntegerVector xv(1, x), yv(1, y), zv(1, z);
  return compute_hindex_cpp(xv, yv, zv, nbits, as_character);
}

