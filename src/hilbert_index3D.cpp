#include <Rcpp.h>
#include <cstdlib>
#include <cerrno>
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// Implementation of the 3D Hilbert curve based on:
// "Compact Hilbert Indices" by Chris Hamilton (2006)
// This provides a correct implementation that preserves the adjacency property

namespace {  // Anonymous namespace for internal helpers

constexpr int N_DIMS = 3;

// Gray code and its inverse
inline uint32_t gray_code(uint32_t i) {
    return i ^ (i >> 1);
}

inline uint32_t gray_code_inverse(uint32_t g) {
    uint32_t i = g;
    uint32_t j = 1;
    while ((1u << j) <= i) {
        i ^= (g >> j);
        j++;
    }
    return i;
}

// Count trailing set bits (trailing ones)
// This is g(i) in Hamilton's paper
inline uint32_t trailing_set_bits(uint32_t i) {
    if (i == 0) return 0;
    // Use compiler builtin if available
#if defined(__GNUC__) || defined(__clang__)
    // The number of trailing ones in 'i' equals the number of trailing zeros in 'i+1'
    return __builtin_ctz(i + 1);
#else
    // Fallback: count trailing ones
    uint32_t count = 0;
    while (i & 1) {
        count++;
        i >>= 1;
    }
    return count;
#endif
}

// Right rotate a 3-bit value
inline uint32_t right_rotate(uint32_t val, int n) {
    n %= N_DIMS;
    if (n == 0) return val;
    uint32_t mask = (1u << n) - 1;
    uint32_t right_part = val & mask;
    uint32_t left_part = val >> n;
    return left_part | (right_part << (N_DIMS - n));
}

// Hamilton's lookup tables for the 3D Hilbert curve
// e(i) = gc(2 * floor(i/2))
const uint32_t e_lookup[8] = {
    gray_code(0), gray_code(0), gray_code(2), gray_code(2),
    gray_code(4), gray_code(4), gray_code(6), gray_code(6)
};

// d(i) logic: d(0)=0; odd i: g(i); even i>0: g(i-1)
const uint32_t d_lookup[8] = {
    0,                          // d(0) = 0
    trailing_set_bits(1),       // d(1) = g(1) = 0
    trailing_set_bits(1),       // d(2) = g(1) = 0
    trailing_set_bits(3),       // d(3) = g(3) = 1
    trailing_set_bits(3),       // d(4) = g(3) = 1
    trailing_set_bits(5),       // d(5) = g(5) = 0
    trailing_set_bits(5),       // d(6) = g(5) = 0
    trailing_set_bits(7)        // d(7) = g(7) = 2
};

// Convert (x,y,z) to Hilbert index using Hamilton's algorithm
uint64_t point_to_hindex(uint32_t x, uint32_t y, uint32_t z, int nbits) {
    uint64_t h = 0;
    uint32_t e = 0;  // entry point state
    uint32_t d = 0;  // direction state

    for (int i = nbits - 1; i >= 0; --i) {
        // Extract the octant bits for this level
        uint32_t l = (((z >> i) & 1) << 2) |
                     (((y >> i) & 1) << 1) |
                     (((x >> i) & 1) << 0);

        // Transform the octant based on current state
        l = right_rotate(l ^ e, d + 1);
        
        // Convert to Gray code inverse
        uint32_t w = gray_code_inverse(l);
        
        // Append to Hilbert index
        h = (h << N_DIMS) | w;

        // Update state for next iteration
        e ^= right_rotate(e_lookup[w], d + 1);
        d = (d + d_lookup[w] + 1) % N_DIMS;
    }
    return h;
}

// Convert Hilbert index to (x,y,z) using Hamilton's algorithm
void hindex_to_point(uint64_t h, int nbits, uint32_t &x, uint32_t &y, uint32_t &z) {
    x = y = z = 0;
    uint32_t e = 0;
    uint32_t d = 0;

    for (int i = nbits - 1; i >= 0; --i) {
        // Extract 3-bit chunk from Hilbert index
        uint32_t w = (h >> (i * N_DIMS)) & 7;
        
        // Get transformed octant from Gray code
        uint32_t l_prime = gray_code(w);
        
        // Inverse transform to get original octant
        uint32_t l = right_rotate(l_prime, N_DIMS - (d + 1)) ^ e;

        // Extract coordinate bits
        x |= ((l >> 0) & 1) << i;
        y |= ((l >> 1) & 1) << i;
        z |= ((l >> 2) & 1) << i;

        // Update state (same as forward transform)
        e ^= right_rotate(e_lookup[w], d + 1);
        d = (d + d_lookup[w] + 1) % N_DIMS;
    }
}

}  // anonymous namespace

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
        uint64_t idx = point_to_hindex(x[i], y[i], z[i], nbits);
        out[i] = std::to_string(idx);
      }
    } else
#endif
    {
      for (int i = 0; i < n; ++i) {
        uint64_t idx = point_to_hindex(x[i], y[i], z[i], nbits);
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
        out[i] = static_cast<double>(point_to_hindex(x[i], y[i], z[i], nbits));
      }
    } else
#endif
    {
      for (int i = 0; i < n; ++i) {
        out[i] = static_cast<double>(point_to_hindex(x[i], y[i], z[i], nbits));
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
    uint32_t xi, yi, zi;
    hindex_to_point(idx, nbits, xi, yi, zi);
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