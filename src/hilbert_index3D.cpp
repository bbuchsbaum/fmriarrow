#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <numeric>

// --- Bitwise Helper Functions ---

// Computes the binary reflected Gray code of an integer.
inline uint64_t gray_code(uint64_t i) {
    return i ^ (i >> 1);
}

// Computes the inverse of the binary reflected Gray code.
inline uint64_t gray_code_inverse(uint64_t g) {
    uint64_t i = g;
    uint64_t j = 1;
    while ((1ULL << j) <= i) {
        i ^= (g >> j);
        j++;
    }
    return i;
}

// Counts the number of trailing set bits (1s).
inline int trailing_set_bits(uint64_t i) {
    if (i == 0) return 0;
    int count = 0;
    while ((i & 1) == 1) {
        count++;
        i >>= 1;
    }
    return count;
}

// Right-rotates a value.
inline uint64_t right_rotate(uint64_t val, int n, int num_bits) {
    n = n % num_bits;
    if (n == 0) return val;
    uint64_t mask = (1ULL << n) - 1;
    uint64_t right_part = val & mask;
    uint64_t left_part = val >> n;
    return left_part | (right_part << (num_bits - n));
}

// --- Hamilton's Hilbert State Helpers (for n=3) ---
const int N_DIMS = 3;

// Pre-computed lookup tables for e_i and d_i
const std::vector<uint64_t> e_i_lookup = {
    gray_code(2 * floor(0/2)), gray_code(2 * floor(1/2)),
    gray_code(2 * floor(2/2)), gray_code(2 * floor(3/2)),
    gray_code(2 * floor(4/2)), gray_code(2 * floor(5/2)),
    gray_code(2 * floor(6/2)), gray_code(2 * floor(7/2))
};

const std::vector<int> d_i_lookup = []{
    std::vector<int> d(8);
    std::vector<int> g(8);
    for(int i=0; i<8; ++i) g[i] = trailing_set_bits(i);
    d[0] = 0;
    for (int i = 1; i < 8; ++i) {
        if (i % 2 == 1) d[i] = g[i];
        else d[i] = g[i-1];
    }
    return d;
}();


// --- Main Hilbert Index Calculation ---

// [[Rcpp::export]]
Rcpp::NumericVector compute_hindex_cpp(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::IntegerVector z, int max_coord_bits) {
    R_xlen_t n = x.size();
    Rcpp::NumericVector h_indices(n);

    for (R_xlen_t k = 0; k < n; ++k) {
        uint64_t cur_x = x[k];
        uint64_t cur_y = y[k];
        uint64_t cur_z = z[k];
        
        uint64_t h = 0;
        uint64_t e = 0;
        int d = 0;

        for (int i = max_coord_bits - 1; i >= 0; --i) {
            uint64_t b_x = (cur_x >> i) & 1;
            uint64_t b_y = (cur_y >> i) & 1;
            uint64_t b_z = (cur_z >> i) & 1;
            uint64_t l = (b_x << 2) | (b_y << 1) | b_z;

            uint64_t l_prime = right_rotate(l ^ e, d, N_DIMS);
            uint64_t w = gray_code_inverse(l_prime);

            h = (h << N_DIMS) | w;
            
            e = e ^ right_rotate(e_i_lookup[w], d, N_DIMS);
            d = (d + d_i_lookup[w] + 1) % N_DIMS;
        }
        h_indices[k] = static_cast<double>(h);
    }
    return h_indices;
}


// --- Inverse Hilbert Index Calculation ---

// [[Rcpp::export]]
Rcpp::DataFrame compute_hindex_cpp_inverse(Rcpp::NumericVector h_indices, int max_coord_bits) {
    R_xlen_t n = h_indices.size();
    Rcpp::IntegerVector x_coords(n);
    Rcpp::IntegerVector y_coords(n);
    Rcpp::IntegerVector z_coords(n);

    for (R_xlen_t k = 0; k < n; ++k) {
        uint64_t h = static_cast<uint64_t>(h_indices[k]);
        
        uint64_t x = 0, y = 0, z = 0;
        uint64_t e = 0;
        int d = 0;

        for (int i = max_coord_bits - 1; i >= 0; --i) {
            uint64_t shift = i * N_DIMS;
            uint64_t w = (h >> shift) & 7;

            uint64_t l_prime = gray_code(w);
            uint64_t l = e ^ right_rotate(l_prime, N_DIMS - d, N_DIMS);

            x |= ((l >> 2) & 1) << i;
            y |= ((l >> 1) & 1) << i;
            z |= ((l >> 0) & 1) << i;

            e = e ^ right_rotate(e_i_lookup[w], d, N_DIMS);
            d = (d + d_i_lookup[w] + 1) % N_DIMS;
        }
        x_coords[k] = x;
        y_coords[k] = y;
        z_coords[k] = z;
    }

    return Rcpp::DataFrame::create(
        Rcpp::Named("x") = x_coords,
        Rcpp::Named("y") = y_coords,
        Rcpp::Named("z") = z_coords
    );
}

