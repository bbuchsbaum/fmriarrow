#include <Rcpp.h>
#include <cstdint>
#include <utility>

// Helper: rotate and flip a quadrant appropriately
void rot(int n, int &x, int &y, int &z, int rx, int ry, int rz) {
    if (rz == 0) {
        if (ry == 0) {
            if (rx == 1) {
                x = n - 1 - x;
                y = n - 1 - y;
            }
            std::swap(x, y);
        } else {
             std::swap(y,z);
        }
    } else {
        if (ry == 1) {
             if (rx == 0) {
                int temp = y;
                y = x;
                x = n - 1 - temp;
             } else {
                x = n - 1 - x;
                y = n - 1 - y;
             }
        } else {
            int temp = z;
            z = x;
            x = temp;
        }
    }
}

// Convert (x,y,z) to Hilbert index
// [[Rcpp::export]]
Rcpp::NumericVector compute_hindex_cpp(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::IntegerVector z, int nbits) {
    int n_coords = x.size();
    Rcpp::NumericVector h_indices(n_coords);
    int N = 1 << nbits;

    for (int k = 0; k < n_coords; ++k) {
        int cur_x = x[k];
        int cur_y = y[k];
        int cur_z = z[k];
        uint64_t h = 0;

        for (int s = nbits - 1; s >= 0; --s) {
            int rx = (cur_x >> s) & 1;
            int ry = (cur_y >> s) & 1;
            int rz = (cur_z >> s) & 1;

            int digit = (rx << 2) | (ry << 1) | rz;
            h = (h << 3) | digit;
            
            rot(N, cur_x, cur_y, cur_z, rx, ry, rz);
        }
        h_indices[k] = static_cast<double>(h);
    }
    return h_indices;
}

// Convert Hilbert index back to (x,y,z)
// [[Rcpp::export]]
Rcpp::DataFrame compute_hindex_cpp_inverse(Rcpp::NumericVector h_indices, int nbits) {
    int n_coords = h_indices.size();
    Rcpp::IntegerVector x_coords(n_coords);
    Rcpp::IntegerVector y_coords(n_coords);
    Rcpp::IntegerVector z_coords(n_coords);
    int N = 1 << nbits;

    for (int k = 0; k < n_coords; ++k) {
        uint64_t h = static_cast<uint64_t>(h_indices[k]);
        int cur_x = 0;
        int cur_y = 0;
        int cur_z = 0;

        for (int s = nbits - 1; s >= 0; --s) {
            int rx = (h >> (3 * s + 2)) & 1;
            int ry = (h >> (3 * s + 1)) & 1;
            int rz = (h >> (3 * s + 0)) & 1;

            rot(N, cur_x, cur_y, cur_z, rx, ry, rz);

            cur_x |= (rx << s);
            cur_y |= (ry << s);
            cur_z |= (rz << s);
        }
        x_coords[k] = cur_x;
        y_coords[k] = cur_y;
        z_coords[k] = cur_z;
    }

    return Rcpp::DataFrame::create(
        Rcpp::Named("x") = x_coords,
        Rcpp::Named("y") = y_coords,
        Rcpp::Named("z") = z_coords
    );
}

