#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix scale_data(NumericMatrix data) {

  int n_cols = data.ncol();
  int n_rows = data.nrow();

  // Create a matrix to store the scaled data
  NumericMatrix scaled_data(n_rows, n_cols);

  // Loop through each column to scale
  for (int j = 0; j < n_cols; ++j) {

    // Extract the column
    NumericVector col = data(_, j);

    // Get minimum and maximum values for scaling
    double col_min = Rcpp::min(col);
    double col_max = Rcpp::max(col);

    // Scale the column
    for (int i = 0; i < n_rows; ++i) {
      scaled_data(i, j) = (col[i] - col_min) / (col_max - col_min);
    }
  }

  // Return the scaled matrix
  return scaled_data;
}
