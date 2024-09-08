#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix scale_numeric_data(NumericMatrix data) {
  // see euclidean_distance for more set up details

  int n_cols = data.ncol();
  int n_rows = data.nrow();
  //gets dimensions

  NumericMatrix scaled_data(n_rows, n_cols);

  // loop through each column (numeric variable)
  for (int j = 0; j < n_cols; ++j) {
    double col_min = Rcpp::min(data(_, j)); // Get minimum value in the column
    double col_max = Rcpp::max(data(_, j)); // Get maximum value in the column

    // scale each element in the column
    for (int i = 0; i < n_rows; ++i) {
      scaled_data(i, j) = (data(i, j) - col_min) / (col_max - col_min);
      //normalises each value
    }
  }

  return scaled_data;
}
