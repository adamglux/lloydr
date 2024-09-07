#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix manhattan_distances(NumericMatrix data) {
  // taxicab distances
  // see euclidean_distance for more set up details

  int n = data.nrow();
  NumericMatrix dist_matrix(n, n);

  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      double sum = 0.0;
      for (int k = 0; k < data.ncol(); ++k) {
        sum += fabs(data(i, k) - data(j, k));
        // fabs(data(i, k) - data(j, k)) to calculate the absolute difference
      }
      dist_matrix(i, j) = sum;
      dist_matrix(j, i) = dist_matrix(i, j);
    }
  }

  // Manhattan distance: sum of the absolute differences b/w two points’ features

  return dist_matrix;
}
