#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix gower_distances(NumericMatrix data) {
  // see euclidean_distance for more set up details

  int n = data.nrow();
  NumericMatrix dist_matrix(n, n);

  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      double sum = 0.0;
      for (int k = 0; k < data.ncol(); ++k) {
        sum += fabs(data(i, k) - data(j, k)) / (max(data(_, k)) - min(data(_, k)));
        // fabs() returns absolute value of the difference between points
        // max(data(_, k)) and min(data(_, k)) calculates the range of values for each feature
        // used to normalize the distance
      }
      dist_matrix(i, j) = sum / data.ncol();
      dist_matrix(j, i) = dist_matrix(i, j);
    }
  }

  return dist_matrix;
}
