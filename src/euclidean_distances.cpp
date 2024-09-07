#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix euclidean_distances(NumericMatrix data) {
  int n = data.nrow();
  NumericMatrix dist_matrix(n, n);

  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      double sum = 0.0;
      for (int k = 0; k < data.ncol(); ++k) {
        sum += pow(data(i, k) - data(j, k), 2);
      }
      dist_matrix(i, j) = sqrt(sum);
      dist_matrix(j, i) = dist_matrix(i, j);  // Symmetry
    }
  }

  return dist_matrix;
}
