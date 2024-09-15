#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix euclidean_point_distances(NumericMatrix data) {
  // dataset is passed into the function as a matrix of numbers
  // each row represents a data point, and each column is a feature

  int n = data.nrow(); //gets the number of rows (points) in the dataset
  NumericMatrix dist_matrix(n, n);
  // NumericMatrix is the Rcpp class for handling numeric matrices
  // create an empty matrix for dist values, with n rows and n columns

  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
    // inner loop starts from i + 1,
    // so only the upper triangle gets calculated (distances are symmetric)

      double sum = 0.0; //accumulates squared differences between two points

      for (int k = 0; k < data.ncol(); ++k) {
        // each feature (or dimension) of the points

        sum += pow(data(i, k) - data(j, k), 2); // pow(base, exponent)
        // squared difference between the i-th and j-th points for each feature

      }
      dist_matrix(i, j) = sqrt(sum);
      dist_matrix(j, i) = dist_matrix(i, j);  // symmetric matrices
    }
  }

  return dist_matrix;
}
