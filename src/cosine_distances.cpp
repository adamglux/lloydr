#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cosine_distances(NumericMatrix data) {
  // dataset is passed into the function as a matrix of numbers
  // each row represents a data point, and each column is a feature


  int n = data.nrow(); //gets the number of rows (points) in the dataset
  NumericMatrix dist_matrix(n, n);

  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      double dot_product = 0.0, norm_i = 0.0, norm_j = 0.0;
      // norm_i and norm_j store the magnitude (length) of the two vectors

      for (int k = 0; k < data.ncol(); ++k) {
        dot_product += data(i, k) * data(j, k);
        norm_i += pow(data(i, k), 2);
        norm_j += pow(data(j, k), 2);
        // multiplying corresponding components of the two vectors and adding them up
        // calculate the norms (magnitudes) of the two vectors by summing the squares of their components
      }

      double cosine_similarity = dot_product / (sqrt(norm_i) * sqrt(norm_j));
      // cosine similarity is the dot product divided by the product of the two vectors' magnitudes

      dist_matrix(i, j) = 1 - cosine_similarity; // Cosine distance
      dist_matrix(j, i) = dist_matrix(i, j); // symetric
    }
  }

  return dist_matrix;
}
