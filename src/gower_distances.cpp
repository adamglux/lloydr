#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double gower_distance_cpp(NumericVector point, NumericVector centroid, NumericMatrix data) {
  //the overall similarity, S, between two objects is the weighted average
  // of the similarities calculated for all their descriptors (Wikipedia)

  //this is version 1, but a more comprehensive version is implemented using library(StatMatch) in R

  int n = point.size();
  double sum = 0.0;

  // Calculate the range of each feature for normalization
  NumericVector ranges(n);
  for (int i = 0; i < n; ++i) {
    double col_min = min(data(_, i));
    double col_max = max(data(_, i));
    ranges[i] = col_max - col_min;
  }

  // Calculate normalized distances for numeric values
  for (int i = 0; i < n; ++i) {
    if (ranges[i] > 0) {
      sum += std::abs(point[i] - centroid[i]) / ranges[i];
    }
  }

  return sum / n;  // Average distance
}
