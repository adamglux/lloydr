#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double manhattan_distance_cpp(NumericVector point, NumericVector centroid) {
  //pass in a set of points and centroids

  double sum = 0.0; //initialise sum vector
  for (int i = 0; i < point.size(); ++i) {
    //iterate over num points
    sum += std::abs(point[i] - centroid[i]);
    //distance is the absolute value of the difference
  }
  return sum;
}
