#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cosine_distance_cpp(NumericVector point, NumericVector centroid) {
  //pass in points and centroids

  double dot_product = 0.0, norm_a = 0.0, norm_b = 0.0;
  // initialise vectors for dot products and point-wise / centroid elements
  for (int i = 0; i < point.size(); ++i) {
    //iterate over num points
    dot_product += point[i] * centroid[i]; //dot product of each point (summed)
    norm_a += pow(point[i], 2); //sum of each point squared
    norm_b += pow(centroid[i], 2); //sum of each centroid squared
  }
  return 1.0 - (dot_product / (sqrt(norm_a) * sqrt(norm_b)));
}
