#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double euclidean_distance_cpp(NumericVector point, NumericVector centroid) {
  // each point and centroid is passed in

 double sum = 0.0; //initialise sum vector
  for (int i = 0; i < point.size(); ++i) {
    //iterate over the number of points
    sum += pow(point[i] - centroid[i], 2);
    //square the difference (the distance)
  }
  return sqrt(sum);
}
