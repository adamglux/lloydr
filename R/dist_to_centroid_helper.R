#' Distance to Centroids
#'
#' @param point points from matrix
#' @param centroid cetroid matrix
#' @param metric type of distance
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#'

.distance_to_centroid <- function(point, centroid, metric) {
  if (metric == "euclidean") {
    return(euclidean_distance_cpp(point, centroid))
  } else if (metric == "manhattan") {
    return(manhattan_distance_cpp(point, centroid))
  } else if (metric == "cosine") {
    return(cosine_distance_cpp(point, centroid))
  } else if (metric == "gower") {
    return(.gower_distance(point, centroid))
  } else {
    stop("Unsupported metric")
  }
}

