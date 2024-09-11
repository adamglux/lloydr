#' Title
#'
#' @param point
#' @param centroid
#' @param metric
#' @param distance_matrix
#'
#' @return matrix
#' @export
#'
#' @examples
#'
.distance_to_centroid <- function(point, centroid, metric) {
  if (metric == "euclidean") {
    return(euclidean_distance_cpp(point, centroid))
  } else if (metric == "manhattan") {
    return(manhattan_distance_cpp(point, centroid))
  } else if (metric == "cosine") {
    return(cosine_distance_cpp(point, centroid))
  } else if (metric == "gower") {
    return(gower_distance_cpp(point, centroid, data))
  } else {
    stop("Unsupported metric")
  }
}

