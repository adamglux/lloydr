#' Distance Between Points
#'
#' @param dist distance metric
#' @param data a data matrix
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#' @keywords internal

.point_distances_helper <- function(metric, data) {
  if (metric == "euclidean") {
    return(euclidean_point_distances(data))
  } else if (metric == "manhattan") {
    return(manhattan_point_distances(data))
  } else if (metric == "cosine") {
    return(cosine_point_distances(data))
  } else if (metric == "gower") {
    return(as.matrix(StatMatch::gower.dist(data)))
  } else {
    stop("Unsupported metric")
  }
}
