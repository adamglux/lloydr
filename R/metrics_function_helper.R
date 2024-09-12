
#' Select Metrics helper
#'
#' @param distance desired distance (as string)
#' @param data dataframe
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#'
#' @return calculated distances
#'
#'@keywords internal
#'
#'
.metrics_function <- function(metric, data) {
  ### TO DO: make the metric all lowercase
  if(metric == "euclidean") {
    distances <- euclidean_distance_cpp(data)
  }

  if(metric == "manhattan") {
    distances <- manhattan_distance_cpp(data)
  }

  if(metric == "cosine") {
    distances <- cosine_distance_cpp(data)
  }

  if(metric == "gower") {
    distances <- .gower_distance(data)
  }

  return(distances)
}
