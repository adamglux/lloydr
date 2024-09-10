
#' Title
#'
#' @param distance desired distance (as string)
#' @param data dataframe
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#' @export
#'
#' @return calculated distances
#'
#'@keywords internal
#'
#'
.metrics_function <- function(metric, data) {
  ### TO DO: make the metric all lowercase
  if(metric == "euclidean") {
    distances <- euclidean_distances(data)
  }

  if(metric == "manhattan") {
    distances <- manhattan_distances(data)
  }

  if(metric == "cosine") {
    distances <- cosine_distances(data)
  }

  if(metric == "gower") {
    distances <- gower_distances(data)
  }

  return(distances)
}
