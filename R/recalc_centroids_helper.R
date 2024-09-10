#' Recalculate Centroids Helper Function
#'
#' @param data a dataframe
#' @param cluster_assignments which cluster belongs to
#' @param k num clusters
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#' @export
#'
#' @return matrix of cetroids
#'
#'@keywords internal
.recalculate_centroids <- function(data, cluster_assignments, k) {
  centroids <- matrix(NA, nrow = k, ncol = ncol(data))

  for (j in 1:k) {
    centroids[j, ] <- base::colMeans(data[cluster_assignments == j, , drop = FALSE], na.rm = TRUE)
  }
  return(centroids)
}

