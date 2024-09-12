#' Recalculate Centroids Helper Function
#'
#' @param data a dataframe
#' @param cluster_assignments which cluster belongs to
#' @param k num clusters
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#'
#' @return matrix of cetroids
#'
#'@keywords internal
.recalculate_centroids <- function(data, cluster_assignments, k) {
  centroids <- matrix(NA, nrow = k, ncol = ncol(data))
  for (i in 1:k) {
    cluster_points <- data[cluster_assignments == i, , drop = FALSE]
    if (nrow(cluster_points) > 0) {
      centroids[i, ] <- base::colMeans(cluster_points)
    } else {
      # Handle empty cluster case
      centroids[i, ] <- data[sample(1:nrow(data), 1), ]
    }
  }
  return(centroids)
}

