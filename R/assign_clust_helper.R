#' Assign Clusters helper function
#'
#' @param data dataframe
#' @param centroids matrix of centroids
#' @param distance_func distance function
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#' @export
#'
#' @return vector of cluster assignments
#'
#'@keywords internal
#'
.assign_clusters <- function(data, centroids, distance_func) {
  cluster_assignments <- numeric(nrow(data))

  for (i in 1:nrow(data)) {
    distances <- apply(centroids, 1, function(centroid) distance_func(data[i, ], centroid))
    cluster_assignments[i] <- which.min(distances)
  }

  return(cluster_assignments)
}
