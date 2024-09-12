#' Assign Clusters helper function
#'
#' @param distance_matrix a matrix of distances
#' @param centroids_indices matrix of centroids
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#'
#' @return vector of cluster assignments
#'
#' @keywords internal
#'
.assign_clusters <- function(data, centroids, distance_function) {
  cluster_assignments <- numeric(nrow(data))
  centroids_distances <- matrix(NA, nrow = nrow(data), ncol = nrow(centroids))

  # Compute distances from data points to centroids
  for (i in 1:nrow(data)) {
    for (j in 1:nrow(centroids)) {
      centroids_distances[i, j] <- distance_function(data[i, ], centroids[j, ])
    }
  }

  # Assign each data point to the nearest centroid
  for (i in 1:nrow(data)) {
    cluster_assignments[i] <- which.min(centroids_distances[i, ])
  }

  return(cluster_assignments)
}
