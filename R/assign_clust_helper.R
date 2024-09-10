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
.assign_clusters <- function(distance_matrix, centroids_indices) {
    cluster_assignments <- numeric(nrow(distance_matrix))

    # Only use the distance rows corresponding to centroids
    centroids_distances <- distance_matrix[, centroids_indices]

    for (i in 1:nrow(distance_matrix)) {
      cluster_assignments[i] <- which.min(centroids_distances[i, ])
    }

    return(cluster_assignments)
  }
