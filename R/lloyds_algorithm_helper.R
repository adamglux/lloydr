#' Lloyd's Algorithm
#'
#' @param data data frame
#' @param k num of clusters
#' @param distance_func distance metric
#' @param max_iters max iterations
#' @param tol tolerance
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#' @export
#'
#' @return vector of cluster assignments
#'
#'@keywords internal
#'
.lloyds_algorithm <- function(data, k, distance_func, max_iters, tol = 1e-4) {

  #randomly assign centroids (recalculate later)
  centroids <- data[sample(1:nrow(data), k), ]

  cluster_assignments <- numeric(nrow(data))
  prev_centroids <- centroids

  for (iter in 1:max_iters) {
    # Step 2: Assign points to the nearest centroid using the helper function
    cluster_assignments <- .assign_clusters(data, centroids, distance_func)

    # Step 3: Recalculate centroids using the helper function
    centroids <- .recalculate_centroids(data, cluster_assignments, k)

    # Step 4: Check for convergence
    centroid_shift <- max(apply((centroids - prev_centroids)^2, 1, sum))
    if (centroid_shift < tol) {
      iter <- iter
      cat("Converged in", iter, "iterations\n")
      break
    }

    # Update previous centroids
    prev_centroids <- centroids
  }

  # Return final cluster assignments and centroids
  return(list(cluster_assignments = cluster_assignments, centroids = centroids, iterations = iter))
}
