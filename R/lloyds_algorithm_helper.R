#' Lloyd's Algorithm
#'
#' @param data distance matrix
#' @param k num of clusters
#' @param distance_func distance metric
#' @param max_iters max iterations
#' @param tol tolerance
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#'
#' @return vector of cluster assignments
#'
#'@keywords internal
#'
.lloyds_algorithm <- function(data, k, distance_metric, max_iters = 100, tol = 1e-4) {
  # Compute the pairwise distance matrix for the entire dataset
  #distance_matrix <- .metrics_function(distance_metric, data)

  # Randomly initialize centroids
  centroids <- data[sample(1:nrow(data), k), ]

  cluster_assignments <- numeric(nrow(data))
  prev_centroids <- centroids

  for (iter in 1:max_iters) {
    # Step 1: Calculate the distance matrix between data points and centroids
    centroids_distances <- matrix(NA, nrow = nrow(data), ncol = k)

    for (i in 1:nrow(data)) {
      for (j in 1:k) {
        # Calculate distance between data point i and centroid j
        centroids_distances[i, j] <- .distance_to_centroid(data[i, ], centroids[j, ], distance_metric)
      }
    }

    # Step 2: Assign points to the nearest centroid
    cluster_assignments <- apply(centroids_distances, 1, which.min)

    # Step 3: Recalculate centroids
    centroids <- .recalculate_centroids(data, cluster_assignments, k)

    # Step 4: Check for convergence
    centroid_shift <- max(apply((centroids - prev_centroids)^2, 1, sum, na.rm = TRUE))

    if (is.na(centroid_shift)) {
      warning("Centroid shift resulted in NA, likely due to missing or empty data in centroids.")
    } else if (centroid_shift < tol) {
      break
    }

    # Update previous centroids
    prev_centroids <- centroids
  }

  # Return final cluster assignments, centroids, and number of iterations
  return(list(cluster_assignments = cluster_assignments, centroids = centroids, iterations = iter))
}







