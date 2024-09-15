#' Dunn Index Helper Function
#'
#' @param clusters a kmeans object output from using calculate_clusters() in this package
#' @param metric a distance measure
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#' @keywords internal
#'
.dunn_index <- function(clusters, metric) {

  # Intra-cluster distance: maximum distance within a cluster
  max_intra_cluster_dist <- 0
  for (i in unique(clusters$cluster)) {
    cluster_points <- clusters$data[clusters$cluster == i, ]
    intra_dist_matrix <- .point_distances_helper(metric, cluster_points) #as.matrix(dist(cluster_points))
    max_intra_cluster_dist <- max(max_intra_cluster_dist, max(intra_dist_matrix, na.rm = TRUE))
  }

  # Inter-cluster distance: minimum distance between centroids
  inter_centroid_dists <- as.matrix(dist(clusters$centers)) #.distance_to_centroid(clusters[i, ], centroids[j, ], distance_metric)#
  min_inter_cluster_dist <- min(inter_centroid_dists[inter_centroid_dists > 0], na.rm = TRUE)

  # Dunn index
  dunn_index <- min_inter_cluster_dist / max_intra_cluster_dist
  return(dunn_index)
}
