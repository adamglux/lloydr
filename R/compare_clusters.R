#' Compare Clusters
#'
#' @description This function compares clustering results for different distance metrics and values of `k`.
#' @param df A dataframe containing the data to be clustered.
#' @param k A vector of integers representing the desired number of clusters (must be > 1).
#' @param max.iter An integer for the maximum number of iterations for convergence.
#' @param tol A numeric value specifying the tolerance for convergence.
#' @param scale Logical. If TRUE, scales the data before clustering; if FALSE, does not scale.
#' @useDynLib lloydr, .registration = TRUE
#' @return A dataframe containing the following columns:
#' \item{distance}{The distance metric used.}
#' \item{k}{The number of clusters.}
#' \item{time}{Time taken for the clustering process (in seconds).}
#' \item{convergence}{The number of iterations to reach convergence.}
#' \item{DUNN}{The Dunn index for the clustering result.}
#' @import Rcpp
#' @useDynLib lloydr, .registration = TRUE
#' @export
#' @examples
#'
#' data <- mtcars
#' cluster_comparison <- compare_clusters(data, k = 2:4)
#' print(cluster_comparison)

compare_clusters <- function(df, k, max.iter = 100, tol = 1e-4, scale = TRUE) {
  distances <- c('euclidean', 'manhattan', 'cosine', 'gower')

  combinations <- length(distances) * length(k)

  #make an empty dataframe
  cluster_df <- data.frame(distance = rep(NA, combinations),
                           k = rep(NA, combinations),
                           time = rep(NA, combinations),
                           covergence = rep(NA, combinations),
                           DUNN = rep(NA, combinations))

  row_index <- 1
  for (k_value in k) {
    for (dist in distances) {
     #start time
     start_time <- Sys.time()

     # Calculate clusters
     clusters <- calculate_clusters(df = df, k = k_value, distance = dist, max.iter = max.iter, tol = tol, scale = scale)

     #end time
     end_time <- Sys.time()

     #time taken
     time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
     time_taken <- round(time_taken, 3)

     # Calculate Dunn index
     dunn_index <- round(.dunn_index(clusters, dist),5)

     # Fill the dataframe
     cluster_df[row_index, ] <- c(distance = dist,
                                  k = k_value,
                                  time = time_taken,
                                  convergence = clusters$iterations,
                                  DUNN = dunn_index)

     row_index <- row_index + 1
   }
  }

  return(cluster_df)
}
