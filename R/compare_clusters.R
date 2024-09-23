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
#' data <- data.frame(x = c(rnorm(20), rnorm(20,10,5), rnorm(20, 20, 5)),
#'                    y = c(rnorm(20), rnorm(20,10,5), rnorm(20, 20, 5)))
#' cluster_comparison <- compare_clusters(data, k = 2:6)
#' print(cluster_comparison)

compare_clusters <- function(df, k, max.iter = 100, tol = 1e-4, scale = TRUE) {

  ######## input validation
  if (!is.data.frame(df)) stop("Input must be a data frame.")
  if (any(!is.numeric(k)) || any(k <= 0) || any(k%%1 !=0)  || any(k <= 1)) {
    stop("k must be a positive integer, greater than 1.")
  }

  if (any(k >= nrow(df))) {
    stop("Number of clusters cannot exceed the number of data points.")
  }

  if (!is.numeric(max.iter) || max.iter <= 0 || max.iter%%1 != 0) {
    stop("max.iter must be a positive integer.")
  }
  if (!is.numeric(tol) || tol <= 0) {
    stop("Tolerance must be a positive number.")
  }

  if (!is.logical(scale) || length(scale) != 1) {
    stop("Scale paramter must be TRUE to scale data or FALSE to skip scalling.")
  }

  ########

  distances <- c('euclidean', 'manhattan', 'cosine', 'gower')

  combinations <- length(distances) * length(k)

  #make an empty dataframe
  cluster_df <- data.frame(distance = rep(NA, combinations),
                           k = rep(NA, combinations),
                           time = rep(NA, combinations),
                           convergence = rep(NA, combinations),
                           DUNN = rep(NA, combinations))

  row_index <- 1
  for (k_value in k) {
    for (dist in distances) {
     #start time
     start_time <- Sys.time()

     # Calculate clusters
     clusters <- calculate_clusters(df = df,
                                    k = k_value,
                                    distance = dist,
                                    max.iter = max.iter,
                                    tol = tol,
                                    scale = scale)

     #end time
     end_time <- Sys.time()

     #time taken
     time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
     time_taken <- round(time_taken, 3)

     # Calculate Dunn index
     dunn_index <- round(.dunn_index(clusters, dist),5)

     # Fill the dataframe
     # cluster_df[row_index, ] <- c(distance = dist,
     #                              k = k_value,
     #                              time = time_taken,
     #                              convergence = clusters$iterations,
     #                              DUNN = dunn_index)

     # assign columns
     cluster_df[row_index, "distance"] <- dist
     cluster_df[row_index, "k"] <- k_value
     cluster_df[row_index, "time"] <- time_taken
     cluster_df[row_index, "convergence"] <- clusters$iterations
     cluster_df[row_index, "DUNN"] <- dunn_index

     row_index <- row_index + 1
   }
  }

  return(cluster_df)
}
