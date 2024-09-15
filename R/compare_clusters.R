
#' Title
#'
#' @param df a dataframe
#' @param k a vector of desired clusters > 1
#' @param max.iter maximum number of iterations to convergence
#' @param tol distance tollerance
#' @param scale TRUE = scale date; FALSE = do no scale data
#'
#' @useDynLib lloydr, .registration = TRUE
#' @return Dataframe comparing clusters
#' @import Rcpp
#' @export
#'

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
     dunn_index <- .dunn_index(clusters, dist)

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
