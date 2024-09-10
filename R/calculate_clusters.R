#' Calculate Clusters
#'
#' @param df A dataframe
#' @param k an integer number of clusters
#' @param distance A distance measure
#' @param max.iter Set maximum iterations for convergence
#'
#' @return CHANGE THIS
#' @import Rcpp
#' @export
#'
#' @examples
#' sum(1:10)
calculate_clusters <- function(df, k, distance, max.iter = 100) {
  ##TO DO: input validation

  ## if it's a dataframe, use extract df to get a matrix

  ## if its a matrix, continue

  #give it a distance matrix
  distance.matrix <- .metrics_function(distance, df)


  .lloyds_algorithm(df, k, distance, max_iters = max.iter)
}
