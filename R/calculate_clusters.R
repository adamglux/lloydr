#' Calculate Clusters
#'
#' @param df A dataframe
#' @param k an integer number of clusters
#' @param distance A distance measure
#' @param max.iter Set maximum iterations for convergence
#' @param tol tolerance
#' @param scale scale data y/n
#'
#' @useDynLib lloydr, .registration = TRUE
#' @return CHANGE THIS
#' @import Rcpp
#' @export
#'
#' @examples
#'
#'
#'
#'
#' @return matrix of cetroids
#'
#'@keywords internal
#' sum(1:10)
calculate_clusters <- function(df, k, distance, max.iter = 100, tol = 1e-4, scale = TRUE) {
  ##TO DO: add more input validation
  if (!is.data.frame(df)) stop("Input must be a data frame.")
  if (!is.numeric(k) || k <= 0) stop("k must be a positive integer.")
  if (!is.character(distance) || !(distance %in% c("euclidean", "manhattan", "cosine", "gower"))) {
    stop("Distance must be one of 'euclidean', 'manhattan', 'cosine', 'gower'.")
  }

  #turn data into number matrix
  matrix.from.data <- data.matrix(df)

  if (any(is.na(matrix.from.data))) stop("Data contains NA values. Please handle missing values before clustering.")

  #scale da data
  if(scale) {
    scaled.data <- scale_data(matrix.from.data)
  } else {scaled.data <- matrix.from.data}
  #if scale is set to false, then use the data matrix as-is w/o scaling


  #give it a distance matrix
  #distance.matrix <- .metrics_function(distance, scaled.data)

  #do the lloyds
  clusters.by.lloyd <- .lloyds_algorithm(scaled.data, k, distance, max_iters = max.iter, tol = tol)

  #TO DO: fix this return
  return(list(cluster_assignments = clusters.by.lloyd$cluster_assignments,
              centroids = clusters.by.lloyd$centroids,
              scaled_data = scaled.data,
              iterations = clusters.by.lloyd$iterations))

}
