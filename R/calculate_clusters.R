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
#' @return A list containing the following components:
#' \item{cluster}{A vector of integers indicating the cluster assignment for each observation.}
#' \item{centers}{A matrix of cluster centers.}
#' \item{totss}{The total sum of squares.}
#' \item{withinss}{The within-cluster sum of squares.}
#' \item{tot.withinss}{The total within-cluster sum of squares.}
#' \item{betweenss}{The between-cluster sum of squares.}
#' \item{size}{The size of each cluster.}
#' \item{iterations}{The number of iterations for convergence.}
#' \item{metric}{The distance metric used.}
#' \item{data}{The input data with cluster assignments appended.}
#' @import Rcpp
#' @export
#'
#' @examples
#' data <- mtcars
#' cluster_values <- calculate_clusters(data,3,"euclidean")
#' print(cluster_values)
#'
#'
calculate_clusters <- function(df, k, distance, max.iter = 100, tol = 1e-4, scale = TRUE) {
  #correct capitalisation in distance
  distance <- tolower(distance)

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

  #do the lloyds
  clusters.by.lloyd <- .lloyds_algorithm(scaled.data, k, distance, max_iters = max.iter, tol = tol)

  #calc sum of squares
  totss <- sum((scaled.data - rowMeans(scaled.data))^2)

  #calc within sum of squares
  tot.withinss <- sum(sapply(1:k, function(i) sum((scaled.data[clusters.by.lloyd$cluster_assignments == i, ] - clusters.by.lloyd$centroids[i, ])^2)))
  withinss <-  sapply(1:k, function(i) sum((scaled.data[clusters.by.lloyd$cluster_assignments == i, ] - clusters.by.lloyd$centroids[i, ])^2))

  #append cluster assignments
  data <- cbind(matrix.from.data, cluster = clusters.by.lloyd$cluster_assignments)

  #print out convergence
  #cat("Converged in", clusters.by.lloyd$iterations, "iterations\n")

  result <- list(
    cluster = clusters.by.lloyd$cluster_assignments,
    centers = clusters.by.lloyd$centroids,
    totss = totss,
    withinss = withinss,
    tot.withinss = tot.withinss,
    betweenss = totss - tot.withinss,
    size = table(clusters.by.lloyd$cluster_assignments),
    iterations = clusters.by.lloyd$iterations,
    metric = distance,
    data = data
  )
  #structure(result, class = c("kmeans", "kmeans_custom"))

  class(result) <- c( "kmeans_custom", "kmeans") #"kmeans_custom" #
  return(result)

}
