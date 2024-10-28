#' Calculate Clusters
#'
#'
#' @description This function applies Lloyd's algorithm to cluster a dataset based on a specified distance metric (e.g., Euclidean, Manhattan, Cosine, or Gower). It returns a list containing cluster assignments, centroids, sum of squares metrics, and convergence details, with optional scaling of the data before clustering.
#' @param df A dataframe
#' @param k an integer number of clusters (must be less than 1)
#' @param distance A distance measure, from one of: {"Euclidean", "Manhattan", "Cosine", "Gower"}, with no default. Note: this parameter is not case sensitive, i.e. "euclidean" and "Euclidean" will obtain the same results.
#' @param max.iter An integer for the maximum number of iterations for convergence.
#' @param tol A numeric value specifying the tolerance for convergence.
#' @param scale Logical. If TRUE, scales the data before clustering; if FALSE, does not scale.
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
#'
#' ######
#' ###### Simple Randomly Generated Dataset
#' ######
#'
#' set.seed(501)
#' data <- data.frame(x = c(rnorm(20), rnorm(20,10,5), rnorm(20, 20, 5)),
#'                    y = c(rnorm(20), rnorm(20,10,5), rnorm(20, 20, 5)))
#'
#'
#' # using data scaling
#' cluster_values <- calculate_clusters(df = data, k = 3, distance = "euclidean")
#'
#' #S3 object methods
#' #print(cluster_values)
#' summary(cluster_values)
#'
#' #plot clusters and cluster centroids for scaled data
#' plot(cluster_values$data.scaled[,1],
#'      cluster_values$data.scaled[,2],
#'      col = cluster_values$cluster)
#' points(cluster_values$centers, col = 1:2, pch = 8, cex = 4)
#'
#' # without scaling
#' cluster_values <- calculate_clusters(df = data, k = 3, distance = "euclidean", scale = FALSE)
#' summary(cluster_values)
#'
#' #plot clusters and cluster centroids for non-scaled data
#' plot(cluster_values$data[,1],
#'      cluster_values$data[,2],
#'      col = cluster_values$cluster)
#' points(cluster_values$centers, col = 1:2, pch = 8, cex = 4)
#'
#'
#' ######
#' ###### Hair Eye Colour Dataset example
#' ######
#'
#' ### note: see vignettes for side-by-side comparison plots, and commentary.
#'
#' ######
#' ###### dataset
#' ######
#'
#' data("HairEyeColor")
#' summary(HairEyeColor)
#' dataset <- as.data.frame(HairEyeColor)
#' summary(dataset)
#'
#' ######
#' ###### plot hair / eye distributions
#' ######
#'
#' hair_eye_table <- with(dataset, tapply(Freq, list(Hair, Eye), sum))
#' # Set custom colors
#' colours_p <- c("#593722", "#6385b1", "#a58454", "#637f19" )
#' # Create the bar plot
#' barplot(hair_eye_table,
#'         beside = TRUE,               # Bars side by side
#'         col = colours_p,            # Fill colors
#'         main = "Hair and Eye Color Distribution",  # Title
#'         xlab = "Hair Color",         # X-axis label
#'         ylab = "Count",              # Y-axis label
#'         legend.text = colnames(hair_eye_table),  # Add legend for eye colors
#'         args.legend = list(x = "topright", bty = "n"))
#'
#' ######
#' ###### calculate clusters
#' ######
#'
#' set.seed(1234)
#' clust1 <- calculate_clusters(dataset,4, distance = "euclidean")
#' print(clust1)
#' clust2 <- calculate_clusters(dataset,4, distance = "manhattan")
#' print(clust2)
#' clust3 <- calculate_clusters(dataset,4, distance = "cosine")
#' print(clust3)
#' clust4 <- calculate_clusters(dataset,4, distance = "gower", max.iter = 1000)
#' print(clust4)
#'
#' ######
#' ###### append dataset
#' ######
#'
#' dataset$cluster1 <- clust1$cluster
#' dataset$cluster2 <- clust2$cluster
#' dataset$cluster3 <- clust3$cluster
#' dataset$cluster4 <- clust4$cluster
#'
#' ######
#' ###### plot the results
#' ######
#'
#' ## results for euclidean cluster
#' hair_cluster_table <- with(dataset, tapply(Freq, list(Hair, factor(cluster1)), sum))
#' barplot(hair_cluster_table,
#'         beside = TRUE, col = colours_p,
#'         main = "Cluster Distribution",
#'         xlab = "Hair Color", ylab = "Count",
#'         legend.text = colnames(hair_cluster_table),
#'         args.legend = list(x = "topright", bty = "n"))
#'
#'
#' ## results for manhattan cluster
#' hair_cluster_table <- with(dataset, tapply(Freq, list(Hair, factor(cluster2)), sum))
#' barplot(hair_cluster_table,
#'         beside = TRUE, col = colours_p,
#'         main = "Cluster Distribution",
#'         xlab = "Hair Color", ylab = "Count",
#'         legend.text = colnames(hair_cluster_table),
#'         args.legend = list(x = "topright", bty = "n"))
#'
#' ## results for cosine cluster
#' hair_cluster_table <- with(dataset, tapply(Freq, list(Hair, factor(cluster3)), sum))
#' barplot(hair_cluster_table,
#'         beside = TRUE, col = colours_p,
#'         main = "Cluster Distribution",
#'         xlab = "Hair Color", ylab = "Count",
#'         legend.text = colnames(hair_cluster_table),
#'         args.legend = list(x = "topright", bty = "n"))
#'
#' ## results for gower cluster
#' hair_cluster_table <- with(dataset, tapply(Freq, list(Hair, factor(cluster4)), sum))
#' barplot(hair_cluster_table,
#'         beside = TRUE, col = colours_p,
#'         main = "Cluster Distribution",
#'         xlab = "Hair Color", ylab = "Count",
#'         legend.text = colnames(hair_cluster_table),
#'         args.legend = list(x = "topright", bty = "n"))
#'
#'
#'
#'
#'
#'
#'
calculate_clusters <- function(df, k, distance, max.iter = 100, tol = 1e-4, scale = TRUE) {
  #correct capitalisation in distance
  distance <- tolower(distance)

  ####### Input validation
  if (!is.data.frame(df)) stop("Input must be a data frame.")

  if (length(k) > 1) {
    stop ("k must be 1 value. \nA vector of values for k can be used in the lloydr::compare_clusters() function")
  }

  if (!is.numeric(k) || k <= 1 || k%%1 !=0){
    stop("k must be a positive integer and > 1.")
  }

  if (k >= nrow(df)) {
    stop("Number of clusters cannot exceed number of data points")
  }

  if (!is.character(distance) || !(distance %in% c("euclidean", "manhattan", "cosine", "gower"))) {
    stop("Distance must be one of 'euclidean', 'manhattan', 'cosine', 'gower'.")
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

  #######

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
  data.scaled <- cbind(scaled.data, cluster = clusters.by.lloyd$cluster_assignments)

  # Check convergence and print iterations
  if (clusters.by.lloyd$converged) {
    #cat("Converged in", clusters.by.lloyd$iterations, "iterations\n")
  } else {
    message(c("Did not converge after ", max.iter, " iterations.\nTry increasing max.iter or adjusting tolerance."))
    return(NULL)
  }

  if (scale) {
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
      data = data,
      data.scaled = data.scaled
    ) } else {
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
    }

  #structure(result, class = c("kmeans", "kmeans_custom"))

  class(result) <- c( "kmeans_custom", "kmeans") #"kmeans_custom" #
  return(result)

}
