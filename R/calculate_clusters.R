#' Calculate Clusters
#'
#' @param df A dataframe
#' @param k an integer number of clusters
#' @param distance A distance measure
#' @param set.seed Set a random seed, default is none (FALSE)
#' @param max.iter Set maximum iterations for convergence
#'
#' @return CHANGE THIS
#' @import Rcpp
#' @export
#'
#' @examples
#' sum(1:10)
calculate_clusters <- function(df, k, distance, max.iter = 100) {
  nothing1 <- euclidean_distances(df)
  nothing2 <- cosine_distances(df)
  nothing3 <- gower_distances(df)
  scale4num <- sapply(df, is.numeric)
  nothing4 <- scale_df(df, scale4num)
  nothing5 <- manhattan_distances(df)
  return(df)

}

