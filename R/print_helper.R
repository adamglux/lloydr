#' Print Method for Cluster Values
#'
#' @param x custom kmeans object
#' @param ... further arguments passed to or from other methods.
#' @export
#' @keywords internal
#'
#'
print.kmeans_custom <- function(x,...) {
  cat("K-Means Clustering Using Lloyd's Algorithm\n")
  cat("With", length(unique(x$cluster)), "clusters of size", paste(x$size, collapse = ",") , "\n")
  cat("Distance measure: ", x$metric, "\n")
  cat("Iterations to Convergence: ", x$iterations, "\n")
  invisible(x)
}
