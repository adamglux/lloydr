#' Summary Method for Cluster Values
#'
#' @param object custom kmeans object
#' @param ... further arguments passed to or from other methods.
#' @export
#' @keywords internal
#'
#'
summary.kmeans_custom <- function(object,...) {
  cat("K-Means Clustering Using Lloyd's Algorithm\n")
  cat("With", length(base::unique(object$cluster)), "clusters of size", paste(object$size, collapse = ",") , "\n")
  cat("Distance measure: ", object$metric, "\n")
  cat("Iterations: ", object$iterations, "\n")

  cat("\nCentroids:\n")
  print(object$centers)

  invisible(object)
}
