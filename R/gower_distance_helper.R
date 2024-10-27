
#' Calculate Gower Distances
#'
#' @param point these are points
#' @param centroid centroid points
#'
#' @return numeric distance values
#' @importFrom StatMatch gower.dist
#'
#' @keywords internal
.gower_distance <- function(point, centroid) {
  # Convert the point and centroid to data frames with 1 row
  point_df <- as.data.frame(t(point))  # transpose to make it a 1-row data frame
  centroid_df <- as.data.frame(t(centroid))  # same for the centroid

  # tryCatch to handle potential errors during distance calculation
  gower_dist <- tryCatch({
    StatMatch::gower.dist(point_df, centroid_df)
  }, error = function(e) {
    warning("Error in calculating Gower distance: ", e$message)
    return(NA)  # Return NA if an error occurs
  })

  return(as.numeric(gower_dist))  # return the distance as a numeric value
}

