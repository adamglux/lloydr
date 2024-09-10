#' Helper Function to Scale Data in Dataframe
#'
#' @param df A data.frame
#'
#' @useDynLib lloydr, .registration = TRUE
#' @import Rcpp
#' @export
#'
#' @return A scaled df
#'
#'@keywords internal
.data_scaler_helper <- function(df) {

  numeric_cols <- sapply(df, is.numeric)
  #checks if columns are numeric
  standardised_data <- scale_df(df, numeric_cols)
  #applies the CPP scaling function
  #returns scaled dataframe
  return(standardised_data)
}

