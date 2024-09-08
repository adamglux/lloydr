#' Helper Function to Scale Data in Dataframe
#'
#' @param df a data.frame
#'
#' @return scaled df
#' @keywords internal
#'
.data_scaler_helper <- function(df) {

  numeric_cols <- sapply(df, is.numeric)
  #checks if columns are numeric
  scaled_df <- scale_df(df, numeric_cols)
  #applies the CPP scaling function

  #returns scaled dataframe
  return(scaled_df)
}
