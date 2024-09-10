#' Extract Matrix
#'
#' @param df A dataframe
#'
#' @return
#' @export
#'
extractMatrix <- function(df){
  numeric.columns <- df[, sapply(df, is.numeric)]
  non.numeric.columns <- df[, !sapply(df, is.numeric)]
  list.of.columns <- c(names(numeric.columns), names(non.numeric.columns))
  data.frame.combined <- cbind(numeric.columns, non.numeric.columns)
  names(data.frame.combined) <- list.of.columns
  the.matrix <- as.matrix(numeric.columns)

  return(list(DataMatrix = the.matrix,
              Non_Numeric_Components = non.numeric.columns,
              Whole_Data_Frame = data.frame.combined))
}


