

extractMatrix <- function(df){
  print("not ready yet")
}

list.of.columns <- colnames(data2)
num.of.columns <- ncol(data2)
non.numeric.columns <- data2[, !sapply(data2, is.numeric)]
numeric.columns <- data2[, sapply(data2, is.numeric)]
