#' Compute percentage of NA in a vector
#' @param x input vector
#' @return number in [0,1] = the percentage of NA; returns 1 is x is empty.
percent_na <- function(x) {
  if (length(x) == 0) {
    res <- 1
  } else {
    res <- sum(is.na(x)) / length(x)
  }
  return(res)
}
