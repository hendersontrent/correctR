#' Compute correlated t-statistic and p-value for resampled data
#' @importFrom stats var pt
#' @param x \code{numeric} vector of values for model A
#' @param y \code{numeric} vector of values for model B
#' @param n \code{integer} denoting number of repeat samples. Defaults to \code{length(x)}
#' @param n1 \code{integer} denoting train set size
#' @param n2 \code{integer} denoting test set size
#' @return object of class \code{data.frame}
#' @references Nadeau, C., and Bengio, Y. Inference for the Generalization Error. Machine Learning 52, (2003).
#' @author Trent Henderson
#' @export
#'

resampled.t.test <- function(x, y, n = length(x), n1, n2){

  # Arg checks

  if(length(x) != length(y)){
    stop("x and y are not the same length.")
  }

  if(!is.numeric(x) | !is.numeric(y)){
    stop("x and y should be numeric vectors of the same length.")
  }

  if(!is.numeric(n) | !is.numeric(n1) !is.numeric(n2)){
    stop("n, n1, and n1 should all be integer scalars.")
  }

  # Calculations

  d <- x - y # Calculate differences
  sigma_2_mod <- stats::var(d, na.rm = TRUE) * (1/n + n2/n1) # Calculate modified variance
  statistic <- mean(d, na.rm = TRUE) / sqrt(sigma_2_mod) # Calculate t-statistic

  if(statistic < 0){
    p.value <- stats::pt(statistic, n - 1) # p-value for left tail
  } else{
    p.value <- stats::pt(statistic, n - 1, lower.tail = FALSE) # p-value for right tail
  }

  tmp <- data.frame(statistic = statistic, p.value = p.value)
  return(tmp)
}
