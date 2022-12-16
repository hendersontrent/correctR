#' Compute correlated t-statistic and p-value for repeated k-fold cross-validated results
#' @importFrom stats var pt
#' @param x \code{numeric} vector of values for model A
#' @param y \code{numeric} vector of values for model B
#' @param n \code{integer} denoting total sample size
#' @param k \code{integer} denoting number of folds used in k-fold
#' @param r \code{integer} denoting number
#' @return object of class \code{data.frame}
#' @references Nadeau, C., and Bengio, Y. Inference for the Generalization Error. Machine Learning 52, (2003).
#' @author Trent Henderson
#' @export
#'

rep.kfold.t.test <- function(x, y, n = length(x), k, r){

  # Arg checks

  if(length(x) != length(y)){
    stop("x and y are not the same length.")
  }

  if(!is.numeric(x) | !is.numeric(y)){
    stop("x and y should be numeric vectors of the same length.")
  }

  if(!is.numeric(n) | !is.numeric(k) !is.numeric(r) |
     length(n) != 1 | length(k) != 1 | length(r) != 1){
    stop("n, k, and r should all be integer scalars.")
  }

  # Calculations

  d <- x - y # Calculate differences
  sigma_2_mod <- stats::var(d, na.rm = TRUE) * (1/n + n2/n1) # Calculate modified variance
  statistic <- mean(d, na.rm = TRUE) / sqrt(sigma_2 * ((1/n + (1/k)) / (1 - 1/k))) # Calculate t-statistic

  if(statistic < 0){
    p.value <- stats::pt(statistic, n - 1) # p-value for left tail
  } else{
    p.value <- stats::pt(statistic, n - 1, lower.tail = FALSE) # p-value for right tail
  }

  tmp <- data.frame(statistic = statistic, p.value = p.value)
  return(tmp)
}
