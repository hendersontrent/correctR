#' Compute correlated t-statistic and p-value for k-fold cross-validated results
#' @importFrom stats var pt
#' @param x \code{numeric} vector of values for model A
#' @param y \code{numeric} vector of values for model B
#' @param n \code{integer} denoting total sample size
#' @param k \code{integer} denoting number of folds used in k-fold
#' @param tailed \code{character} denoting whether to perform a two-tailed or one-tailed test. Can be one of \code{"two"} or \code{"one"}. Defaults to \code{"two"}
#' @param greater \code{character} specifying whether \code{"x"} or \code{"y"} is greater for the one-tailed test if \code{tailed = "one"}. Defaults to \code{NULL}
#' @return \code{data.frame} containing the test statistic and p-value
#' @references Nadeau, C., and Bengio, Y. Inference for the Generalization Error. Machine Learning 52, (2003).
#' @references Corani, G., Benavoli, A., Demsar, J., Mangili, F., and Zaffalon, M. Statistical comparison of classifiers through Bayesian hierarchical modelling. Machine Learning, 106, (2017).
#' @author Trent Henderson
#' @export
#' @examples
#' x <- rnorm(100, mean = 95, sd = 0.5)
#' y <- rnorm(100, mean = 90, sd = 1)
#' kfold_ttest(x = x, y = y, n = 100, k = 5, tailed = "two")
#'

kfold_ttest <- function(x, y, n, k, tailed = c("two", "one"), greater = NULL){

  # Arg checks

  tailed <- match.arg(tailed)

  if(length(x) != length(y)){
    stop("x and y are not the same length.")
  }

  if(!is.numeric(x) || !is.numeric(y)){
    stop("x and y should be numeric vectors of the same length.")
  }

  if(!is.numeric(n) || !is.numeric(k)){
    stop("n and k should be integer scalars.")
  }

  if(length(n) != 1 || length(k) != 1){
    stop("n and k should be integer scalars.")
  }

  if(tailed == "one"){
    if(!greater %in% c("x", "y")){
      stop("If tailed = 'one', greater must be either 'x' or 'y'")
    }
  }

  #--------- Calculations ---------

  if(tailed == "two"){
    d <- x - y
    if (sum(d) == 0) {
      tmp <- data.frame(statistic = 0, p.value = 1)
      return(tmp)
    } else{
      statistic <- mean(d, na.rm = TRUE) / sqrt(stats::var(d, na.rm = TRUE) * ((1 / n + (1 / k)) / (1 - 1 / k)))
      p.value <- 2 * stats::pt(statistic, n - 1, lower.tail = FALSE)
    }
  } else{
    if(greater == "x"){
      d <- x - y
    } else{
      d <- y - x
    }

    if (sum(d) == 0) {
      tmp <- data.frame(statistic = 0, p.value = 1)
      return(tmp)
    } else{
      statistic <- mean(d, na.rm = TRUE) / sqrt(stats::var(d, na.rm = TRUE) * ((1 / n + (1 / k)) / (1 - 1 / k)))
      p.value <- stats::pt(statistic, n - 1, lower.tail = FALSE)
    }
  }

  tmp <- data.frame(statistic = statistic, p.value = p.value)
  return(tmp)
}
