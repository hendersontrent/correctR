#' Compute correlated t-statistic and p-value for resampled data
#' @importFrom stats var pt
#' @param x \code{numeric} vector of values for model A
#' @param y \code{numeric} vector of values for model B
#' @param n \code{integer} denoting number of repeat samples. Defaults to \code{length(x)}
#' @param n1 \code{integer} denoting train set size
#' @param n2 \code{integer} denoting test set size
#' @param tailed \code{character} denoting whether to perform a two-tailed or one-tailed test. Can be one of \code{"two"} or \code{"one"}. Defaults to \code{"two"}
#' @param greater \code{character} specifying whether \code{"x"} or \code{"y"} is greater for the one-tailed test if \code{tailed = "one"}. Defaults to \code{NULL}
#' @return \code{data.frame} containing the test statistic and p-value
#' @references Nadeau, C., and Bengio, Y. Inference for the Generalization Error. Machine Learning 52, (2003).
#' @references Bouckaert, R. R., and Frank, E. Evaluating the Replicability of Significance Tests for Comparing Learning Algorithms. Advances in Knowledge Discovery and Data Mining. PAKDD 2004. Lecture Notes in Computer Science, 3056, (2004).
#' @author Trent Henderson
#' @export
#' @examples
#' x <- rnorm(100, mean = 95, sd = 0.5)
#' y <- rnorm(100, mean = 90, sd = 1)
#' resampled_ttest(x = x, y = y, n = 100, n1 = 80, n2 = 20, tailed = "two")
#'

resampled_ttest <- function(x, y, n, n1, n2, tailed = c("two", "one"), greater = NULL){

  # Arg checks

  tailed <- match.arg(tailed)

  if(length(x) != length(y)){
    stop("x and y are not the same length.")
  }

  if(!is.numeric(x) || !is.numeric(y)){
    stop("x and y should be numeric vectors of the same length.")
  }

  if(!is.numeric(n) || !is.numeric(n1) || !is.numeric(n2) ||
     length(n) != 1 || length(n1) != 1 || length(n2) != 1){
    stop("n, n1, and n2 should all be integer scalars.")
  }

  if(missing(n) || is.null(n)){
    n <- length(x)
    message("n argument missing. Using length(x) as default.")
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
      statistic <- mean(d, na.rm = TRUE) / sqrt(stats::var(d, na.rm = TRUE) * (1 / n + n2 / n1))
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
        statistic <- mean(d, na.rm = TRUE) / sqrt(stats::var(d, na.rm = TRUE) * (1 / n + n2 / n1))
        p.value <- stats::pt(statistic, n - 1, lower.tail = FALSE)
      }
    }

  tmp <- data.frame(statistic = statistic, p.value = p.value)
  return(tmp)
}
