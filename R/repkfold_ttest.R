#' Compute correlated t-statistic and p-value for repeated k-fold cross-validated results
#' @importFrom stats var pt
#' @param data \code{data.frame} of values for model A and model B over repeated k-fold cross-validation. Four named columns are expected: \code{"model"}, \code{"values"}, \code{"k"}, and \code{"k"}
#' @param n1 \code{integer} denoting train set size
#' @param n2 \code{integer} denoting test set size
#' @param k \code{integer} denoting number of folds used in k-fold
#' @param r \code{integer} denoting number of repeats per fold
#' @param tailed \code{character} denoting whether to perform a two-tailed or one-tailed test. Can be one of \code{"two"} or \code{"one"}. Defaults to \code{"two"}
#' @param greater value specifying which value in the \code{"model"} column is greater for the one-tailed test if \code{tailed = "one"}. Defaults to \code{NULL}
#' @return \code{data.frame} containing the test statistic and p-value
#' @references Nadeau, C., and Bengio, Y. Inference for the Generalization Error. Machine Learning 52, (2003).
#' @references Bouckaert, R. R., and Frank, E. Evaluating the Replicability of Significance Tests for Comparing Learning Algorithms. Advances in Knowledge Discovery and Data Mining. PAKDD 2004. Lecture Notes in Computer Science, 3056, (2004).
#' @author Trent Henderson
#' @export
#' @examples
#' tmp <- data.frame(model = rep(c(1, 2), each = 60),
#'   values = c(stats::rnorm(60, mean = 0.6, sd = 0.1),
#'   stats::rnorm(60, mean = 0.4, sd = 0.1)),
#'   k = rep(c(1, 1, 2, 2), times = 15),
#'   r = rep(c(1, 2), times = 30))
#'
#' repkfold_ttest(data = tmp, n1 = 80, n2 = 20, k = 2, r = 2, tailed = "two")
#'

repkfold_ttest <- function(data, n1, n2, k, r, tailed = c("two", "one"), greater = NULL){

  # Arg checks

  tailed <- match.arg(tailed)
  '%ni%' <- Negate('%in%')

  if("model" %ni% colnames(data) || "values" %ni% colnames(data) || "k" %ni% colnames(data) || "r" %ni% colnames(data)){
    stop("data should contain at least four columns called 'model', 'values', 'k', and 'r'.")
  }

  if(!is.numeric(data$values) || !is.numeric(data$k) || !is.numeric(data$r)){
    stop("data should be a data.frame with only numerical values in columns 'values', 'k', and 'r'.")
  }

  if(!is.numeric(n1) || !is.numeric(n2) || !is.numeric(k) || !is.numeric(r) ||
     length(n1) != 1 || length(n2) != 1 || length(k) != 1 || length(r) != 1){
    stop("n1, n2, k, and r should all be integer scalars.")
  }

  if(length(unique(data$model)) != 2){
    stop("Column 'model' in data should only have two unique labels (one for each model to compare).")
  }

  if(tailed == "one"){
    if(!greater %in% unique(data$model)){
      stop("greater must correspond to a value in the 'model' column of data")
    }
  }

  #--------- Calculations ---------

  if(tailed == "two"){

    d <- c()

    for(i in 1:k){
      for(j in 1:r){
        x <- data[data$k == i, ]
        x <- x[x$r == j, ]
        d <- append(d, x[x$model == unique(x$model)[1], c("values")] - x[x$model == unique(x$model)[2], c("values")])
      }
    }
    if (sum(d) == 0) {
      tmp <- data.frame(statistic = 0, p.value = 1)
      return(tmp)
    } else{
      statistic <- mean(d, na.rm = TRUE) / sqrt(stats::var(d, na.rm = TRUE) * ((1 / (k * r)) + (n2 / n1)))
      p.value <- 2 * stats::pt(statistic, (k * r) - 1, lower.tail = FALSE)
    }

  } else{

    d <- c()

    for(i in 1:k){
      for(j in 1:r){
        x <- data[data$k == i, ]
        x <- x[x$r == j, ]
        d <- append(d, x[x$model == greater, c("values")] - x[x$model != greater, c("values")])
      }
    }

    if (sum(d) == 0) {
      tmp <- data.frame(statistic = 0, p.value = 1)
      return(tmp)
    } else{
      statistic <- mean(d, na.rm = TRUE) / sqrt(stats::var(d, na.rm = TRUE) * ((1 / (k * r)) + (n2 / n1)))
      p.value <- stats::pt(statistic, (k * r) - 1, lower.tail = FALSE)
    }
  }

  tmp <- data.frame(statistic = statistic, p.value = p.value)
  return(tmp)
}
