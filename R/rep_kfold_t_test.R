#' Compute correlated t-statistic and p-value for repeated k-fold cross-validated results
#' @importFrom stats var pt
#' @param data \code{data.frame} of values for model A and model B over repeated k-fold cross-validation. Three named columns are expected:
#' @param n1 \code{integer} denoting train set size
#' @param n2 \code{integer} denoting test set size
#' @param k \code{integer} denoting number of folds used in k-fold
#' @param r \code{integer} denoting number of repeats per fold
#' @return object of class \code{data.frame}
#' @references Nadeau, C., and Bengio, Y. Inference for the Generalization Error. Machine Learning 52, (2003).
#' @references Bouckaert, R. R., and Frank, E. Evaluating the Replicability of Significance Tests for Comparing Learning Algorithms. Advances in Knowledge Discovery and Data Mining. PAKDD 2004. Lecture Notes in Computer Science, 3056, (2004).
#' @author Trent Henderson
#' @export
#'

repkfold.t.test <- function(data, n1, n2, k, r){

  # Arg checks

  '%ni%' <- Negate('%in%')

  if("model" %ni% colnames(data)){
    stop("data should contain at least four columns called 'model', 'values', 'k', and 'r'.")
  }

  if("values" %ni% colnames(data)){
    stop("data should contain at least four columns called 'model', 'values', 'k', and 'r'.")
  }

  if("k" %ni% colnames(data)){
    stop("data should contain at least four columns called 'model', 'values', 'k', and 'r'.")
  }

  if("r" %ni% colnames(data)){
    stop("data should contain at least four columns called 'model', 'values', 'k', and 'r'.")
  }

  if(!is.numeric(data$values) || !is.numeric(data$k) || !is.numeric(data$r)){
    stop("data should be a data.frame with only numerical values in columns 'values', 'k', and 'r'.")
  }

  if(!is.numeric(n1) || !is.numeric(n2) || !is.numeric(k) || !is.numeric(r) ||
     length(n1) != 1 || length(n2) != 1 || length(k) != 1 || length(r) != 1){
    stop("n1, n2, k, and r should all be integer scalars.")
  }

  if(length(unique(tmp$model)) != 2){
    stop("Column 'model' in data should only have two unique labels (one for each model to compare).")
  }

  # Calculations

  d <- c()

  for(i in 1:k){
    for(j in 1:r){
      x <- data[data$k == i, ]
      x <- x[x$r == j, ]
      d <- c(d, x[x$model == unique(x$model)[1], c("values")] - x[x$model == unique(x$model)[2], c("values")]) # Differences
    }
  }

  statistic <- mean(d, na.rm = TRUE) / sqrt(stats::var(d, na.rm = TRUE) * ((1/(k * r)) + (n2/n1))) # Calculate t-statistic

  if(statistic < 0){
    p.value <- stats::pt(statistic, (k * r) - 1) # p-value for left tail
  } else{
    p.value <- stats::pt(statistic, (k * r) - 1, lower.tail = FALSE) # p-value for right tail
  }

  tmp <- data.frame(statistic = statistic, p.value = p.value)
  return(tmp)
}
