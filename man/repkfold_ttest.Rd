% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repkfold_ttest.R
\name{repkfold_ttest}
\alias{repkfold_ttest}
\title{Compute correlated t-statistic and p-value for repeated k-fold cross-validated results}
\usage{
repkfold_ttest(data, n1, n2, k, r)
}
\arguments{
\item{data}{\code{data.frame} of values for model A and model B over repeated k-fold cross-validation. Four named columns are expected: \code{"model"}, \code{"values"}, \code{"k"}, and \code{"k"}}

\item{n1}{\code{integer} denoting train set size}

\item{n2}{\code{integer} denoting test set size}

\item{k}{\code{integer} denoting number of folds used in k-fold}

\item{r}{\code{integer} denoting number of repeats per fold}
}
\value{
object of class \code{data.frame}
}
\description{
Compute correlated t-statistic and p-value for repeated k-fold cross-validated results
}
\references{
Nadeau, C., and Bengio, Y. Inference for the Generalization Error. Machine Learning 52, (2003).

Bouckaert, R. R., and Frank, E. Evaluating the Replicability of Significance Tests for Comparing Learning Algorithms. Advances in Knowledge Discovery and Data Mining. PAKDD 2004. Lecture Notes in Computer Science, 3056, (2004).
}
\author{
Trent Henderson
}
