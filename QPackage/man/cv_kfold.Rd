% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/crossvalidation.R
\name{cv_kfold}
\alias{cv_kfold}
\title{K-fold cross validation}
\usage{
cv_kfold(feat_table, num_folds, method = NULL, ...)
}
\arguments{
\item{feat_table}{Feature vector (got from labeled data) represented as
qtable object.}

\item{num_folds}{Number of folds.}

\item{method}{Learning model to be applied represented as string. Currently
  seven learning models are supported  (see \code{\link{show_learners}}). The
  method should be one of the following: "svm", "rpart", "randomforest",
  "nnet", "bagging", "boosting", "naivebayes".}

\item{...}{Optional parameters relevant to learning model.}
}
\value{
Returns average accuracy computed from k-fold corssvalidation.
}
\description{
\code{cv_kfold} computes k-fold crossvalidation given feature vector(got from
labeled data) and learning model.
}
\examples{
\dontrun{
 acc_dt <- cv_kfold(feat_table, 10, method="rpart")
}
}

