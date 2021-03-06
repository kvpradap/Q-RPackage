% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/sample-data.R
\name{sample_qtable}
\alias{sample_qtable}
\title{Sample qtable}
\usage{
sample_qtable(table_obj, num_samples, replace = FALSE)
}
\arguments{
\item{table_obj}{qtable object}

\item{num_samples}{a positive number, the number of items to choose. Note:
currently only random sampling is supported}

\item{replace}{boolean to do sampling with or without replacement.}
}
\value{
qtable with subset of rows of size \code{num_samples} from input
  table
}
\description{
\code{sample_table} It takes a sample of specified size from the qtable using
either with or without replacement.
}
\examples{
\dontrun{
sampled_qtable <- sample_qtable(qtable_obj, 25)
}
}

