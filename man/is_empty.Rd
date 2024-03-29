% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_empty}
\alias{is_empty}
\title{Test whether a vector is length zero and IS not NULL}
\usage{
is_empty(x)
}
\arguments{
\item{x}{}
}
\value{
logical. TRUE if x is empty but it is not NULL
}
\description{
Test whether a vector is length zero and IS not NULL
}
\examples{
x <- vector()
is_empty(x)

y <- NULL
length(y)
is_empty(y)
}
