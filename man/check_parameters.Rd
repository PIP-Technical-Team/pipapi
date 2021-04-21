% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-plumber.R
\name{check_parameters}
\alias{check_parameters}
\title{check_parameters
Check whether a parsed parameter is valid or not}
\usage{
check_parameters(req, res, param, valid_values)
}
\arguments{
\item{req}{env: plumber request environment}

\item{res}{evn: plumber response environment}

\item{param}{character or numeric: Parsed parameters}

\item{valid_values}{character or numeric: Accepted values for this parameter}
}
\value{
logical
}
\description{
check_parameters
Check whether a parsed parameter is valid or not
}