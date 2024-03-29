% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{select_reporting_level}
\alias{select_reporting_level}
\title{helper function to correctly filter look up table according to requested
reporting level}
\usage{
select_reporting_level(lkup, keep, reporting_level)
}
\arguments{
\item{lkup}{data.table: Main lookup table}

\item{keep}{logical: Logical vector of rows to be kept}

\item{reporting_level}{character: Requested reporting level}
}
\value{
data.table
}
\description{
helper function to correctly filter look up table according to requested
reporting level
}
