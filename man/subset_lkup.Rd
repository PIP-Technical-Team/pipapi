% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{subset_lkup}
\alias{subset_lkup}
\title{Subset look-up data}
\usage{
subset_lkup(country, year, welfare_type, reporting_level, lkup, valid_regions)
}
\arguments{
\item{country}{character: Country ISO 3 codes}

\item{year}{integer: Reporting year}

\item{welfare_type}{character: Welfare type}

\item{reporting_level}{character: Geographical reporting level}

\item{lkup}{list: A list of lkup tables}

\item{valid_regions}{character: List of valid region codes that can be used
for region selection}
}
\value{
data.frame
}
\description{
Subset look-up data
}
\keyword{internal}
