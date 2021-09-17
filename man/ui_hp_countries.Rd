% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui_functions.R
\name{ui_hp_countries}
\alias{ui_hp_countries}
\title{Home Page Country Charts}
\usage{
ui_hp_countries(
  country = c("AGO", "CIV"),
  povline = 1.9,
  pop_units = 1e+06,
  lkup
)
}
\arguments{
\item{country}{character: Country ISO 3 codes}

\item{povline}{numeric: Poverty line}

\item{pop_units}{numeric: Units used to express population numbers (default
to million)}

\item{lkup}{list: A list of lkup tables}
}
\value{
data.table
}
\description{
Provides numbers that will populate the home page country charts.
}