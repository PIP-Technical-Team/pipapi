% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui_functions.R
\name{ui_pc_charts}
\alias{ui_pc_charts}
\title{Provides numbers that will populate the poverty calculator main chart}
\usage{
ui_pc_charts(
  country = c("AGO"),
  year = "all",
  povline = 1.9,
  fill_gaps = FALSE,
  aggregate = FALSE,
  group_by = c("none", "wb"),
  welfare_type = c("all", "consumption", "income"),
  reporting_level = c("all", "national", "rural", "urban"),
  lkup
)
}
\arguments{
\item{country}{character: Country code}

\item{year}{numeric: reporting year}

\item{povline}{numeric: Poverty line}

\item{fill_gaps}{logical: Whether to impute missing values (TRUE) or not (FALSE)}

\item{aggregate}{logical: Whether to aggregate results (TRUE) or not (FALSE)}

\item{group_by}{character: Subgroups to aggregate by}

\item{welfare_type}{character: Welfare type}

\item{reporting_level}{character: Reporting level}

\item{lkup}{list: A list of lkup tables}
}
\value{
data.frame
}
\description{
Provides numbers that will populate the poverty calculator main chart
}
