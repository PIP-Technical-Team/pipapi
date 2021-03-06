% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pip_version.R
\name{get_pip_version}
\alias{get_pip_version}
\title{Return the versions of the pip packages used for computations}
\usage{
get_pip_version(
  pip_packages = c("pipapi", "wbpip"),
  data_folder_root,
  valid_params
)
}
\arguments{
\item{pip_packages}{character: Custom packages powering the API}

\item{data_folder_root}{character: Root path of data folder}

\item{valid_params}{list: Valid API query parameters}
}
\value{
list
}
\description{
Return the versions of the pip packages used for computations
}
