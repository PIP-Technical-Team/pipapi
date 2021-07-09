
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipapi

<!-- badges: start -->
<!-- [![Codecov test coverage](https://codecov.io/gh/PIP-Technical-Team/pipapi/branch/master/graph/badge.svg)](https://codecov.io/gh/PIP-Technical-Team/pipapi?branch=master) -->
<!-- badges: end -->

The goal of pipapi is to provide a high level API to the computations
and methods that power the Poverty and Inequality Platform (PIP).

World Bank staff who have read access to the PIP data can use the
functions from this package directly (without hitting the PIP API)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/pipapi")
```

## Getting started

The main function from the `pipapi` package is the `pip()` function. See
`?pip` for more information.

### Data access

In order to use the `pip()` you’ll need to have access to PIP
`data_folder`. The expected structure for the PIP `data_folder` is the
following: data\_folder \|\_aux \|estimations \|survey\_data

### Basic example

``` r
library(pipapi)
# STEP 1: Connect to data_folder
lkups <- pipapi:::clean_api_data(data_folder_root = "path/to/pip/data_folder")
pip(country = "AGO",
    year    = 2008,
    povline = 1.9,
    fill_gaps = FALSE,
    aggregate = FALSE,
    reporting_level = "national",
    lkup = lkups)
```
