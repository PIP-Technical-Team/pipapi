
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipapi

<!-- badges: start -->
<!-- [![Codecov test coverage](https://codecov.io/gh/PIP-Technical-Team/pipapi/branch/master/graph/badge.svg)](https://codecov.io/gh/PIP-Technical-Team/pipapi?branch=master) -->

[![R-CMD-check](https://github.com/PIP-Technical-Team/pipapi/workflows/R-CMD-check/badge.svg)](https://github.com/PIP-Technical-Team/pipapi/actions?workflow=R-CMD-check)
[![test-coverage](https://github.com/PIP-Technical-Team/pipapi/workflows/test-coverage/badge.svg)](https://github.com/PIP-Technical-Team/pipapi/actions)
[![pkgdown](https://github.com/PIP-Technical-Team/pipapi/workflows/pkgdown/badge.svg)](https://github.com/PIP-Technical-Team/pipapi/actions)
<!-- badges: end -->

The goal of pipapi is to provide a high level API to the computations
and methods that power the Poverty and Inequality Platform (PIP).

World Bank staff who have read access to the PIP data can use the
functions from this package directly, without hitting the PIP API.

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

In order to use `pip()` you’ll need to have access to a PIP
`data_folder`. The folder structure looks like this:

``` bash
data-folder-root/
├─ _aux/
│  ├─ pop_regions.fst
│  ├─ pop.fst
│  ├─ ...
├─ estimations/
│  ├─ prod_svy_estimation.fst
│  ├─ prod_ref_estimation.fst
├─ survey_data/
│  ├─ survey_1.fst
│  ├─ ...
│  ├─ survey_n.fst
```

``` r
# Create a list of look-up tables from the root data folder
lkups <- pipapi:::clean_api_data(
  data_folder_root = "path/to/data_folder")
```

### Usage

Pass the `lkups` list to the main `pip()` function to compute poverty
and inequality statistics in your `R` session.

``` r
library(pipapi)

pip(country = "AGO",
    year = 2000,
    povline = 1.9,
    lkup = lkups)
#>    region_code country_code reporting_year survey_acronym survey_coverage
#> 1:         SSA          AGO           2000            HBS        national
#>    survey_year welfare_type survey_comparability comparable_spell poverty_line
#> 1:     2000.21  consumption                    0             2000          1.9
#>    headcount poverty_gap poverty_severity     mean   median       mld      gini
#> 1: 0.3637448   0.1636806       0.09982393 4.100014 2.593394 0.5125765 0.5195689
#>    polarization     watts    decile1    decile2    decile3    decile4
#> 1:    0.4643401 0.2811239 0.00983246 0.02195307 0.03342455 0.04495307
#>       decile5    decile6    decile7   decile8  decile9  decile10
#> 1: 0.05662774 0.07048758 0.08808485 0.1134946 0.158687 0.4024552
#>    survey_mean_lcu survey_mean_ppp predicted_mean_ppp        cpi cpi_data_level
#> 1:        11.23264        4.100014                 NA 0.03385145       national
#>        ppp ppp_data_level reporting_pop pop_data_level reporting_gdp
#> 1: 80.9318       national      16395473       national      2195.631
#>    gdp_data_level reporting_pce pce_data_level is_interpolated
#> 1:       national            NA       national           FALSE
#>    is_used_for_aggregation distribution_type estimation_type
#> 1:                   FALSE             micro          survey
```
