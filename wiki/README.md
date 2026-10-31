# PIP API

<!-- cg:auto:overview -->
The PIP API provides a high-level interface to the computations and methods that power the Poverty and Inequality Platform (PIP). This R package and REST API enable programmatic access to poverty and inequality statistics, welfare distributions, and related metrics for countries and regions worldwide.

**Project**: PIP API  
**Team**: DECDG / GPID -- World Bank  
**Maintainer**: PIP-Technical-Team

The package is designed to deliver:
- **R package**: Native R functions for poverty and inequality computations
- **REST API**: HTTP endpoints for remote access to PIP data and methods
- **Access to Poverty Global numbers**: Retrieve and analyze global poverty statistics

All code is maintained with strict test coverage and optimized for performance.
<!-- cg:auto:end -->

<!-- cg:auto:installation -->
## Installation

### Requirements
- R >= 4.0.0
- Dependencies: data.table, collapse, plumber, duckdb, wbpip

### From GitHub
```r
# Install from GitHub (development version)
# install.packages("remotes")
remotes::install_github("PIP-Technical-Team/pipapi")
```

### Dependencies
The package depends on:
- **data.table**: Fast data manipulation
- **collapse**: Advanced statistical functions
- **plumber**: REST API framework
- **duckdb**: Embedded analytical database
- **wbpip**: World Bank PIP welfare computations
<!-- cg:auto:end -->

<!-- cg:auto:quick-start -->
## Quick Start

### Using the R Package
```r
library(pipapi)

# Example: Access poverty statistics
# (Add actual function examples once API is stable)
```

### Running the REST API
```r
# Start the Plumber API server
library(pipapi)
# Run API server
# (Add actual API startup code once implemented)
```

### Testing
The package uses testthat for testing. All tests should pass before commits:
```r
# Run tests
testthat::test_local()
```
<!-- cg:auto:end -->

## Contents
- [API Reference](api-reference.md)
- [Vignettes](vignettes.md)
- [Changelog](changelog.md)
