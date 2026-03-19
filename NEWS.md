# pipapi 1.5.2

* Temporarily block `national_poverty_lines` from the public API via the new
  `blocked_aux_tables()` function. Direct R calls to `get_aux_table()` are
  unaffected. Edit `R/blocked_aux_tables.R` to add or remove blocked tables.

# pipapi 1.5.0

# pipapi 1.4.2
* fix bugs
* add more safeties to plumber and better documentation of errors
* add `safe_endpoint()` to critical endpoints.
* apply new way normalize arguments with `normalize_args()`
* add multiple options to `group_by` parameter

# pipapi 1.4.1
* Removed old regions aggregate from `pc_chart`.
* Removed old regions aggregate from `get_aux_table_ui`.
* Bring back MDG.
* Add hash to all UI enpdoints

# pipapi 1.4.0
* Added `safe_endpoint` function for safer endpoint handling.
* Removed `preroute` as it is now managed in `endpoints.R`.
* Introduced a new and improved Plumber setup.
* Refactored Plumber configuration for better maintainability.
* Implemented a timeout wrapper for more endpoints.
* Added "Request timed out" responses to `pip` and `pip-grp` endpoints.
* Introduced a lightweight request ID and timing filter for better request tracking.
* Added `tryCatch` blocks to improve error handling in several endpoints.
* Enhanced error handling throughout the API.
* Fixed bug by changing `req$args` to `req$argsQuery`.
* Removed unnecessary calls to `gc()`.
* Improved filtering of poverty lines when updating the master (intermediate cache) file.
* Utilized the missing data file instead of recreating it.
* Removed verbose output in join operations.
* Ensured correct sorting for surveys with multiple `reporting_level` values.
* Updated handling of empty data responses.
* Enabled support for multiple `popshare` values in fill gaps operations.
* Removed `filter_Lkup` from `fg_pip()`.
* Allowed region selection from any variable (to be revised when supporting more aggregations).
* Added prosperity gap metric to `ui_cp_charts`.
* Enabled multiple `popshare` values for survey years.
* Improved the `infer_poverty_line()` function.
* Updated data documentation for clarity and completeness.
* Stored variables in the environment defined in `zzz`.
* Fixed several bugs across the codebase.


# pipapi 1.3.24
* Incorporate Lineup distribution and Countries with Missing Data (CMD) distributions
* New way to estimate poverty using cumulative sums
* allow multiple `popshare`


# pipapi 1.3.23
* Add Venn diagram information to Country profiles
* Add prosperity gap to country profiles chart.


# pipapi 1.3.22

* Fix issue with popshare in fill gaps calls.

# pipapi 1.3.21
* Fix issue with popshare in survey year calls
* Fix problem with alternative aggregates like AFW and AFE
* Make sure all tests pass
* add logs in docker container

# pipapi 1.3.20

# pipapi 1.3.19
* fix issue with comparability

# pipapi 1.3.18
* Fix country profile data
* Add UKR
* fix some deletion of column by reference

# pipapi 1.3.17
* Fix bugs
* improve matching algorithm

# pipapi 1.3.16

* Fix algorithm loading duckdb code.
* Vectorization of pov line working now

# pipapi 1.3.15

* vectorization of poverty line. Just in internal functions. They are still not available in the API
* Fix issue with aggregates.

# pipapi 1.3.14

* Fix issues with intermediate caching when multiple R sessions are open
* Optimize opening and closing of duckdb connection. 

# pipapi 1.3.13

* implement intermediate caching

# pipapi 1.3.12

* Hot fix with `future` dependency.
* Fix examples for `/grouped-stats` endpoint. 
* cache-control header added for test

# pipapi 1.3.11

* fix bug for not including `key_values` into Lorenz quadratic function.


# pipapi 1.3.10

* update esimate_type filter for fillgaps at cl

# pipapi 1.3.9
* remove censoring and apply filter at the UI level.


# pipapi 1.3.8

* Add Prosperity Gap to both svy and lnp years. 
* Implement nowcast up to 2024
* Add `estimate_type` variable to `pip()` and `pip_grp*()` calls
* Add new aux file, `metaregion`
* Change algorithms for MRV calls to return up to lineup year. 
* Modify unit tests to account for the changes above.

* Fix distribution_type variables for both svy and lnp years. 
* improve algorithm of add_*() functions
* Fix aggregation of medians
* Update empty responses. 
* add end point `wld-lineup-year` to return nowcast year and tooltip text

# pipapi 1.3.7
remove censoring of official regions to account for cases like GNQ in which they are not counted as part of the total population of AFE and AFW


# pipapi 1.3.6

# pipapi 1.3.5

# pipapi 1.3.4
## New features
- Add new SPR and SPL indicators
- Add new endpoint `grouped-stats` to return grouped data

## Enhancements
- Address some linting issues
- Increase test coverage
- Improve caching for `ui_cp_ki_headcount` and `ui_cp_poverty_charts`

# pipapi 1.3.3
- Fix bug with ag_average_poverty_stats
- Better control of returned columns
- Fix bug that was causing wrong aggregates to be returned in some instances

# pipapi 1.3.2

# pipapi 1.3.1

# pipapi 1.3.0

# pipapi 1.2.1

## New features
- New indicators available. Optional with `additional_ind = TRUE`
- /citation endpoint now returns additional information: `version_id` and `accessed_date`
- Limit accepted poverty lines to 3 decimals
- Add new /version parameter that returns information about a specific data version
- Povline is now limited to a maximum value of $2700 PPP (daily value)
- `/aux` endpoints now return tables in long_format by default
- A new `/ui_aux` endpoint has been created with the opposite behavior: tables are
returned in wide format by default.
- `/pip-info` returns additional information
- New "etag" and "max-age" headers returned by the API to facilitate caching of API responses

## Enhancements
- pip-grp and cp-chart endpoint small optimization
- support to arrow/feather serialization with `format=arrow`
- [Implement checks for long_format parameter in API filter](https://github.com/PIP-Technical-Team/pipapi/pull/332)

## Bug fixes
- Hot fix to handle failing of `create_vector_countries()` when `country="ALL"`
- Fix cp-download
- Fix creation of duplicated responses for some regional aggregates
- Fix year selection for /pip-grp

# pipapi 1.2.0

## New features
- [Add new endpoint to support country profile downloads](https://github.com/PIP-Technical-Team/pipapi/pull/283)
- [Add separate arguments to select release and ppp version](https://github.com/PIP-Technical-Team/pipapi/issues/265)
- [Allow direct selection of non-official WB regions](https://github.com/PIP-Technical-Team/pipapi/pull/287)
- [Add new /citation endpoint](https://github.com/PIP-Technical-Team/pipapi/pull/302)
 
## Enhancements
- [Disable popshare option for pip_grp and aggregate distributions](https://github.com/PIP-Technical-Team/pipapi/pull/274)
- [Allow /aux endpoint to return data in long format](https://github.com/PIP-Technical-Team/pipapi/issues/272)
- [Make default poverty line dynamic in API](https://github.com/PIP-Technical-Team/pipapi/pull/310)

# pipapi 1.1.0

## Enhancements
- [Sort the rows by `country_code` and `reporting_year` in the pip response.](https://github.com/PIP-Technical-Team/pipapi/issues/248)
- [Fix casing making pip call case insensitive](https://github.com/PIP-Technical-Team/pipapi/issues/120)
- [Add unit tests for newly created fg_remove_duplicates() and sub-functions](https://github.com/PIP-Technical-Team/pipapi/issues/226)
- [auto convert parameters to their respective types in `parse_parameters`](https://github.com/PIP-Technical-Team/pipapi/issues/241)
- [Sanitize user inputs in get_aux_table](https://github.com/PIP-Technical-Team/pipapi/issues/259)
- Removed `purrr` dependency
- [Better error message when passing more than one dataset as `lkup` in `pip` call](https://github.com/PIP-Technical-Team/pipapi/issues/263)

## New features
- Region codes can now be passed directly to the `country` query parameter to 
return all countries pertaining to the specified region
- the `/valid-params` endpoint gains an `endpoint` parameter that allows to only
return parameters that are relevant to the specified endpoint
- [Add /valid-years endpoint that returns available years for both survey and 
interpolated years](https://github.com/PIP-Technical-Team/pipapi/issues/182)
- [Add direct selection for WLD aggregate](https://github.com/PIP-Technical-Team/pipapi/pull/268)

# pipapi 1.0.0

## New features

- Add /pip-grp as a new endpoint for aggregated statistics
- Soft deprecate `group_by` argument in /pip
- Change parameter `version_length` for `vintage_pattern` in `extract_data_dirs()`. The algorithm for extracting valid versions has been modified. 
- Add option to enable disk based caching of `pip()` and `pip_grp()`
- Add asynchronous processing of slow API requests
 
## Enhancements

- Increase maximum limit for `povline` parameter from 100 to 10 000
- Remove unnecessary columns from API response
- Add estimation_type and distribution_type to the API response when `fill_gaps=TRUE`
- Use `pip_grp()` to calculate aggregated statistics in UI functions; `ui_hp_stacked()` and `ui_pc_regional()`
- Add unit tests for `pip_grp()`
- Improve filtering of data version directories in `create_versioned_lkups()` 
- Add country and region name to /pip response 
- Make sure latest data version is available as a specific version (not just as "latest_release")
- Use `{fs}` functions rather than base R's. 

## Bug fixes

- Duplicates are no longer created when `fill_gaps=TRUE`
- Add `povline=NULL` option for /cp-key-indicators endpoint. This fixes an issue with the UI ingestion.
- Fix bug where distributional stats were incorrectly returned as missing for extrapolated surveys when `fill_gaps=TRUE`
- Add reporting_level to the output of `ui_cp_poverty_charts()`
- Make sure `ui_cp_poverty_charts()` only returns non-national observations when a country has no surveys with national coverage
- Fix a bug in the application of censoring within `pip_grp()`
- Add a specific empty response for `pip_grp()` to ensure that the response is consistent when no data is available
- Fix a bug in the selection of most recent value (`year="MRV"`) when `country="ALL"`

# pipapi 0.0.1

Initial release of the API that powered the PIP soft-launch on February 9, 2022

# pipapi 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
