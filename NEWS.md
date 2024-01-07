# pipapi (development version)
## New features
- Add new SPR and SPL indicators
- Add new endpoint `grouped-stats` to return grouped data

## Enhancements
- Address some linting issues
- Increase test coverage

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
