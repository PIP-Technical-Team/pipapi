# pipapi 0.0.1.9001

## New features

- Add /pip-grp as a new endpoint for aggregated statistics
- Soft deprecate `group_by` argument in /pip
- change parameter `version_length` for `vintage_pattern` in `extract_data_dirs()`. The algorithm for extracting valid versions has been modified. 
- Add asynchronous processing of slow API requests
 
## Enhancements

- Increase maximum limit for `povline` parameter from 100 to 10 000
- Remove unnecessary columns from API response
- Add estimation_type and distribution_type to the API response when `fill_gaps=TRUE`
- Use `pip_grp()` to calculate aggregated statistics in UI functions; `ui_hp_stacked()` and `ui_pc_regional()`
- Add unit tests for `pip_grp()`
- Improve filtering of data version directories in `create_versioned_lkups()` 
- Add country and region name to /pip response 
- Make sure latest data version is available as a specific version (not just as "latest_relase")
- use `{fs}` functions rather than base R's. 

## Bug fixes

- Duplicates are no longer created when `fill_gaps=TRUE`
- Add `povline=NULL` option for /cp-key-indicators endpoint. This fixes an issue with the UI ingestion.
- Fix bug where distributional stats were incorrectly returned as missing for extrapolated surveys when `fill_gaps=TRUE`
- Add reporting_level to the output of `ui_cp_poverty_charts()`
- Make sure `ui_cp_poverty_charts()` only returns non-national observations when a country has no surveys with national coverage
- Fix a bug in the application of censoring within `pip_grp()`
- Add a specific empty repsonse for `pip_grp()` to ensure that the response is consistent when no data is available
- Fix a bug in the selection of most recent value (`year="mrv"`) when `country="all"`

# pipapi 0.0.1

Initial release of the API that powered the PIP soft-launch on February 9, 2022

# pipapi 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
