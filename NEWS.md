# pipapi 0.0.1.9000

## New features

## Enhancements

- Increase maximum limit for `povline` parameter from 100 to 10 000
- Remove unnecessary columns from API response
- Add estimation_type and distribution_type to the API response when `fill_gaps=TRUE`
- Make sure latest data version is available as a specific version (not just as "latest_relase")

## Bug fixes

- Duplicates are no longer created when `fill_gaps=TRUE`
- Add `povline=NULL` option for /cp-key-indicators endpoint. This fixes an issue with the UI ingestion.
- Fix bug where distributional stats were incorrectly returned as missing for extrapolated surveys when `fill_gaps=TRUE`
- Add reporting_level to the output of `ui_cp_poverty_charts()`
- Make sure `ui_cp_poverty_charts()` only returns non-national observations when a country has no surveys with national coverage

# pipapi 0.0.1

Initial release of the API that powered the PIP soft-launch on February 9, 2022

# pipapi 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
