#' @keywords internal
#' @aliases pipapi-package
"_PACKAGE"

#' List of two datasets `pov_trend` and `pov_mrv`
#'
#' @docType data
#' @keywords datasets
#' @name empty_response_cp_poverty
#' @usage data(empty_response_cp_poverty)
#' @format A list with 2 dataframes
NULL

#' Dataframe for grouped empty response
#'
#' @docType data
#' @keywords datasets
#' @name empty_response_grp
#' @usage data(empty_response_grp)
#' @format Data frame with 0 rows and 12 columns
NULL


#' List of lookup values
#'
#' @docType data
#' @keywords datasets
#' @name lkup
#' @usage data(lkup)
#' @format A list of lookup values
NULL


# data.table is generally careful to minimize the scope for namespace
# conflicts (i.e., functions with the same name as in other packages);
# a more conservative approach using @importFrom should be careful to
# import any needed data.table special symbols as well, e.g., if you
# run DT[ , .N, by='grp'] in your package, you'll need to add
# @importFrom data.table .N to prevent the NOTE from R CMD check.
# See ?data.table::`special-symbols` for the list of such symbols
# data.table defines; see the 'Importing data.table' vignette for more
# advice (vignette('datatable-importing', 'data.table')).
#
#' @rawNamespace import(collapse, except = fdroplevels)
#' @rawNamespace import(data.table, except = fdroplevels)
#' @importFrom utils head tail
NULL



utils::globalVariables(
  c(
    ".",
    "cache_id",
    "country_code",
    "cpi",
    "decile1",
    "decile10",
    "decile2",
    "decile3",
    "decile4",
    "decile5",
    "decile6",
    "decile7",
    "decile8",
    "decile9",
    "distribution_type",
    "gini",
    "headcount",
    "interpolation_id",
    "is_interpolated",
    "median",
    "mld",
    "polarization",
    "pop",
    "reporting_level",
    "pop_in_poverty",
    "poverty_gap",
    "poverty_line",
    "poverty_severity",
    "ppp",
    "region_code",
    "reporting_pop",
    "reporting_year",
    "survey_comparability",
    "survey_coverage",
    "survey_mean_lcu",
    "survey_mean_ppp",
    "survey_year",
    "watts",
    "wb_region_code",
    "weighted.mean",
    "welfare_type",
    "pcn_region_code",
    "comparable_spell",
    "..cols",
    "N",
    "check",
    "data_interpolation_id",
    "display_cp",
    "region_name",
    "sessionInfo",
    "bottom40",
    "max_year",
    "headcount_national",
    "area",
    "empty_response",
    "estimate_type",
    "estimation_type",
    "i.distribution_type",
    "i.lineup_year",
    "i.median",
    "i.pg",
    "i.spl",
    "i.spr",
    "id",
    "lineup_year",
    "statistic",
    "survey_acronym",
    "surveyid_year",
    "tmp_id",
    "to_remove",
    "uniq_dist",
    "unique_replevel",
    "use_bin",
    "use_groupdata",
    "use_imputed",
    "use_microdata",
    "path"
  )
)
