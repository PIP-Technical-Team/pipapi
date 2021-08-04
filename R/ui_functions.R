#' Provides numbers that will populate the home page main chart
#'
#' @param povline numeric: Poverty line
#' @param lkup list: A list of lkup tables
#'
#' @return data.frame
#' @export
#'
ui_hp_stacked <- function(povline = 1.9,
                          lkup) {
  out <- pip(country      = "all",
             year         = "all",
             povline      = povline,
             lkup         = lkup,
             fill_gaps    = TRUE,
             aggregate    = TRUE,
             group_by     = "wb",
             reporting_level = "national")

  out <- out[, .(region_code, reporting_year,
                 poverty_line, pop_in_poverty)]
  return(out)
}

#' Provides numbers that will populate the home page country charts
#'
#' @param country character: Country code
#' @param povline numeric: Poverty line
#' @param pop_units numeric: Units used to express population numbers (default to million)
#' @param lkup list: A list of lkup tables
#'
#' @return data.frame
#' @export
#'
ui_hp_countries <- function(country = c("AGO", "CIV"),
                            povline = 1.9,
                            pop_units = 1e6,
                            lkup) {
  out <- pip(country      = country,
             year         = "all",
             povline      = povline,
             lkup         = lkup,
             fill_gaps    = FALSE,
             aggregate    = FALSE,
             group_by     = NULL,
             reporting_level = "national")

  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop  <- out$reporting_pop / pop_units

  out <- out[, .(pcn_region_code, country_code, reporting_year,
                 poverty_line, reporting_pop, pop_in_poverty)]
  out <- out %>%
    data.table::setnames('pcn_region_code', 'region_code')

  return(out)
}

#' Provides numbers that will populate the poverty calculator main chart
#'
#' @param country character: Country code
#' @param year numeric: reporting year
#' @param povline numeric: Poverty line
#' @param fill_gaps logical: Whether to impute missing values (TRUE) or not (FALSE)
#' @param aggregate logical: Whether to aggregate results (TRUE) or not (FALSE)
#' @param group_by character: Subgroups to aggregate by
#' @param welfare_type character: Welfare type
#' @param reporting_level character: Reporting level
#' @param lkup list: A list of lkup tables
#'
#' @return data.frame
#' @export
#'
ui_pc_charts <- function(country = c("AGO"),
                       year    = "all",
                       povline = 1.9,
                       fill_gaps = FALSE,
                       aggregate = FALSE,
                       group_by = c("none", "wb"),
                       welfare_type = c("all", "consumption", "income"),
                       reporting_level = c("all", "national", "rural", "urban"),
                       lkup) {

  out <- pip(country      = country,
             year         = year,
             povline      = povline,
             fill_gaps    = fill_gaps,
             aggregate    = aggregate,
             group_by     = group_by,
             reporting_level = reporting_level,
             lkup         = lkup,)

  if (group_by != "none") {
    return(out)
  }  else if (fill_gaps == FALSE) {
    out <- out[, .(country_code, reporting_year, welfare_type,
                   pop_data_level, median, gini, polarization,
                   mld, decile1, decile2, decile3, decile4, decile5,
                   decile6, decile7, decile8, decile9, decile10,
                   pcn_region_code, survey_coverage, survey_comparability,
                   comparable_spell, survey_year, survey_mean_lcu, survey_mean_ppp,
                   reporting_pop, ppp, cpi, distribution_type,
                   is_interpolated, poverty_line, mean, headcount,
                   poverty_gap, poverty_severity, watts)]
    out <- out %>%
      data.table::setnames('pcn_region_code', 'region_code')
    return(out)
  } else {
    out <- out[, .(country_code, reporting_year, poverty_line, mean,
                   headcount, poverty_gap, poverty_severity, watts,
                   pcn_region_code, reporting_pop, is_interpolated)]
    out <- out %>%
      data.table::setnames('pcn_region_code', 'region_code')
    return(out)
  }

}
