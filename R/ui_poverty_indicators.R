#' Poverty Calculator Main chart
#'
#' Provides numbers that will populate the poverty calculator main chart.
#'
#' @inheritParams pip
#' @inheritParams ui_hp_countries
#' @return data.table
#' @export
ui_pc_charts <- function(country = c("AGO"),
                         year = "all",
                         povline = 1.9,
                         fill_gaps = FALSE,
                         group_by = c("none", "wb"),
                         welfare_type = c("all", "consumption", "income"),
                         reporting_level = c("all", "national", "rural", "urban"),
                         pop_units = 1e6,
                         lkup) {
  group_by <- match.arg(group_by)

  out <- pip(
    country = country,
    year = year,
    povline = povline,
    fill_gaps = fill_gaps,
    group_by = group_by,
    reporting_level = reporting_level,
    lkup = lkup
  )

  # Add pop_in_poverty and scale according to pop_units
  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop <- out$reporting_pop / pop_units

  # DO WE NEED THIS CODE CHUNK?
  if (group_by != "none") {
    return(out)
  } else if (fill_gaps == FALSE) {
    out <- out[, c(
      'country_code', 'reporting_year', 'welfare_type',
      'reporting_level', 'median', 'gini', 'polarization',
      'mld', 'decile1', 'decile2', 'decile3', 'decile4', 'decile5',
      'decile6', 'decile7', 'decile8', 'decile9', 'decile10',
      'region_code', 'survey_coverage', 'survey_comparability',
      'comparable_spell', 'survey_year',
      'reporting_pop', 'ppp', 'cpi', 'distribution_type',
      'is_interpolated', 'poverty_line', 'mean', 'headcount',
      'poverty_gap', 'poverty_severity', 'watts', 'pop_in_poverty', 'spr'
    )]
    return(out)
  } else {
    # out <- out[, c(
    #   "country_code", "reporting_year", "poverty_line", "mean",
    #   "headcount", "poverty_gap", "poverty_severity", "watts",
    #   "region_code", "reporting_pop", "is_interpolated",
    #   "pop_in_poverty"
    # )]

    out <- out[, c(
      'country_code', 'reporting_year', 'welfare_type',
      'reporting_level', 'median', 'gini', 'polarization',
      'mld', 'decile1', 'decile2', 'decile3', 'decile4', 'decile5',
      'decile6', 'decile7', 'decile8', 'decile9', 'decile10',
      'region_code', 'survey_coverage', 'survey_comparability',
      'comparable_spell', 'survey_year',
      'reporting_pop', 'ppp', 'cpi', 'distribution_type',
      'is_interpolated', 'poverty_line', 'mean', 'headcount',
      'poverty_gap', 'poverty_severity', 'watts', 'pop_in_poverty', 'spr'
    )]

    inequality_indicators <- c('median', 'gini', 'polarization',
                               'mld', 'decile1', 'decile2', 'decile3', 'decile4', 'decile5',
                               'decile6', 'decile7', 'decile8', 'decile9', 'decile10')

    out[, inequality_indicators] <- NA

    return(out)
  }
}

#' Poverty Calculator regional aggregates
#'
#' Provides numbers that will populate poverty calculator regional aggregates
#' for all years.
#'
#' @inheritParams ui_pc_charts
#' @return data.table
#' @export
ui_pc_regional <- function(country   = "ALL",
                           year      = "ALL",
                           povline   = 1.9,
                           pop_units = 1e6,
                           lkup) {

  # TEMPORARY UNTIL SELECTION MECHANISM IS BEING IMPROVED
  country <- toupper(country)
  if (is.character(year)) {
    year <- toupper(year)
  }

  out <- pip_grp_logic(country         = country,
                       year            = year,
                       group_by        = "wb",
                       reporting_level = "national",
                       povline         = povline,
                       lkup            = lkup,
                       censor          = TRUE)

  # Add pop_in_poverty and scale according to pop_units
  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop <- out$reporting_pop / pop_units

  return(out)
}
