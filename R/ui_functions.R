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
             group_by     = TRUE,
             svy_coverage = "national")

  out <- out[, .(region_code, reporting_year, poverty_line, pop_in_poverty)]
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
             svy_coverage = "national")

  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop  <- out$reporting_pop / pop_units

  out <- out[, .(country_code, reporting_year, poverty_line, reporting_pop,
                 pop_in_poverty)]
  return(out)
}

