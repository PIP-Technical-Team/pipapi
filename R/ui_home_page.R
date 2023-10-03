#' Home Page Main Chart
#'
#' Provides numbers that will populate the home page main chart.
#'
#' @param povline numeric: Poverty line
#' @param lkup list: A list of lkup tables
#' @param lkup_hash character: hash of pip
#'
#' @return data.table
#' @export
ui_hp_stacked <- function(povline = 1.9,
                          lkup,
                          lkup_hash = lkup$cache_data_id$hash_pip_grp) {

  ref_years <- sort(unique(lkup$ref_lkup$reporting_year))
  ref_years <- ref_years[!ref_years %in% c(1981:1989, 2020, 2021)]

  out <- pip_grp(
    country = "all",
    year = ref_years,
    povline = povline,
    group_by = "wb",
    reporting_level = "national",
    censor = FALSE,
    lkup = lkup
  )

  out <- out[, c(
    "region_code", "reporting_year",
    "poverty_line", "pop_in_poverty"
  )]

  return(out)
}

#' Home Page Country Charts
#'
#' Provides numbers that will populate the home page country charts.
#'
#' @inheritParams pip
#' @param pop_units numeric: Units used to express population numbers (default
#'   to million)
#' @return data.table
#' @export
ui_hp_countries <- function(country = c("IDN", "CIV"),
                            povline = 1.9,
                            pop_units = 1e6,
                            lkup) {
  out <- pip(
    country = country,
    year = "all",
    povline = povline,
    lkup = lkup,
    fill_gaps = FALSE,
    group_by = NULL,
    reporting_level = "national"
  )

  # Add pop_in_poverty and scale according to pop_units
  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop <- out$reporting_pop / pop_units

  out <- out[, c(
    "region_code", "country_code", "reporting_year",
    "poverty_line", "reporting_pop", "pop_in_poverty"
  )]

  return(out)
}
