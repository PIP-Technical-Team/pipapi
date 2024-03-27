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
                         group_by = "none",
                         welfare_type = c("all", "consumption", "income"),
                         reporting_level = c("all", "national", "rural", "urban"),
                         pop_units = 1e6,
                         lkup) {
  # Set returned columns
  return_cols           <- lkup$return_cols$ui_pc_charts$cols
  inequality_indicators <- lkup$return_cols$ui_pc_charts$inequality_indicators

  group_by         <- match.arg(group_by)
  welfare_type     <- match.arg(welfare_type)
  reporting_level  <- match.arg(reporting_level)

  out <- pip(
    country         = country,
    year            = year,
    povline         = povline,
    fill_gaps       = fill_gaps,
    group_by        = group_by,
    reporting_level = reporting_level,
    lkup            = lkup
  )

  # Add pop_in_poverty and scale according to pop_units
  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop <- out$reporting_pop / pop_units

  # handle different responses when fill_gaps = TRUE / FALSE
  if (fill_gaps == FALSE) {
    # Return all columns when survey years are requested
    out <- out[, .SD, .SDcols = return_cols]
    return(out)

  } else {
    # Set non-interpolated variables to NA if line-up years are requested
    out <- out[, .SD, .SDcols = return_cols]
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
