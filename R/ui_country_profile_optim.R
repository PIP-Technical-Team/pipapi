#' Country Profiles Key Indicators
#'
#' Provides numbers that will population for country profiles key indicators.
#'
#' @inheritParams pip
#' @return list
#' @export
ui_cp_key_indicators_optim <- function(country   = "AGO",
                                       povline   = NULL,
                                       lkup,
                                       lkup_hash = lkup$cache_data_id$hash_ui_cp) {

  # Select surveys to use for CP page
  lkup$svy_lkup <- lkup$svy_lkup[display_cp == 1]

  hc <- ui_cp_ki_headcount_optim(country = country,
                                 povline = povline,
                                 lkup = lkup)

  dl <- lapply(lkup[["cp_lkups"]]$key_indicators, function(x) {
    x[country_code == country]
  })

  tmp <- list(headcount = hc)
  dl <- list(append(tmp, dl))

  return(dl)

}


#' CP Key Indicator Headcount
#'
#' Populate the country profiles key indicator for headcount.
#'
#' @inheritParams cp_key_indicators
#' @return data.table
#' @noRd
ui_cp_ki_headcount_optim <- function(country,
                                     year = "MRV",
                                     povline,
                                     lkup) {

  # Fetch most recent year (for CP-display)
  res <- pip(country         = country,
             year            = year,
             povline         = povline,
             lkup            = lkup)

  ### TEMP FIX for reporting level
  res <- cp_correct_reporting_level(res)
  ### TEMP FIX END

  out <- data.table::data.table(
    country_code = country, reporting_year = res$reporting_year,
    poverty_line = povline, headcount = res$headcount
  )
  return(out)
}


#' Country Profiles Charts
#'
#' Provides numbers that will populate the country profile charts.
#'
#' @inheritParams pip
#' @inheritParams ui_hp_countries
#' @return list
#' @export
ui_cp_charts_optim <- function(country   = "AGO",
                               povline   = 1.9,
                               pop_units = 1e6,
                               lkup,
                               lkup_hash = lkup$cache_data_id$hash_ui_cp) {
  # Only supports single country selection
  # Make it explicit
  country <- country[1]

  # Select surveys to use for CP page
  lkup$svy_lkup <- lkup$svy_lkup[display_cp == 1]

  # Create list with poverty charts data
  dl <- ui_cp_poverty_charts_optim(
    country = country,
    povline = povline,
    pop_units = pop_units,
    lkup = lkup)

  dl <- list(pov_charts = list(dl))

  # Fetch pre-calculated data (filter selected country)
  dl2 <- lapply(lkup[["cp_lkups"]]$charts, function(x) {
    x[country_code == country]
  })

  dl <- list(append(dl, dl2))
  names(dl) <- country

  return(dl)
}


#' CP Poverty Charts
#'
#' Provides numbers that will populate the country profiles poverty charts
#'
#' @inheritParams ui_cp_charts
#' @return list
#' @keywords internal
ui_cp_poverty_charts_optim <- function(country,
                                 povline,
                                 pop_units,
                                 lkup) {
  # STEP 1: Identify all regional countries to be used in comparison chart ----
  # Region of the selected country
  region <-
    lkup$svy_lkup[country_code == country]$region_code |>
    unique()
  # All countries from the same region
  countries <-
    lkup$svy_lkup[region_code == region]$country_code |>
    unique()

  # STEP 2: Compute stats for all countries from the region ----
  res_pov <- pip(country         = countries,
                 year            = "all",
                 povline         = povline,
                 lkup            = lkup)

  # STEP 3: Prepare data for poverty trend chart ----
    res_pov_trend <- res_pov[res_pov$country_code == country, ]
  if (nrow(res_pov_trend) == 0) {
    return(pipapi::empty_response_cp_poverty)
  }
  ### TEMP FIX for reporting level
    res_pov_trend <- cp_correct_reporting_level(res_pov_trend)
  ### TEMP FIX END

  res_pov_trend$pop_in_poverty <-
    res_pov_trend$reporting_pop * res_pov_trend$headcount / pop_units
  res_pov_trend <-
    res_pov_trend[, c(
      "country_code", "reporting_year",  "poverty_line",
      "survey_acronym", "welfare_type", "survey_comparability",
      "comparable_spell", "headcount", "pop_in_poverty",
      "reporting_level"
    )]

  # STEP 4 - Prepare data for poverty bar chart ----
  ### TEMP FIX for reporting level
  res_pov_mrv <- cp_correct_reporting_level(res_pov)
  ### TEMP FIX END

  res_pov_mrv <-
    res_pov_mrv[, .SD[which.max(reporting_year)],
                by = .(country_code)
    ]
  selected_year <- res_pov_mrv[country_code == country]$reporting_year
  year_range <- c((selected_year - 3):(selected_year + 3))
  res_pov_mrv <-
    res_pov_mrv[reporting_year %in% year_range]
  res_pov_mrv <- res_pov_mrv[, c("country_code",
                                 "reporting_year",
                                 "poverty_line",
                                 "headcount",
                                 "welfare_type",
                                 "reporting_level"
    )]
  res_pov_mrv <-
    cp_pov_mrv_select_countries(res_pov_mrv, country)
  res_pov_mrv$sort_order <- 1:nrow(res_pov_mrv)

  out <- list(
    pov_trend = res_pov_trend,
    pov_mrv = res_pov_mrv
  )

  return(out)
}
