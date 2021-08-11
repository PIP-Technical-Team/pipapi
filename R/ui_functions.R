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


#' Provides numbers that will population key indicators for Country Profiles
#'
#' @param country character: Country code
#' @param povline numeric: Poverty line
#' @param lkup list: A list of lkup tables
#'
#' @return data.frame
#' @export
#'
ui_cp_key_indicators <- function(country = 'AGO', povline = NULL, lkup) {

  if (is.null(povline)) {
    poverty_lines <- lkup$pl_lkup$poverty_line
    dl_hc <- lapply(poverty_lines, function(pl) {
      ui_cp_ki_headcount(country, pl, lkup)
    })
  } else {
    dl_hc <- ui_cp_ki_headcount(country, povline, lkup)
  }

  dl <- lapply(lkup$cp$key_indicators, function(x) {
    x[country_code == country]
  })

  out <- list(headcount = dl_hc)
  out <- append(out, dl)
  return(out)
}

#' Provides numbers that will populate the country profiles key indicator for headcount
#'
#' @param country character: Country code
#' @param povline numeric: Poverty line
#' @param lkup list: A list of lkup tables
#'
#' @return data.frame
#' @noRd
#'
ui_cp_ki_headcount <- function(country, povline, lkup) {

  query_year <- max(lkup$svy_lkup[country_code == country]$reporting_year)
  res <- pip(country, year = query_year, povline = povline, lkup = lkup)
  out <- data.table::data.table(
    country_code = country, reporting_year = query_year,
    poverty_line = povline, headcount = res$headcount)
  return(out)

}


#' Provides numbers that will populate the country profile charts
#'
#' @param country character: Country code
#' @param povline numeric: Poverty line
#' @param pop_units numeric: Units used to express population numbers (default to million)
#' @param year_range numeric: Range used to subset countries in Poverty MRV chart
#' @param lkup list: A list of lkup tables
#'
#' @return list
#' @export
#'
ui_cp_charts <- function(country = 'AGO', povline = NULL,
                         pop_units = 1e6, year_range = 2016:2019,
                         lkup) {


  if (is.null(povline)) {
    poverty_lines <- lkup$pl_lkup$poverty_line
    dl <- lapply(poverty_lines, function(pl) {
      ui_cp_poverty_charts(country = country,
                           povline = pl,
                           pop_units = pop_units,
                           year_range = year_range,
                           lkup = lkups)
    })
  } else {
    dl <-
      ui_cp_poverty_charts(country = country,
                           povline = povline,
                           pop_units = pop_units,
                           year_range = year_range,
                           lkup = lkups)

  }

  # Fetch pre-calculated data (filter selected country)
  dl2 <- lapply(lkup$cp$charts, function(x) {
    x[country_code == country]
  })

  out <- append(dl, dl2)

  return(out)

}

#' Provides numbers that will populate the country profiles poverty charts
#'
#' @inheritParams ui_cp_charts
#' @return list
#' @keywords internal
#'
ui_cp_poverty_charts <- function(country, povline, pop_units,
                                 year_range, lkup) {

  # Fetch data for poverty trend chart
  res_pov_trend <-
    pip(country = country, povline = povline, lkup = lkup)
  res_pov_trend$pop_in_poverty <-
    res_pov_trend$reporting_pop * res_pov_trend$headcount / pop_units
  res_pov_trend <-
    res_pov_trend[, c('country_code', 'reporting_year',
                      'headcount', 'pop_in_poverty')]

  # Fetch data for poverty bar chart
  region <-
    lkup$svy_lkup[country_code == country]$pcn_region_code %>%
    unique()
  countries <-
    lkup$svy_lkup[pcn_region_code == region]$country_code %>%
    unique()

  res_pov_mrv <- pip(country = countries, lkup = lkup)
  res_pov_mrv <-
    res_pov_mrv[, .SD[which.max(reporting_year)],
                by = country_code]
  res_pov_mrv <-
    res_pov_mrv[reporting_year %in% year_range]
  res_pov_mrv <-
    res_pov_mrv[, c('country_code', 'reporting_year',
                    'poverty_line', 'headcount')]
  res_pov_mrv <-
    cp_pov_mrv_select_countries(res_pov_mrv, country)

  out <- list(
    pov_trend = res_pov_trend,
    pov_mrv = res_pov_mrv
  )

  return(out)

}

#' Select countries to display for CP Poverty MRV Chart
#' @param dt data.table: Result from `pip()` query
#' @param country character: Country code
#' @noRd
cp_pov_mrv_select_countries <- function(dt, country){
  # If the number of countries in the region of the selected country is > 12,
  # then the countries to be displayed will be selected as follow:
  if (nrow(dt) > 12) {
    h <- dt[country_code == country]$headcount
    v <- sort(dt$headcount)
    # IF selected country does not pertain to top 5 or bottom 5, display:
    if (!(h %in% utils::tail(v, 5) | h %in% utils::head(v, 5))) {
      v <- v[!v %in% h]
      v2 <- v[v > h]
      v3 <- v[v < h]
      vals <- c(h, utils::head(v, 3), utils::tail(v, 3),
                utils::head(v2, 2), utils::tail(v3, 2))
      # ELSE IF selected country pertains to top 5, display:
    } else if (h %in% utils::tail(v, 5)) {
      vals <- c(utils::head(v, 5), utils::tail(v, 6))
      # ELSE display
    } else {
      vals <- c(utils::head(v, 6), utils::tail(v, 5))
    }

    dt <- dt[headcount %in% vals]
    #dt <- dt[order(headcount)]

  }
  dt <- dt[order(headcount)]
  return(dt)
}
