#' Home Page Main Chart
#'
#' Provides numbers that will populate the home page main chart.
#'
#' @param povline numeric: Poverty line
#' @param lkup list: A list of lkup tables
#'
#' @return data.table
#' @export
ui_hp_stacked <- function(povline = 1.9,
                          lkup) {
  out <- pip(
    country = "all",
    year = "all",
    povline = povline,
    lkup = lkup,
    fill_gaps = TRUE,
    aggregate = TRUE,
    group_by = "wb",
    reporting_level = "national"
  )

  out <- out[, .(
    region_code, reporting_year,
    poverty_line, pop_in_poverty
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
ui_hp_countries <- function(country = c("AGO", "CIV"),
                            povline = 1.9,
                            pop_units = 1e6,
                            lkup) {
  out <- pip(
    country = country,
    year = "all",
    povline = povline,
    lkup = lkup,
    fill_gaps = FALSE,
    aggregate = FALSE,
    group_by = NULL,
    reporting_level = "national"
  )

  # Add pop_in_poverty and scale according to pop_units
  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop <- out$reporting_pop / pop_units

  out <- out[, .(
    region_code, country_code, reporting_year,
    poverty_line, reporting_pop, pop_in_poverty
  )]

  return(out)
}

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
                         aggregate = FALSE,
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
    aggregate = aggregate,
    group_by = group_by,
    reporting_level = reporting_level,
    lkup = lkup,
  )

  # Add pop_in_poverty and scale according to pop_units
  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop <- out$reporting_pop / pop_units

  if (group_by != "none") {
    return(out)
  } else if (fill_gaps == FALSE) {
    out <- out[, .(
      country_code, reporting_year, welfare_type,
      pop_data_level, median, gini, polarization,
      mld, decile1, decile2, decile3, decile4, decile5,
      decile6, decile7, decile8, decile9, decile10,
      region_code, survey_coverage, survey_comparability,
      comparable_spell, survey_year,
      survey_mean_lcu, survey_mean_ppp, # Do we need these here?
      reporting_pop, ppp, cpi, distribution_type,
      is_interpolated, poverty_line, mean, headcount,
      poverty_gap, poverty_severity, watts, pop_in_poverty
    )]
    return(out)
  } else {
    out <- out[, .(
      country_code, reporting_year, poverty_line, mean,
      headcount, poverty_gap, poverty_severity, watts,
      region_code, reporting_pop, is_interpolated,
      pop_in_poverty
    )]
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
ui_pc_regional <- function(povline = 1.9, pop_units = 1e6, lkup) {

  out <- pip(country = "all",
             year    = "all",
             group_by = "wb",
             povline = povline,
             lkup = lkup)

  # Add pop_in_poverty and scale according to pop_units
  out$pop_in_poverty <- out$reporting_pop * out$headcount / pop_units
  out$reporting_pop <- out$reporting_pop / pop_units

  return(out)
}


#' Country Profiles Key Indicators
#'
#' Provides numbers that will population for country profiles key indicators.
#'
#' @inheritParams pip
#' @return list
#' @export
ui_cp_key_indicators <- function(country = "AGO",
                                 povline = NULL,
                                 lkup) {

  if (country == "all") {
    country_codes <- unique(lkup$svy_lkup$country_code)
    dl <- lapply(country_codes, function(country)
      ui_cp_key_indicators_single(
        country = country, povline = povline, lkup = lkup))
  } else {
    dl <- ui_cp_key_indicators_single(
      country = country, povline = povline, lkup = lkup)
    dl <- list(dl)
  }
  return(dl)

}

#' Country Profile Key Indicators Single
#'
#' Country profile key indicators for a single country.
#'
#' @inheritParams ui_cp_key_indicators
#' @return list
#' @keywords internal
ui_cp_key_indicators_single <- function(country = "AGO",
                                        povline = NULL,
                                        lkup) {
  if (is.null(povline)) {
    poverty_lines <- lkup$pl_lkup$poverty_line
    hc <- lapply(poverty_lines, function(pl) {
      ui_cp_ki_headcount(country, pl, lkup)
    })
    hc <- data.table::rbindlist(hc)
    # names(hc) <- poverty_lines
  } else {
    hc <- ui_cp_ki_headcount(country, povline, lkup)
    # hc <- list(hc)
    # names(hc) <- povline
  }

  dl <- lapply(lkup$cp$key_indicators, function(x) {
    x[country_code == country]
  })

  out <- list(headcount = hc)
  out <- append(out, dl)
  return(out)
}

#' CP Key Indicator Headcount
#'
#' Populate the country profiles key indicator for headcount.
#'
#' @inheritParams cp_key_indicators
#' @return data.table
#' @noRd
ui_cp_ki_headcount <- function(country, povline, lkup) {
  res <- pip(country, year = "mrv", reporting_level = "national",
             povline = povline, lkup = lkup)
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
ui_cp_charts <- function(country = "AGO",
                         povline = NULL,
                         pop_units = 1e6,
                         lkup) {

  if (country == "all") {
    country_codes <- unique(lkup$svy_lkup$country_code)
    dl <- lapply(country_codes, function(country)
      ui_cp_charts_single(country = country, povline = povline,
                          pop_units = pop_units, lkup = lkup))
    names(dl) <- country_codes
  } else {
    dl <- ui_cp_charts_single(country = country, povline = povline,
                              pop_units = pop_units, lkup = lkup)
    dl <- list(dl)
    names(dl) <- country
  }
  return(dl)
}


#' Country profile charts for a single country
#'
#' @inheritParams ui_cp_charts
#' @return list
#' @keywords internal
ui_cp_charts_single <- function(country, povline,
                                pop_units, lkup) {

  if (is.null(povline)) {
    poverty_lines <- lkup$pl_lkup$poverty_line
    dl <- lapply(poverty_lines, function(pl) {
      ui_cp_poverty_charts(
        country = country,
        povline = pl,
        pop_units = pop_units,
        lkup = lkup
      )
    })
    # names(dl) <- poverty_lines
  } else {
    dl <- ui_cp_poverty_charts(
      country = country,
      povline = povline,
      pop_units = pop_units,
      lkup = lkup
    )
    dl <- list(dl)
    # names(dl) <- povline
  }

  dl <- list(pov_charts = dl)

  # Fetch pre-calculated data (filter selected country)
  dl2 <- lapply(lkup$cp$charts, function(x) {
    x[country_code == country]
  })

  out <- append(dl, dl2)

  return(out)
}


#' CP Poverty Charts
#'
#' Provides numbers that will populate the country profiles poverty charts
#'
#' @inheritParams ui_cp_charts
#' @return list
#' @keywords internal
ui_cp_poverty_charts <- function(country, povline, pop_units,
                                 lkup) {

  # Fetch data for poverty trend chart
  res_pov_trend <-
    pip(country = country, povline = povline,
        reporting_level = "national", lkup = lkup)
  res_pov_trend$pop_in_poverty <-
    res_pov_trend$reporting_pop * res_pov_trend$headcount / pop_units
  res_pov_trend <-
    res_pov_trend[, c(
      "country_code", "reporting_year",  "poverty_line",
      "survey_acronym", "welfare_type", "survey_comparability",
      "comparable_spell", "headcount", "pop_in_poverty"
    )]

  # Fetch data for poverty bar chart
  region <-
    lkup$svy_lkup[country_code == country]$region_code %>%
    unique()
  countries <-
    lkup$svy_lkup[region_code == region]$country_code %>%
    unique()

  res_pov_mrv <- pip(country = countries, povline = povline,
                     reporting_level = "national", lkup = lkup)
  res_pov_mrv <-
    res_pov_mrv[, .SD[which.max(reporting_year)],
                by = country_code
    ]
  selected_year <- res_pov_mrv[country_code == country]$reporting_year
  year_range <- c((selected_year - 3):(selected_year + 3))
  res_pov_mrv <-
    res_pov_mrv[reporting_year %in% year_range]
  res_pov_mrv <-
    res_pov_mrv[, c(
      "country_code", "reporting_year",
      "poverty_line", "headcount"
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

#' Select countries to display for CP Poverty MRV Chart
#' @param dt data.table: Result from `pip()` query
#' @param country character: Country code
#' @noRd
cp_pov_mrv_select_countries <- function(dt, country) {
  # IF the number of countries is > 12
  if (nrow(dt) > 12) {
    hc_req_country <- dt[country_code == country]$headcount
    hc_all <- dt$headcount
    vals <- cp_pov_mrv_select_values(v = hc_all, h = hc_req_country)
    dt <- dt[headcount %in% vals]
  }
  dt <- dt[order(headcount)]
  return(dt)
}

#' Select values to display for CP Poverty MRV Chart
#' @param v numeric: A vector with headcounts for all countries in the region
#' @param h numeric: Headcount for the request country
#' @noRd
#' @importFrom utils head tail
cp_pov_mrv_select_values <- function(v, h) {

  # Sort values
  v <- sort(v)

  # IF selected country does not pertain to top 5 or bottom 5, display:
  if (!(h %in% tail(v, 5) | h %in% head(v, 5))) {
    v <- v[!v %in% h]
    v_above <- v[v > h]
    v_below <- v[v < h]
    vals <- c(
      h, head(v, 3), tail(v, 3),
      head(v_above, 2), tail(v_below, 2)
    )
    # ELSE IF selected country pertains to top 5, display:
  } else if (h %in% tail(v, 5)) {
    vals <- c(head(v, 5), tail(v, 6))
    # ELSE
  } else {
    vals <- c(head(v, 6), tail(v, 5))
  }
  vals <- sort(vals)
  return(vals)
}

#' Data Sources Survey Metadata
#'
#' Provides survey metadata that will populate the Data Sources page.
#'
#' @inheritParams pip
#' @return data.table
#' @export
ui_svy_meta <- function(country = "all", lkup) {
  out <- pipapi::get_aux_table(
    data_dir = lkup$data_root,
    table = "survey_metadata"
  )
  if (country == "all") {
    return(out)
  } else {
    out <- out[out$country_code == country, ]
    return(out)
  }
}
