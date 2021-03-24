get_svy_metadata <- function(country,
                             year,
                             welfare_type,
                             svy_coverage,
                             lkup) {

  svy_n <- nrow(lkup)
  keep <- rep(TRUE, svy_n)
  # Select data files based on requested country, year, etc.
  # Select countries
  if (country[1] != "all") {
    keep <- keep & lkup$country_code %in% country
  }
  # Select years
  if (year[1] != "all") {
      keep <- keep & lkup$reporting_year %in% year
    }
  # Select welfare_type
  if (welfare_type[1] != "all") {
    keep <- keep & lkup$welfare_type == welfare_type
  }
  # Select survey coverage
  if (svy_coverage[1] != "all") {
    keep <- keep &
      (lkup$survey_coverage == svy_coverage |
         lkup$pop_data_level  == svy_coverage)
  }

  lkup <- lkup[keep, ]

    return(lkup)
}

get_svy_data <- function(svy_id,
                         svy_coverage,
                         paths)
{
  svy_coverage <- unique(svy_coverage)
  assertthat::assert_that(length(svy_coverage) == 1,
                          msg = "Problem with input data: Multiple pop_data_levels")

  out <- purrr::map(svy_id, function(id) {
    path <- paths[stringr::str_detect(paths, id)]
    tmp <- fst::read_fst(path)
    if (svy_coverage %in% c("urban", "rural")) { # Not robust. Should not be hard coded here.
      tmp <- tmp[tmp$area == svy_coverage, ]
    }
    tmp <- tmp[, c("welfare", "weight")]

    return(tmp)
  })

  names_out <- paste0("df", (seq_along(svy_id) - 1))
  names(out) <- names_out

  return(out)
}


create_empty_response <- function() {
  out <- data.frame(
           survey_id = c(NA),
         region_code = c(NA),
        country_code = c(NA),
      reference_year = c(NA),
       surveyid_year = c(NA),
      reporting_year = c(NA),
      survey_acronym = c(NA),
     survey_coverage = c(NA),
         survey_year = c(NA),
        welfare_type = c(NA),
     survey_mean_ppp = c(NA),
  predicted_mean_ppp = c(NA),
                 ppp = c(NA),
       reference_pop = c(NA),
       reference_gdp = c(NA),
       reference_pce = c(NA),
      pop_data_level = c(NA),
      gdp_data_level = c(NA),
      pce_data_level = c(NA),
      cpi_data_level = c(NA),
      ppp_data_level = c(NA),
   distribution_type = c(NA),
             gd_type = c(NA),
        poverty_line = c(NA),
                mean = c(NA),
              median = c(NA),
        poverty_rate = c(NA),
         poverty_gap = c(NA),
    poverty_severity = c(NA),
               watts = c(NA),
                gini = c(NA),
                 mld = c(NA),
        polarization = c(NA),
             decile1 = c(NA),
             decile2 = c(NA),
             decile3 = c(NA),
             decile4 = c(NA),
             decile5 = c(NA),
             decile6 = c(NA),
             decile7 = c(NA),
             decile8 = c(NA),
             decile9 = c(NA),
            decile10 = c(NA),
     is_interpolated = c(NA)
  )

  return(out)
}

#' Computes poverty statistics (aggregated)
#'
#' Compute poverty statistics for aggregated data distribution.
#'
#' @inheritParams gd_compute_pip_stats
#' @param area character: Area (Urban or Rural)
#' @param area_pop numeric: Total population per area.
#' @return list
#' @keywords internal
ag_average_poverty_stats <- function(df) {

  assertthat::assert_that(assertthat::are_equal(length(df$pop_data_level), 2))
  dfu <- df[df$pop_data_level == "urban", ]
  dfr <- df[df$pop_data_level == "rural", ]

  # Compute stats for each sub-group
  out <- dfr

  # Set distributional stats to NA is not based on microdata
  if (dfu$distribution_type != "micro" | dfr$distribution_type != "micro") {
    # Column to be set to NA
    # Cannot be computed through weighted average because   # these measures are
    # not additive
    na_cols <- c("survey_mean_lcu", "ppp", "cpi","gini", "mld", "polarization",
                 "decile1", "decile2", "decile3", "decile4", "decile5", "decile6",
                 "decile7", "decile8", "decile9", "decile10")
    out[, na_cols] <- NA
  }

  # Compute population weighted average
  wgt_urban <- dfu$reporting_pop / sum(df$reporting_pop)
  wgt_rural <- 1 - wgt_urban

  out$survey_mean_ppp = wgt_urban * dfu$mean +
    wgt_rural * dfr$mean

  if (dfr$poverty_severity < 0) {# Check if rural poverty severity < 0

    if (dfu$poverty_severity < 0) # Same for urban
    {
      out[, c("headcount", "poverty_gap", "poverty_severity")] <- NA
    } else {
      out$headcount        <- dfu$headcount
      out$poverty_gap      <- dfu$poverty_gap
      out$poverty_severity <- dfu$poverty_severity
    }
  } else {
    if (dfu$poverty_severity < 0) {
      out$headcount        <- dfr$headcount
      out$poverty_gap      <- dfr$poverty_gap
      out$poverty_severity <- dfr$poverty_severity
    } else {
      out$headcount <- wgt_rural * dfr$headcount +
        wgt_urban * dfu$headcount

      out_poverty_gap <- wgt_rural * dfr$poverty_gap +
        wgt_urban * dfu$poverty_gap

      out_poverty_severity <- wgt_rural * dfr$poverty_severity +
        wgt_urban * dfu$poverty_severity
    }
  }

  if (dfu$watts > 0 & dfr$watts > 0) {
    out_watts <- wgt_rural * dfr$watts +
      wgt_urban * dfu$watts
  } else {
    out$watts <- NA
  }

  # Update other variables
  out$reporting_pop <- sum(df$reporting_pop)
  out[, c("pop_data_level", "gdp_data_level",
          "pce_data_level", "cpi_data_level", "ppp_data_level")] <- "national"

  return(out)
}
