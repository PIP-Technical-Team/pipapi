#' Add aggregated stats to `pip()`
#' @param df data.frame: Response from `rg_pip()` or `fg_pip()`.
#' @return data.frame
#' @noRd
add_agg_stats <- function(df) {
  # Keep only Urban / Rural observations that will be aggregated at the
  # national level
  aggregated <- df[df$is_used_for_aggregation == TRUE, ]

  if (nrow(aggregated) > 0) {
    aggregated_list <- split(aggregated,
      interaction(
        aggregated$country_code,
        aggregated$reporting_year
      ),
      drop = TRUE
    )
    aggregated <- lapply(aggregated_list, ag_average_poverty_stats)
    aggregated <- data.table::rbindlist(aggregated)

    df <- rbind(df, aggregated)
  }

  return(df)
}

#' Compute poverty statistics for aggregated data distribution.
#'
#' @param df data.frame: Survey data
#' @return data.frame
#' @noRd
ag_average_poverty_stats <- function(df) {
  assertthat::assert_that(assertthat::are_equal(length(df$reporting_level), 2))
  dfu <- df[df$reporting_level == "urban", ]
  dfr <- df[df$reporting_level == "rural", ]

  # Compute stats for each sub-group
  out <- dfr

  # CHECK: CONDITION NOT NEEDED?
  # Set distributional stats to NA if not based on microdata
  # if (unique(df$distribution_type) == "micro") {
  #   # Column to be set to NA
  #   # Cannot be computed through weighted average because these measures are
  #   # not additive
  #   # na_cols <- c("survey_mean_lcu", "ppp", "median", "survey_median_ppp")
  #   # out[, na_cols] <- NA_real_
  # }

  # Set distributional stats to NA if not based on microdata
  na_cols <- c("survey_mean_lcu", "ppp", "median", "survey_median_ppp")
  out[, na_cols] <- NA_real_

  # Compute population weighted average
  wgt_urban <- dfu$reporting_pop / sum(df$reporting_pop)
  wgt_rural <- 1 - wgt_urban

  # Weighted national mean
  out$mean <- wgt_urban * dfu$mean +
    wgt_rural * dfr$mean

  if (dfr$poverty_severity < 0) { # Check if rural poverty severity < 0

    if (dfu$poverty_severity < 0) { # Same for urban

      out[, c("headcount", "poverty_gap", "poverty_severity")] <- NA_real_
    } else {
      out$headcount <- dfu$headcount
      out$poverty_gap <- dfu$poverty_gap
      out$poverty_severity <- dfu$poverty_severity
    }
  } else {
    if (dfu$poverty_severity < 0) {
      out$headcount <- dfr$headcount
      out$poverty_gap <- dfr$poverty_gap
      out$poverty_severity <- dfr$poverty_severity
    } else {
      out$headcount <- wgt_rural * dfr$headcount +
        wgt_urban * dfu$headcount

      out$poverty_gap <- wgt_rural * dfr$poverty_gap +
        wgt_urban * dfu$poverty_gap

      out$poverty_severity <- wgt_rural * dfr$poverty_severity +
        wgt_urban * dfu$poverty_severity
    }
  }

  if (dfu$watts > 0 & dfr$watts > 0) {
    out$watts <- wgt_rural * dfr$watts +
      wgt_urban * dfu$watts
  } else {
    out$watts <- NA_real_
  }

  # Update other variables
  out$reporting_pop <- sum(df$reporting_pop)
  national_cols <- c("reporting_level", "gdp_data_level",
                     "pce_data_level", "cpi_data_level", "ppp_data_level")
  out[, national_cols] <- "national"

  return(out)
}
