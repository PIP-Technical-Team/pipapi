#' Add aggregated stats to `pip()`
#' @param df data.frame: Response from `rg_pip()` or `fg_pip()`.
#' @param except character: countries to be filtered out from computations
#' @return data.frame
#' @noRd
add_agg_stats <- function(df) {
  # Keep only Urban / Rural observations that will be aggregated at the
  # national level
  aggregated <- df[df$is_used_for_aggregation, ]

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
ag_average_poverty_stats_old <- function(df) {
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

#' Compute poverty statistics for aggregated data distribution.
#'
#' @param df data.frame: Survey data
#' @return data.frame
#' @noRd
ag_average_poverty_stats <- function(df) {
  # This should be removed eventually
  assertthat::assert_that(assertthat::are_equal(length(df$reporting_level), 2))

  # set different groups of variables --------

  ## original names -------
  orig_names <- data.table::copy(names(df))
  # convert years variables te chracter temporarily.

  ## years variables to character ------
  years_vars <- grep("year", names(df), value = TRUE)
  df[, (years_vars) :=
       lapply(.SD, as.character),
     .SDcols = years_vars]

  ## names of vars to average ------
  num_names <- names(df)[unlist(lapply(df, is.numeric))]
  nonum_names <- names(df)[ !names(df) %in% num_names]

  avg_names <- grep("^reporting", num_names, value = TRUE, invert = TRUE)
  rep_names <- grep("^reporting", num_names, value = TRUE)

  # treat negatives  and Zeros -----------
  ## negatives ------
  noneg_vars <-
    c("mean",
      "median",
      "headcount",
      "poverty_gap",
      "poverty_severity",
      "watts",
      "spr")

  df[, (noneg_vars) :=
       lapply(.SD, \(x) {
         if (any(x < 0, na.rm = TRUE) || anyNA(x)) {
           NA_real_
         } else {
           x
         }
       }),
     .SDcols = noneg_vars]

  ## zeros -------------
  zero_vars <-
    c("mean",
      "median",
      "watts")

  df[, (zero_vars) :=
       lapply(.SD, \(x) {
         if (any(x == 0, na.rm = TRUE)) {
           NA_real_
         } else {
           x
         }
         }),
     .SDcols = zero_vars]

  # Calculations ----------
  ## weighted average  ------
  totpop <- sum(df$reporting_pop)
  # df[,
  #    wgt := reporting_pop/totpop]
  wgt <-  df$reporting_pop/totpop

  # wgt_df <-
  #   df[,
  #      lapply(.SD, weighted.mean, w = wgt),
  #      .SDcols = avg_names]

  # wgt_df <- df |>
  #   # this grouping is not necessary, but ensures data.frame as output
  #   collapse::fgroup_by(c("country_code", "reporting_year", "welfare_type")) |>
  #   collapse::get_vars(avg_names) |>
  #   collapse::fmean(wgt,
  #                   keep.group_vars = FALSE,
  #                   keep.w = FALSE)
  wgt_df <- df |>
    # this grouping is not necessary, but ensures data.frame as output
    collapse::fgroup_by(c("country_code", "reporting_year", "welfare_type")) |>
    collapse::get_vars(c("reporting_pop", avg_names)) |>
    collapse::fmean(reporting_pop,
                    keep.group_vars = FALSE,
                    keep.w = TRUE)


  ## National total of reporting vars ------
  sum_df <-
    df[,
       lapply(.SD, sum),
       .SDcols = rep_names]

  # Bind resulting tables  ------
  out <- cbind(df[1, ..nonum_names], wgt_df, sum_df)


  # format variables  ----
  ## convert years back to numeric
  out[, (years_vars) :=
       lapply(.SD, as.numeric),
     .SDcols = years_vars]

  ## vars to intentionally set to NAs  ------
  na_cols <- c("survey_mean_lcu", "ppp", "median", "survey_median_ppp")
  out[, na_cols] <- NA_real_

  ## Label as national -------
  national_cols <- c(
    "reporting_level",
    "gdp_data_level",
    "pce_data_level",
    "cpi_data_level",
    "ppp_data_level"
  )

  out[, (national_cols) := "national"]

  ## set order of obs anc col -------

  out <- out[, ..orig_names]
  data.table::setcolorder(out, orig_names)
  data.table::setorderv(out, c("country_code", "reporting_year","welfare_type"))


  # Return ------
  return(out)

}
