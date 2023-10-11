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
ag_average_poverty_stats <- function(df) {
  # This should be removed eventually
  assertthat::assert_that(assertthat::are_equal(length(df$reporting_level), 2))

  # STEP 1: Identify groups of variables that will be handled differently ------
  ## original names
  orig_names <- data.table::copy(names(df))

  ## convert years variables to character temporarily ------
  ## Only numeric variables will be aggregated or averaged
  ## Year variables must not be modified
  years_vars <- grep("year", names(df), value = TRUE)
  years_vars <- years_vars[!vapply(df[, ..years_vars],
                                   is.logical,
                                   FUN.VALUE = logical(1))]
  df[, (years_vars) :=
       lapply(.SD, as.character),
     .SDcols = years_vars]

  ## Identify vars to average ------
  num_names <- names(df)[unlist(lapply(df, is.numeric))]
  nonum_names <- names(df)[ !names(df) %in% num_names]
  avg_names <- grep("^reporting", num_names, value = TRUE, invert = TRUE)

  ## Identify vars to aggregate ------
  rep_names <- grep("^reporting", num_names, value = TRUE)

  # STEP 2: Handle negative and zero values -----------
  ## Handle negatives ------
  noneg_vars <-
    c("mean",
      "median",
      "headcount",
      "poverty_gap",
      "poverty_severity",
      "watts"#,
      #"spr"
      )

  df[, (noneg_vars) :=
       lapply(.SD, \(x) {
         if (any(x < 0, na.rm = TRUE) || anyNA(x)) {
           NA_real_
         } else {
           x
         }
       }),
     .SDcols = noneg_vars]

  ## Handle zeros -------------
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

  # STEP 3: Calculations ----------
  ## weighted average  ------
  totpop <- sum(df$reporting_pop)

  wgt <-  df$reporting_pop/totpop

  wgt_df <- df |>
    # this grouping is not necessary, but ensures data.frame as output
    collapse::fgroup_by(c("country_code", "reporting_year", "welfare_type")) |>
    collapse::get_vars(c("reporting_pop", avg_names)) |>
    collapse::fmean(reporting_pop,
                    keep.group_vars = FALSE,
                    keep.w = FALSE)


  ## Sum: National total of reporting vars ------
  sum_df <-
    df[,
       lapply(.SD, sum),
       .SDcols = rep_names]

  # STEP 4: Format results ----
  ## Bind resulting tables ----
  out <- cbind(df[1, ..nonum_names], wgt_df, sum_df)

  ## convert years back to numeric ----
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
