#' Format and finalise pip lineups output
#'
#' Shared post-processing tail used by both [pip_new_lineups()] and
#' [pip_old_lineups()].  Responsibilities (in order):
#' 1. Attach pre-computed distributional statistics.
#' 2. Attach aggregate medians.
#' 3. Zero out distributional vars and set `estimate_type` for lineup years.
#' 4. Filter to the requested `reporting_level`.
#' 5. Censor country rows when `censor = TRUE`.
#' 6. Optionally add additional indicators.
#' 7. Keep only the relevant output columns.
#' 8. Round all doubles to 12 significant digits.
#' 9. Sort by `country_code`, `reporting_year`, `reporting_level`,
#'    `welfare_type`.
#' 10. Remove duplicates with [collapse::funique()].
#'
#' @param out data.table: main output data passed in from the caller
#' @param lkup list: a versioned lkup list (same object as in callers)
#' @param fill_gaps logical: forwarded from the caller
#' @param reporting_level character: forwarded from the caller
#' @param censor logical: forwarded from the caller
#' @param additional_ind logical: forwarded from the caller
#' @param use_old_dist_stats logical: if `TRUE` use [add_dist_stats_old()]
#'   (frozen old pathway); if `FALSE` (default) use [add_dist_stats()]
#'   (new pathway). Default is `FALSE`.
#'
#' @return data.table with final output columns, ordered and de-duplicated
#'
#' @keywords internal
pip_lineups_format_output <- function(
  out,
  lkup,
  fill_gaps,
  reporting_level,
  censor,
  additional_ind,
  use_old_dist_stats = FALSE
) {
  # pre-computed distributional stats ---------------
  crr_names <- names(out) # current variables
  names2keep <- lkup$return_cols$pip$cols # all variables

  if (use_old_dist_stats) {
    out <- add_dist_stats_old(
      df = out,
      dist_stats = lkup[["dist_stats"]]
    )
  } else {
    out <- add_dist_stats(
      df = out,
      lkup = lkup,
      fill_gaps = fill_gaps
    )
  }

  # Add aggregate medians ----------------
  out <- add_agg_medians(
    df = out,
    fill_gaps = fill_gaps,
    data_dir = lkup$data_root
  )

  # format ----------------

  if (fill_gaps) {
    ## Inequality indicators to NA for lineup years ----
    dist_vars <- names2keep[!(names2keep %in% crr_names)]
    out[, (dist_vars) := NA_real_]

    ## estimate_var -----
    out <- estimate_type_ctr_lnp(out, lkup)
  } else {
    out[, estimate_type := NA_character_]
  }

  ## Handle survey coverage ------------
  if (reporting_level != "all") {
    keep <- out$reporting_level == reporting_level
    out <- out[keep, ]
  }

  # Censor country values
  if (censor) {
    out <- censor_rows(out, lkup[["censored"]], type = "countries")
  }

  # Select columns
  if (additional_ind) {
    get_additional_indicators(out)
    added_names <- attr(out, "new_indicators_names")
    names2keep <- c(names2keep, added_names)
  }
  # Keep relevant variables
  out <- out[, .SD, .SDcols = names2keep]

  # make sure we always report the same precision in all numeric variables
  doub_vars <-
    names(out)[unlist(lapply(out, is.double))] |>
    data.table::copy()

  out[, (doub_vars) := lapply(.SD, round, digits = 12), .SDcols = doub_vars]

  # Order rows by country code and reporting year
  data.table::setorder(
    out,
    country_code,
    reporting_year,
    reporting_level,
    welfare_type
  )

  # Make sure no duplicate remains
  out <- out |> collapse::funique()

  return(out)
}
