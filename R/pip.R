#' Compute the main PIP poverty and inequality statistics
#'
#' @param country character: Country ISO 3 codes
#' @param year integer: Reporting year
#' @param povline numeric: Poverty line
#' @param popshare numeric: Proportion of the population living below the poverty line
#' @param fill_gaps logical: If set to TRUE, will interpolate / extrapolate values for missing years
#' @param aggregate logical: If set to TRUE, will return aggregate results
#' @param group_by character: Will return aggregated values for pre-defined sub-groups
#' @param welfare_type character: Welfare type.
#' @param reporting_level character: Geographical reporting level
#' @param ppp numeric: Custom Purchase Power Parity value
#' @param lkup list: A list of lkup tables
#'
#' @return data.frame
#' @export
#'
pip <- function(country   = "all",
                year      = "all",
                povline   = 1.9,
                popshare  = NULL,
                fill_gaps = FALSE,
                aggregate = FALSE,
                group_by  = c("none", "wb"),
                welfare_type = c("all", "consumption", "income"),
                reporting_level = c("all", "national", "rural", "urban"),
                ppp       = NULL,
                lkup) {

  welfare_type <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by <- match.arg(group_by)

  # Forces fill_gaps to TRUE when using group_by option
  if (group_by != "none") {
    fill_gaps <- TRUE
  }

  if (fill_gaps == TRUE) {
    # Compute imputed stats
    out <- fg_pip(country      = country,
                  year         = year,
                  povline      = povline,
                  popshare     = popshare,
                  aggregate    = aggregate,
                  welfare_type = welfare_type,
                  reporting_level = reporting_level,
                  ppp          = ppp,
                  lkup         = lkup)
  } else {
    # Compute survey year stats
    out <- rg_pip(country      = country,
                  year         = year,
                  povline      = povline,
                  popshare     = popshare,
                  aggregate    = aggregate,
                  welfare_type = welfare_type,
                  reporting_level = reporting_level,
                  ppp          = ppp,
                  lkup         = lkup)
  }

  # return empty dataframe if no metadata is found
  if (nrow(out) == 0) {
    return(out)
  }

  # Handles aggregated distributions
  if (reporting_level %in% c("national", "all")) {
    out <- add_agg_stats(out)
  }

  # Handles grouped aggregations
  if (group_by != "none") {
    # Handle potential (insignificant) difference in poverty_line values that
    # may mess-up the grouping
    out$poverty_line <- povline

    out <- aggregate_by_group(df = out,
                              group_lkup = lkup[["pop_region"]])

    return(out)
  }


  # Add pre-computed distributional statistics
  out <- add_dist_stats(df = out,
                        dist_stats = lkup[["dist_stats"]])

  # Handle survey coverage
  if (reporting_level != "all") {
    out <- out[out$pop_data_level == reporting_level, ]
  }

  # Censor values
  out <- censor_rows(out, lkup[["censored"]])

  # ADD FIX FOR MEDIAN WHEN INTERPOLATING
  # median <- dist_stats[["median"]]/(data_mean/requested_mean)

  return(out)
}


