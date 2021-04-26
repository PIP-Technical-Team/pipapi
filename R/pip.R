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
#' @param svy_coverage character: Survey coverage.
#' @param ppp numeric: Custom Purchase Power Parity value
#' @param lkup
#' @param paths
#'
#' @return data.frame
#' @export
#'
pip <- function(country   = "all",
                year      = "all",
                povline   = NULL,
                popshare  = NULL,
                fill_gaps = FALSE,
                aggregate = FALSE,
                group_by  = NULL,
                welfare_type = c("all", "consumption", "income"),
                svy_coverage = c("all", "rural", "urban"),
                ppp       = NULL,
                lkup,
                paths) {

  welfare_type <- match.arg(welfare_type)
  svy_coverage <- match.arg(svy_coverage)

  # Forces fill_gaps to TRUE when using group_by option
  if (!is.null(group_by)) {
    fill_gaps <- TRUE
  }

  # Handle interpolation
  if (fill_gaps == TRUE) {

    out <- fg_pip(country      = country,
                  year         = year,
                  povline      = povline,
                  popshare     = popshare,
                  aggregate    = aggregate,
                  welfare_type = welfare_type,
                  svy_coverage = svy_coverage,
                  ppp          = ppp,
                  server       = server,
                  lkup         = lkup,
                  paths        = paths)
  } else {
    out <- rg_pip(country      = country,
                  year         = year,
                  povline      = povline,
                  popshare     = popshare,
                  aggregate    = aggregate,
                  welfare_type = welfare_type,
                  svy_coverage = svy_coverage,
                  ppp          = ppp,
                  server       = server,
                  lkup         = lkup,
                  paths        = paths)
  }

  # return empty dataframe if no metadata is found
  if (nrow(out) == 0) {
    return(out)
  }

  # Handle aggregated distributions
  if (svy_coverage %in% c("national", "all")) {
    out <- add_agg_stats(out)
  }


  # Handle grouped aggregations
  if (!is.null(group_by)) {
    # Handle potential (insignificant) difference in poverty_line values  that
    # may mess-up the grouping
    out$poverty_line <- povline

    out <- aggregate_by_group(df = out,
                              group_lkup = lkups[["pop_region"]])

    return(out)
  }


  # Add pre-computed distributional statistics
  out <- add_dist_stats(df = out,
                        dist_stats = lkup[["dist_stats"]])

  # Handle survey coverage
  if (svy_coverage != "all") {
    out <- out[out$pop_data_level == svy_coverage, ]
  }

  # ADD FIX FOR MEDIAN WHEN INTERPOLATING
  # median <- dist_stats[["median"]]/(data_mean/requested_mean)

  return(out)
}


