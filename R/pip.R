#' Compute PIP statistics
#'
#' Compute the main PIP poverty and inequality statistics.
#'
#' @param country character: Country ISO 3 codes
#' @param year integer: Reporting year
#' @param povline numeric: Poverty line
#' @param popshare numeric: Proportion of the population living below the
#'   poverty line
#' @param fill_gaps logical: If set to TRUE, will interpolate / extrapolate
#'   values for missing years
#' @param group_by character: Will return aggregated values for predefined
#'   sub-groups
#' @param welfare_type character: Welfare type
#' @param reporting_level character: Geographical reporting level
#' @param ppp numeric: Custom Purchase Power Parity value
#' @param lkup list: A list of lkup tables
#' @param debug logical: If TRUE poverty calculations from `wbpip` will run in
#'   debug mode
#'
#' @return data.table
#' @examples
#' \dontrun{
#' # Create lkups
#' lkups <- create_lkups("<data-folder>")
#'
#' # A single country and year
#' pip(country = "AGO",
#'     year = 2000,
#'     povline = 1.9,
#'     lkup = lkups)
#'
#' # All years for a single country
#' pip(country = "AGO",
#'     year = "all",
#'     povline = 1.9,
#'     lkup = lkups)
#'
#' # Fill gaps
#' pip(country = "AGO",
#'     year = "all",
#'     povline = 1.9,
#'     fill_gaps = TRUE,
#'     lkup = lkups)
#'
#' # Group by regions
#' pip(country = "all",
#'     year = "all",
#'     povline = 1.9,
#'     group_by = "wb",
#'     lkup = lkups)
#' }
#' @export
pip <- function(country = "all",
                year = "all",
                povline = 1.9,
                popshare = NULL,
                fill_gaps = FALSE,
                group_by = c("none", "wb"),
                welfare_type = c("all", "consumption", "income"),
                reporting_level = c("all", "national", "rural", "urban"),
                ppp = NULL,
                lkup,
                debug = FALSE) {

  welfare_type <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by <- match.arg(group_by)


  # **** TO BE REMOVED **** REMOVAL STARTS HERE
  # Once `pip-grp` has been integrated in ingestion pipeline
  # Forces fill_gaps to TRUE when using group_by option
  if (group_by != "none") {
    fill_gaps <- TRUE
  }
  # **** TO BE REMOVED **** REMOVAL ENDS HERE

  if (fill_gaps == TRUE) {
    # Compute imputed stats
    out <- fg_pip(
      country = country,
      year = year,
      povline = povline,
      popshare = popshare,
      welfare_type = welfare_type,
      reporting_level = reporting_level,
      ppp = ppp,
      lkup = lkup,
      debug = debug
    )
  } else {
    # Compute survey year stats
    # tictoc::tic("pip")
    out <- rg_pip(
      country = country,
      year = year,
      povline = povline,
      popshare = popshare,
      welfare_type = welfare_type,
      reporting_level = reporting_level,
      ppp = ppp,
      lkup = lkup,
      debug = debug
    )
    # Logging
    # end_pip <- tictoc::toc(quiet = TRUE)
    # logger::log_info('pip: {round(end_pip$toc - end_pip$tic, digits = getOption("digits", 6))}')
  }

  # return empty dataframe if no metadata is found
  if (nrow(out) == 0) {
    # out <- out[, .SD, .SDcols = cols]
    return(out)
  }

  # Handles aggregated distributions
  if (reporting_level %in% c("national", "all")) {
    out <- add_agg_stats(out)
  }

  # **** TO BE REMOVED **** REMOVAL STARTS HERE
  # Once `pip-grp` has been integrated in ingestion pipeline
  # Handles grouped aggregations
  if (group_by != "none") {
    # Handle potential (insignificant) difference in poverty_line values that
    # may mess-up the grouping
    out$poverty_line <- povline

    out <- aggregate_by_group(
      df = out,
      group_lkup = lkup[["pop_region"]]
    )
    # Censor regional values
    out <- censor_rows(out, lkup[["censored"]], type = "region")
    
    return(out)
  }
  # **** TO BE REMOVED **** REMOVAL ENDS HERE

  # Add pre-computed distributional statistics
  out <- add_dist_stats(
    df = out,
    dist_stats = lkup[["dist_stats"]]
  )

  # Handle survey coverage
  if (reporting_level != "all") {
    keep <- out$reporting_level == reporting_level
    out <- out[keep, ]
  }

  # Censor country values
  if (group_by == "none") {
    out <- censor_rows(out, lkup[["censored"]], type = "country")
  }

  # Select columns
  if (group_by == "none") {
    out <- out[, .SD, .SDcols = lkup$pip_cols]
  }

  return(out)
}
