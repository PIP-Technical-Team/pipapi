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
#' @param censor logical: Triggers censoring of country/year statistics
#' @param lkup_hash character: hash of pip
#' @param additional_ind logical: If TRUE add new set of indicators. Default if
#'   FALSE
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
pip <- function(country         = "ALL",
                year            = "ALL",
                povline         = 1.9,
                popshare        = NULL,
                fill_gaps       = FALSE,
                group_by        = c("none", "wb"),
                welfare_type    = c("all", "consumption", "income"),
                reporting_level = c("all", "national", "rural", "urban"),
                ppp             = NULL,
                lkup,
                censor          = TRUE,
                lkup_hash       = lkup$cache_data_id$hash_pip,
                additional_ind  = FALSE) {


  welfare_type    <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by        <- match.arg(group_by)

  # TEMPORARY UNTIL SELECTION MECHANISM IS BEING IMPROVED
  country <- toupper(country)
  if (is.character(year)) {
    year <- toupper(year)
  }

  # If svy_lkup is not part of lkup throw an error.
  if (!all(c('svy_lkup') %in% names(lkup)))
    stop("You are probably passing more than one dataset as lkup argument.
  Try passing a single one by subsetting it lkup <- lkups$versions_paths$dataset_name_PROD")

  # **** TO BE REMOVED **** REMOVAL STARTS HERE
  # Once `pip-grp` has been integrated in ingestion pipeline
  # Forces fill_gaps to TRUE when using group_by option
  if (group_by != "none") {
    fill_gaps <- TRUE
    message("Info: argument group_by in pip() is deprecated; please use pip_grp() instead.")
  }
  # **** TO BE REMOVED **** REMOVAL ENDS HERE

  lcv <- # List with countries vectors
    create_countries_vctr(
      country         =  country,
      year            =  year,
      valid_years     =  lkup$valid_years,
      aux_files       =  lkup$aux_files
    )


  if (fill_gaps) {
    # Compute imputed stats
    out <- fg_pip(
      country            = lcv$est_ctrs,
      year               = year,
      povline            = povline,
      popshare           = popshare,
      welfare_type       = welfare_type,
      reporting_level    = reporting_level,
      ppp                = ppp,
      lkup               = lkup
      )
  } else {
    # Compute survey year stats
    out <- rg_pip(
      country         = lcv$est_ctrs,
      year            = year,
      povline         = povline,
      popshare        = popshare,
      welfare_type    = welfare_type,
      reporting_level = reporting_level,
      ppp             = ppp,
      lkup            = lkup
    )
  }

  # return empty dataframe if no metadata is found
  if (nrow(out) == 0) {
    return(out)
  }

  # Handles aggregated distributions
  if (reporting_level %in% c("national", "all")) {
    out <- add_agg_stats(out)
    if (reporting_level == "national") {
      out <- out[reporting_level == "national"]
    }
  }

  # **** TO BE REMOVED **** REMOVAL STARTS HERE
  # Once `pip-grp` has been integrated in ingestion pipeline
  # Handles grouped aggregations
  if (group_by != "none") {
    # Handle potential (insignificant) difference in poverty_line values that
    # may mess-up the grouping
    out$poverty_line <- povline

    out <- pip_aggregate_by(
      df = out,
      group_lkup = lkup[["pop_region"]]
    )
    # Censor regional values
    if (censor) {
      out <- censor_rows(out, lkup[["censored"]], type = "regions")
    }

    out <- out[, c("region_name",
                   "region_code",
                   "reporting_year",
                   "reporting_pop",
                   "poverty_line",
                   "headcount",
                   "poverty_gap",
                   "poverty_severity",
                   "watts",
                   "mean",
                   "pop_in_poverty")]

    return(out)
  }
  # **** TO BE REMOVED **** REMOVAL ENDS HERE

  # Add pre-computed distributional statistics
  crr_names  <- names(out)    # current variables
  names2keep <- lkup$pip_cols # all variables

  out <- add_dist_stats(
    df = out,
    dist_stats = lkup[["dist_stats"]]
  )

  if (fill_gaps) {
    # Convert inequality indicators to NA
    dist_vars  <- names2keep[!(names2keep %in% crr_names)]
    out[,
        (dist_vars) := NA_real_]
  }

  # Handle survey coverage
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
    names2keep  <- c(names2keep, added_names)

    # Keep relevant variables
    out  <- out[, .SD, .SDcols = names2keep]

  } else {

    out <- out[, .SD, .SDcols = names2keep]

  }

  #Order rows by country code and reporting year
  data.table::setorder(out, country_code, reporting_year, reporting_level, welfare_type)


  return(out)
}
