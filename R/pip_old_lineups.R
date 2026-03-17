#' Compute PIP statistics - Old lineups function
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
#' pip_old_lineups(country = "AGO",
#'     year = 2000,
#'     povline = 1.9,
#'     lkup = lkups)
#'
#' # All years for a single country
#' pip_old_lineups(country = "AGO",
#'     year = "all",
#'     povline = 1.9,
#'     lkup = lkups)
#'
#' # Fill gaps
#' pip_old_lineups(country = "AGO",
#'     year = "all",
#'     povline = 1.9,
#'     fill_gaps = TRUE,
#'     lkup = lkups)
#'
#' # Group by regions
#' pip_old_lineups(country = "all",
#'     year = "all",
#'     povline = 1.9,
#'     group_by = "wb",
#'     lkup = lkups)
#' }
#' @export
pip_old_lineups <- function(
  country = "ALL",
  year = "ALL",
  povline = 1.9,
  popshare = NULL,
  fill_gaps = FALSE,
  group_by = c("none", "wb"),
  welfare_type = c("all", "consumption", "income"),
  reporting_level = c("all", "national", "rural", "urban"),
  ppp = NULL,
  lkup,
  censor = TRUE,
  lkup_hash = lkup$cache_data_id$hash_pip,
  additional_ind = FALSE
) {
  # set up -------------
  welfare_type <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by <- match.arg(group_by)
  povline <- round(povline, digits = 3)

  # TEMPORARY UNTIL SELECTION MECHANISM IS BEING IMPROVED
  country <- toupper(country)
  if (is.character(year)) {
    year <- toupper(year)
  }

  # If svy_lkup is not part of lkup throw an error.
  if (!all(c('svy_lkup') %in% names(lkup))) {
    stop(
      "You are probably passing more than one dataset as lkup argument.
  Try passing a single one by subsetting it lkup <- lkups$versions_paths$dataset_name_PROD"
    )
  }

  # **** TO BE REMOVED **** REMOVAL STARTS HERE
  # Once `pip-grp` has been integrated in ingestion pipeline
  # Forces fill_gaps to TRUE when using group_by option
  if (group_by != "none") {
    fill_gaps <- TRUE
    message(
      "Info: argument group_by in pip() is deprecated; please use pip_grp() instead."
    )
  }
  # **** TO BE REMOVED **** REMOVAL ENDS HERE

  # Countries vector ------------
  lcv <- # List with countries vectors
    create_countries_vctr(
      country = country,
      year = year,
      lkup = lkup
    )
  # lcv$est_ctrs has all the country_code that we are interested in

  cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
  if (!file.exists(cache_file_path)) {
    # Create an empty duckdb file
    create_duckdb_file(cache_file_path)
  }
  # mains estimates ---------------
  if (fill_gaps) {
    ## lineup years-----------------
    out <- fg_pip_old(
      country = lcv$est_ctrs,
      year = year,
      povline = povline,
      popshare = popshare,
      welfare_type = welfare_type,
      reporting_level = reporting_level,
      ppp = ppp,
      lkup = lkup
    )
  } else {
    ## survey years ------------------
    out <- rg_pip_old(
      country = lcv$est_ctrs,
      year = year,
      povline = povline,
      popshare = popshare,
      welfare_type = welfare_type,
      reporting_level = reporting_level,
      ppp = ppp,
      lkup = lkup
    )
  }

  cached_data <- out$data_in_cache
  main_data <- out$main_data

  if (nrow(main_data) > 0) {
    out <- main_data |>
      rowbind(cached_data)

    update_master_file(main_data, cache_file_path, fill_gaps)
  } else {
    out <- cached_data
  }
  if (!data.table::is.data.table(out)) {
    setDT(out)
  }
  # Early return for empty table---------------
  if (nrow(out) == 0) {
    return(pipapi::empty_response)
  }

  # aggregate distributions ------------------
  if (reporting_level %in% c("national", "all")) {
    out <- add_agg_stats(
      df = out,
      return_cols = lkup$return_cols$ag_average_poverty_stats
    )
    if (reporting_level == "national") {
      out <- out[reporting_level == "national"]
    }
  }

  add_vars_out_of_pipeline(out, fill_gaps = fill_gaps, lkup = lkup)

  # **** TO BE REMOVED **** REMOVAL STARTS HERE
  # Once `pip-grp` has been integrated in ingestion pipeline
  # Handles grouped aggregations
  if (group_by != "none") {
    # Handle potential (insignificant) difference in poverty_line values that
    # may mess-up the grouping
    out$poverty_line <- povline

    out <- pip_aggregate_by(
      df = out,
      group_by = group_by,
      return_cols = lkup$return_cols$pip_grp
    )
    # Censor regional values
    if (censor) {
      out <- censor_rows(out, lkup[["censored"]], type = "regions")
    }

    out <- out[, c(
      "region_name",
      "region_code",
      "reporting_year",
      "reporting_pop",
      "poverty_line",
      "headcount",
      "poverty_gap",
      "poverty_severity",
      "watts",
      "mean",
      "pop_in_poverty"
    )]

    return(out)
  }
  # **** TO BE REMOVED **** REMOVAL ENDS HERE

  # Format, censor, select columns, order, de-duplicate ----------------
  out <- pip_lineups_format_output(
    out = out,
    lkup = lkup,
    fill_gaps = fill_gaps,
    reporting_level = reporting_level,
    censor = censor,
    additional_ind = additional_ind,
    use_old_dist_stats = TRUE
  )
  # return -------------
  return(out)
}
