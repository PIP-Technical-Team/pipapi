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


  # set up -------------
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

  # Countries vector ------------
  lcv <- # List with countries vectors
    create_countries_vctr(
      country         =  country,
      year            =  year,
      valid_years     =  lkup$valid_years,
      aux_files       =  lkup$aux_files
    )
  # lcv$est_ctrs has all the country_code that we are interested in
  # Integrate return_if_exists for following scenario
  # 1) country = "AGO" year = 2000 pl = 1.9 should return from master file
  # 2) country = "AGO" year = 2019 pl = 1.9 should return pip call
  # 3) country = c("CHN", "IND"), year = 2019, 2017 should return half from master file and half from pip call
  #
  # 4) country = "all" year = 2019
  # 5) country = "AGO" year = "all"
  # 6) country = "all" year = "all"

  cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
  read_con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_file_path, read_only = TRUE)
    # mains estimates ---------------
    if (fill_gaps) {
      ## lineup years-----------------
      out <- fg_pip(
        country            = lcv$est_ctrs,
        year               = year,
        povline            = povline,
        popshare           = popshare,
        welfare_type       = welfare_type,
        reporting_level    = reporting_level,
        ppp                = ppp,
        lkup               = lkup,
        con                = read_con
        )
    } else {
      ## survey years ------------------
      out <- rg_pip(
        country         = lcv$est_ctrs,
        year            = year,
        povline         = povline,
        popshare        = popshare,
        welfare_type    = welfare_type,
        reporting_level = reporting_level,
        ppp             = ppp,
        lkup            = lkup,
        con             = read_con
      )
    }
    # It is important to close the read connection before you open a write connection because
    # duckdb kind of inherits read_only flag from previous connection object if it is not closed
    # More details here https://app.clickup.com/t/868cdpe3q
    duckdb::dbDisconnect(read_con)
    cached_data <- out$data_in_cache
    main_data <- out$main_data

    if (nrow(main_data) > 0) {
      out <- main_data |>
        collapse::fmutate(path = as.character(path)) |>
        collapse::rowbind(cached_data)
      # cached_data is NULL when we are querying live data in which case we don't update cache
      # This will be used only for development purpose and we don't have any intention to use it in production.
      if(!is.null(cached_data)) {
        # Update cache with data
        update_master_file(main_data, cache_file_path, fill_gaps)
      }
    } else {
      out <- cached_data
    }
    if (!data.table::is.data.table(out)) {
      setDT(out)
    }
    # Early return for empty table---------------
    if (nrow(out) == 0) return(pipapi::empty_response)

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
        df          = out,
        group_lkup  = lkup[["pop_region"]],
        return_cols = lkup$return_cols$pip_grp
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


    # pre-computed distributional stats ---------------
    crr_names  <- names(out)    # current variables
    names2keep <- lkup$return_cols$pip$cols # all variables

    out <- add_dist_stats(
      df = out,
      dist_stats = lkup[["dist_stats"]]
    )

    # Add aggregate medians ----------------
    out <- add_agg_medians(
      df        = out,
      fill_gaps = fill_gaps,
      data_dir  = lkup$data_root
    )

    # format ----------------


    if (fill_gaps) {

    ## Inequality indicators to NA for lineup years ----
      dist_vars  <- names2keep[!(names2keep %in% crr_names)]
      out[,
          (dist_vars) := NA_real_]

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
      names2keep  <- c(names2keep, added_names)

    }
    # Keep relevant variables
    out  <- out[, .SD, .SDcols = names2keep]


    # make sure we always report the same precision in all numeric variables
    doub_vars <-
      names(out)[unlist(lapply(out, is.double))] |>
      data.table::copy()

    out[, (doub_vars) := lapply(.SD, round, digits = 12),
       .SDcols = doub_vars]

    # Order rows by country code and reporting year
    data.table::setorder(out, country_code, reporting_year, reporting_level, welfare_type)
  #}

  # Make sure no duplicate remains
  out <- out |> collapse::funique()
  # return -------------
  return(out)
}
