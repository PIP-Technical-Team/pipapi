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
#' pip_new_lineups(country = "AGO",
#'     year = 2000,
#'     povline = 1.9,
#'     lkup = lkups)
#'
#' # All years for a single country
#' pip_new_lineups(country = "AGO",
#'     year = "all",
#'     povline = 1.9,
#'     lkup = lkups)
#'
#' # Fill gaps
#' pip_new_lineups(country = "AGO",
#'     year = "all",
#'     povline = 1.9,
#'     fill_gaps = TRUE,
#'     lkup = lkups)
#'
#' }
#' @export
pip_new_lineups <- function(
  country = "ALL",
  year = "ALL",
  povline = 1.9,
  popshare = NULL,
  fill_gaps = FALSE,
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
  povline <- round(povline, digits = 3)

  # TODO: Remove toupper() coercion when input validation is standardized upstream
  country <- toupper(country)
  if (is.character(year)) {
    year <- toupper(year)
  }

  # Validate lkup structure (covers svy_lkup and all new-pathway fields).
  # Replaces the former ad-hoc svy_lkup check with a consistent validator.
  validate_lkup(lkup, c("core", "new_pathway"))

  # Countries vector ------------
  validate_country_codes(country = country, lkup = lkup)

  # lcv$est_ctrs has all the country_code that we are interested in

  cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
  if (!file.exists(cache_file_path)) {
    # Create an empty duckdb file
    create_duckdb_file(cache_file_path)
  }
  # mains estimates ---------------
  if (fill_gaps) {
    ## lineup years-----------------
    out <- fg_pip(
      country = country,
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
    out <- rg_pip(
      country = country,
      year = year,
      povline = povline,
      popshare = popshare,
      welfare_type = welfare_type,
      reporting_level = reporting_level,
      ppp = ppp,
      lkup = lkup
    )
  }

  # Cache new data
  #---------------------------------------------
  out <- treat_cache_and_main(
    out,
    cache_file_path = cache_file_path,
    lkup = lkup,
    fill_gaps = fill_gaps
  )

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

  # Add out of pipeline variable
  #---------------------------------------------
  add_vars_out_of_pipeline(out, fill_gaps = fill_gaps, lkup = lkup)

  # Format, censor, select columns, order, de-duplicate ----------------
  out <- pip_lineups_format_output(
    out = out,
    lkup = lkup,
    fill_gaps = fill_gaps,
    reporting_level = reporting_level,
    censor = censor,
    additional_ind = additional_ind,
    use_old_dist_stats = FALSE
  )
  # return -------------
  return(out)
}


#' Merge main and cached FGT estimates into a single data.table
#' @noRd
treat_cache_and_main <- \(out, cache_file_path, lkup, fill_gaps) {
  # early return of cache data if not available.
  cached_data <-
    if (is.null(out$data_in_cache)) {
      NULL
    } else if (is.data.frame(out$data_in_cache)) {
      if (fnrow(out$data_in_cache) == 0) {
        NULL
      } else {
        ft <- qDT(out$data_in_cache)
        if (fill_gaps) {
          ft <-
            fg_remove_duplicates(
              ft,
              use_new_lineup_version = lkup$use_new_lineup_version
            )
        }

        # Add just mean and median
        get_mean_median(ft, lkup, fill_gaps = fill_gaps)
      }
    } else {
      cli::cli_abort(
        "{.code out$data_in_cache} must be NULL or data.frame not
      {.field {class(out$data_in_cache)}}"
      )
    }

  main_data <- qDT(out$main_data)

  if (nrow(main_data) > 0) {
    if (is.null(cached_data)) {
      out <- copy(main_data)
    } else {
      out <- main_data |>
        rowbind(cached_data)
    }

    update_master_file(main_data, cache_file_path, fill_gaps)
    rm(main_data)
  } else {
    out <- cached_data
  }

  setDT(out)
}


#' Abort if any element of country is not a valid PIP country code
#' @noRd
validate_country_codes <- \(country, lkup) {
  cls <- lkup$aux_files$country_list$country_code |>
    unique() |>
    c("ALL")

  if (any(!country %in% cls)) {
    wcls <- which(!country %in% cls)
    cli::cli_abort(
      "{.field {country[wcls]}} {?is/are} not {?a/} valid country code{?s}"
    )
  }
  invisible(TRUE)
}
