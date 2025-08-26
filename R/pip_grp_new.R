#' New way to estimate Aggregate data
#' @rdname pip_gg
pip_grp_new <- \(country         = "ALL",
                 year            = "ALL",
                 povline         = 1.9,
                 welfare_type    = c("all", "consumption", "income"),
                 reporting_level = c("all", "national"),
                 lkup,
                 censor          = TRUE,
                 additional_ind  = FALSE) {

  welfare_type    <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)

  # Custom aggregations only supported at the national level
  # subgroups aggregations only supported for "all" countries
  country <- toupper(country)
  year    <- toupper(year)
  reporting_level <- "all"
  if (!all(country %in% c("ALL", lkup$query_controls$region$values))) {
    country <- "ALL"
  }

  # Select countries to estimate poverty
  cts <- copy(lkup$aux_files$country_list)
  country_code  <-  if (country != "ALL") {
    get_country_code_subset(dt = cts, country = country)
  } else {
    "ALL"
  }









  out <- fg_pip(
    country         = country_code,
    year            = year,
    povline         = povline,
    popshare        = NULL,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    ppp             = NULL,
    lkup           = lkup)

  out <- treat_cache_and_main(out)

  # return empty dataframe if no metadata is found
  if (nrow(out) == 0) {
    return(pipapi::empty_response_grp)
  }

  # Handles aggregated distributions (like CHN and IND)
  if (tolower(reporting_level) %in% c("national", "all")) {
    out <- add_agg_stats(out,
                         return_cols = lkup$return_cols$ag_average_poverty_stats)
  }

  add_vars_out_of_pipeline(out, fill_gaps = TRUE, lkup = lkup)

  # Handle potential (insignificant) difference in poverty_line values that
  # may mess-up the grouping
  # I don't think we need this out$poverty_line already has the correct values additionally,
  # since povline is vectorized the below line does not work as expected
  #out$poverty_line <- povline

  # Handle aggregations with sub-groups

    out <- pip_aggregate_by(
      df = out,
      group_lkup = lkup[["pop_region"]],
      country = country,
      return_cols = lkup$return_cols$pip_grp
    )

    out <- estimate_type_var(out,lkup)

    # Censor regional values
    if (censor) {
      out <- censor_rows(out, lkup[["censored"]], type = "regions")
    }


}

#' Subset country_code values based on matches in *_code columns and country_code
#'
#' This function searches all columns in a data.table ending with '_code' (except 'country_code'),
#' as well as 'country_code' itself, and returns a unique character vector of 'country_code' values
#' for rows where any of those columns match a value in the provided 'country' vector. If any value
#' in 'country' is not found in any *_code column or in 'country_code', an error is thrown. The input
#' data.table 'dt' should be 'lkup$aux_files$country_list', which contains country and region codes for subsetting.
#'
#' @param dt A data.table, typically lkup$aux_files$country_list, containing country_code and other *_code columns.
#' @param country Character vector of country or region codes to match against *_code columns and country_code.
#'
#' @return A unique character vector of country_code values corresponding to matches in any *_code column or country_code.
#' @examples
#' \dontrun{
#' dt <- lkup$aux_files$country_list
#' get_country_code_subset(dt, c("USA", "EAP"))
#' }
get_country_code_subset <- function(dt, country) {
  code_cols <- grep("_code$", names(dt), value = TRUE)
  result <- character(0)
  matched <- logical(length(country))

  for (col in code_cols) {
    idx <- dt[[col]] %in% country
    if (any(idx, na.rm = TRUE)) {
      result <- c(result, dt[idx, country_code])
      matched <- matched | country %in% dt[[col]]
    }
  }
  # Also check country_code itself
  idx_cc <- dt$country_code %in% country
  if (any(idx_cc, na.rm = TRUE)) {
    result <- c(result, dt[idx_cc, country_code])
    matched <- matched | country %in% dt$country_code
  }
  # Error if any country not matched
  if (any(!matched)) {
    cli::cli_abort(
      "The following values in {.arg country} were not found in any *_code column or country_code:
      {country[!matched]}")
  }
  funique(result)
}

#' List values in each *_code column that match the country vector
#'
#' Returns a named list where each element is the vector of unique values in each *_code column
#' that are present in the provided 'country' vector.
#'
#' @param dt A data.table, typically lkup$aux_files$country_list.
#' @param country Character vector of country or region codes to match against *_code columns.
#'
#' @return A named list of unique values for each *_code column that match 'country'.
#' @examples
#' \dontrun{
#' dt <- lkup$aux_files$country_list
#' list_code_column_values(dt, c("USA", "EAP"))
#' }
list_code_column_values <- function(dt, country) {
  code_cols <- grep("_code$", names(dt), value = TRUE)
  lapply(code_cols, \(col) {
    dt[get(col) %in% country, ..col] |>
      funique()
  }) |>
    setNames(code_cols)
}
