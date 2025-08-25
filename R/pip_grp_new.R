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




}

#' Subset country_code values based on matches in *_code columns
#'
#' This function searches all columns in a data.table ending with '_code' (except 'country_code')
#' and returns a unique character vector of 'country_code' values for rows where any of those
#' columns match a value in the provided 'country' vector. The input data.table 'dt' should be
#' 'lkup$aux_files$country_list', which contains country and region codes for subsetting.
#'
#' @param dt A data.table, typically lkup$aux_files$country_list, containing country_code and other *_code columns.
#' @param country Character vector of country or region codes to match against *_code columns.
#'
#' @return A unique character vector of country_code values corresponding to matches in any *_code column.
#' @examples
#' \dontrun{
#' dt <- lkup$aux_files$country_list
#' get_country_code_subset(dt, c("USA", "EAP"))
#' }
get_country_code_subset <- function(dt, country) {
  # Find all *_code columns except 'country_code'
  code_cols <- setdiff(grep("_code$", names(dt), value = TRUE), "country_code")

  # Initialize result vector
  result <- character(0)

  # For each code column, check for matches and collect country_code
  for (col in code_cols) {
    # Find rows where the code column matches any value in country
    idx <- dt[[col]] %in% country
    if (any(idx, na.rm = TRUE)) {
      result <- c(result, dt[idx, country_code])
    }
  }

  # Return unique country_code values
  funique(result)
}
