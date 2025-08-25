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


}



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