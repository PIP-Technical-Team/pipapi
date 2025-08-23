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
