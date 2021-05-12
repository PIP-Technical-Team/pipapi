#' Provides numbers that will populate the home page main chart
#'
#' @param povline numeric: Poverty line
#' @param lkup
#' @param paths
#'
#' @return data.frame
#' @export
#'
ui_hp_stacked <- function(povline = 1.9,
                          lkup,
                          paths) {
  out <- pip(country = "all",
             year    = "all",
             povline = povline,
             lkup = lkups,
             fill_gaps = TRUE,
             aggregate = TRUE,
             group_by = TRUE,
             svy_coverage = "national",
             paths = paths)

  out <- out[, .(region_code, reporting_year, poverty_line, pop_in_poverty)]
  return(out)
}
