#' Aggregate by group
#' @param df data.frame: Response from `fg_pip()` or `rg_pip()`.
#' @param group_lkup data.frame Group lkup table (pop_region)
#' @noRd
aggregate_by_group <- function(df, group_lkup) {

  df <- df[, .(
    region_code,
    reporting_year,
    poverty_line,
    mean,
    headcount,
    poverty_gap,
    poverty_severity,
    watts,
    reporting_pop
  )]

  cols <- c("headcount", "poverty_gap", "poverty_severity", "watts", "mean")
  group_lkup <- group_lkup[, c("region_code", "reporting_year", "reporting_pop")]

  # Compute stats weighted average by groups
  rgn <- df[, lapply(.SD, stats::weighted.mean, w = reporting_pop, na.rm = TRUE),
    by = .(region_code, reporting_year, poverty_line),
    .SDcols = cols
  ]

  rgn <- group_lkup[rgn,
    on = .(region_code, reporting_year),
    allow.cartesian = TRUE
  ]

  # Compute world aggregates
  wld <- compute_world_aggregates(rgn = rgn,
                                  cols = cols)

  # Combine
  out <- rbind(rgn, wld, fill = TRUE)

  # Compute population living in poverty
  out <- out[, pop_in_poverty := round(headcount * reporting_pop, 0)]

  return(out)
}


compute_world_aggregates <- function(rgn, cols) {
  # Compute stats
  wld <- rgn[, lapply(.SD,
                      stats::weighted.mean,
                      w = reporting_pop,
                      na.rm = TRUE),
             by = .(reporting_year, poverty_line),
             .SDcols = cols
  ]
  # Compute yearly population WLD totals
  tmp <- rgn[, .(reporting_pop = sum(reporting_pop)),
             by = .(reporting_year)]


  wld <- wld[tmp, on = .(reporting_year = reporting_year)]
  wld[["region_code"]] <- "WLD"

  return(wld)

}
