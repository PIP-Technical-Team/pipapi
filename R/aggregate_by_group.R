aggregate_by_group <- function(df, group_lkup) {
  # TEMPORARY FIX: TO BE MODIFIED
  data.table::setnames(df, old = "pcn_region_code", new = "region_code")

  df <- df[, .(
    region_code,
    reporting_year,
    poverty_line,
    headcount,
    poverty_gap,
    poverty_severity,
    watts,
    reporting_pop
  )]

  cols <- c("headcount", "poverty_gap", "poverty_severity", "watts")
  group_lkup <- group_lkup[, .(region_code, reporting_year, reporting_pop)]

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
  wld <- rgn[, lapply(.SD, stats::weighted.mean, w = reporting_pop, na.rm = TRUE),
    by = .(reporting_year, poverty_line),
    .SDcols = cols
  ]

  wld$reporting_pop <- sum(rgn$reporting_pop)
  wld[["region_code"]] <- "WLD"

  # Combine
  out <- rbind(rgn, wld, fill = TRUE)

  # Compute population living in poverty
  out <- out[, pop_in_poverty := headcount * reporting_pop]

  return(out)
}
