aggregate_by_group <- function(df, group_lkup) {

  df <- df[, .(pcn_region_code,
                reporting_year,
                poverty_line,
                headcount,
                poverty_gap,
                poverty_severity,
                watts,
                reporting_pop)]

  cols <- c("headcount", "poverty_gap", "poverty_severity", "watts")

  # Compute stats weighted average by groups
  rgn <- df[ , lapply(.SD, weighted.mean, w = reporting_pop, na.rm = TRUE),
             by = .(pcn_region_code, reporting_year, poverty_line),
             .SDcols = cols]

  # Compute world aggregates
  wld <- group_lkup[rgn,
                    on = .(pcn_region_code, reporting_year),
                    allow.cartesian = TRUE]

  wld <- wld[ , lapply(.SD, weighted.mean, w = reporting_pop, na.rm = TRUE),
              by = .(reporting_year, poverty_line),
              .SDcols = cols]

  wld[["pcn_region_code"]] <- "WLD"

  # Combine
  out <- rbind(rgn, wld)

  return(out)
}
