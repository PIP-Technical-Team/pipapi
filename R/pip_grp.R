#' Compute various aggregations of PIP statistics
#'
#'
#' @param country character: Country ISO 3 codes
#' @param year integer: Reporting year
#' @param povline numeric: Poverty line
#' @param popshare numeric: Proportion of the population living below the
#'   poverty line
#' @param group_by character: Will return aggregated values for predefined
#'   sub-groups
#' @param welfare_type character: Welfare type
#' @param reporting_level character: Geographical reporting level
#' @param lkup list: A list of lkup tables
#' @param debug logical: If TRUE poverty calculations from `wbpip` will run in
#'   debug mode
#'
#' @return data.table
#' @examples
#' \dontrun{
#' # Create lkups
#' lkups <- create_lkups("<data-folder>")
#'
#' # A single country and year
#' pip_grp(country = "all",
#'         year = 2000,
#'         povline = 1.9,
#'         group_by = "wb",
#'         lkup = lkups)
#' }
#'
pip_grp <- function(country = "all",
                    year = "all",
                    povline = 1.9,
                    popshare = NULL,
                    group_by = c("none", "wb"),
                    welfare_type = c("all", "consumption", "income"),
                    reporting_level = c("all", "national"),
                    lkup,
                    debug = FALSE) {

  welfare_type <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by <- match.arg(group_by)

  # Custom aggregations only supported at the national level
  if (group_by != "none") {
    reporting_level <- "all"
  } else {
    reporting_level <- "national"
  }

  out <- fg_pip(
    country = country,
    year = year,
    povline = povline,
    popshare = popshare,
    welfare_type = welfare_type,
    reporting_level = reporting_level,
    lkup = lkup,
    ppp = NULL,
    debug = debug
  )

  # return empty dataframe if no metadata is found
  if (nrow(out) == 0) {
    return(out)
  }

  # Handles aggregated distributions
  if (reporting_level %in% c("national", "all")) {
    out <- add_agg_stats(out)
  }

  # Handle potential (insignificant) difference in poverty_line values that
  # may mess-up the grouping
  out$poverty_line <- povline

  # Handle aggregations with sub-groups
  if (group_by != "none") {

    out <- aggregate_by_group(
      df = out,
      group_lkup = lkup[["pop_region"]]
    )
    # Censor regional values
    out <- censor_rows(out, lkup[["censored"]], type = "region")

  } else {
    # Handle simple aggregation
    out <- pip_aggregate(out)
  }

  out <- out[, c("region_code",
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


pip_aggregate <- function(df) {
  # Handle simple aggregation
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

  # Compute population totals
  pop <- df[, lapply(.SD,
                     base::sum,
                     na.rm = TRUE),
            by = .(reporting_year, poverty_line),
            .SDcols = "reporting_pop"
  ]

  # Compute stats weighted average by groups
  cols <- c("headcount", "poverty_gap", "poverty_severity", "watts", "mean")
  df <- df[, lapply(.SD,
                      stats::weighted.mean,
                      w = reporting_pop,
                      na.rm = TRUE),
             by = .(reporting_year, poverty_line),
             .SDcols = cols
  ]

  # Combine results
  df <- df[pop, on = .(reporting_year, poverty_line)]

  df$region_code <- "CUSTOM"


  # Compute population living in poverty
  df <- df[, pop_in_poverty := round(headcount * reporting_pop, 0)]

  return(df)
}

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
