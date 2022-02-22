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
#' @export
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

    out <- pip_aggregate_by(
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
                 "year",
                 "pop",
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
    year,
    poverty_line,
    mean,
    headcount,
    poverty_gap,
    poverty_severity,
    watts,
    pop
  )]

  # Compute population totals
  pop <- df[, lapply(.SD,
                     base::sum,
                     na.rm = TRUE),
            by = .(year, poverty_line),
            .SDcols = "pop"
  ]

  # Compute stats weighted average by groups
  cols <- c("headcount", "poverty_gap", "poverty_severity", "watts", "mean")
  df <- df[, lapply(.SD,
                      stats::weighted.mean,
                      w = pop,
                      na.rm = TRUE),
             by = .(year, poverty_line),
             .SDcols = cols
  ]

  # Combine results
  df <- df[pop, on = .(year, poverty_line)]

  df$region_code <- "CUSTOM"


  # Compute population living in poverty
  df <- df[, pop_in_poverty := round(headcount * pop, 0)]

  return(df)
}

#' Aggregate by predefined groups
#' @param df data.frame: Response from `fg_pip()` or `rg_pip()`.
#' @param group_lkup data.frame Group lkup table (pop_region)
#' @noRd
pip_aggregate_by <- function(df, group_lkup) {

  # Keep only rows necessary for regional aggregates
  df <- filter_for_aggregate_by(df)

  df <- df[, .(
    region_code,
    year,
    poverty_line,
    mean,
    headcount,
    poverty_gap,
    poverty_severity,
    watts,
    pop
  )]

  cols <- c("headcount", "poverty_gap", "poverty_severity", "watts", "mean")
  group_lkup <- group_lkup[, c("region_code", "year", "pop")]

  # Compute stats weighted average by groups
  rgn <- df[, lapply(.SD, stats::weighted.mean, w = pop, na.rm = TRUE),
            by = .(region_code, year, poverty_line),
            .SDcols = cols
  ]

  rgn <- group_lkup[rgn,
                    on = .(region_code, year),
                    allow.cartesian = TRUE
  ]

  # Compute world aggregates
  wld <- compute_world_aggregates(rgn = rgn,
                                  cols = cols)

  # Combine
  out <- rbind(rgn, wld, fill = TRUE)

  # Compute population living in poverty
  out <- out[, pop_in_poverty := round(headcount * pop, 0)]

  return(out)
}


compute_world_aggregates <- function(rgn, cols) {
  # Compute stats
  wld <- rgn[, lapply(.SD,
                      stats::weighted.mean,
                      w = pop,
                      na.rm = TRUE),
             by = .(year, poverty_line),
             .SDcols = cols
  ]
  # Compute yearly population WLD totals
  tmp <- rgn[, .(pop = sum(pop)),
             by = .(year)]


  wld <- wld[tmp, on = .(year = year)]
  wld[["region_code"]] <- "WLD"

  return(wld)

}


#' Filter relevant rows for aggregating by predefined groups
#' @param df data.frame: Response from `fg_pip()`
#' @noRd
filter_for_aggregate_by <- function(df) {
  # This algorithm is incorrect, but should mostly work as a first iteration
  # Keep only one row per country / year
  # If nationally representative survey is available, use it
  # Otherwise, use whatever is available

  out <- df[, check := length(reporting_level),
            by = c("country_code", "year", "poverty_line")]
  out <- out[out$check == 1 | (out$check > 1 & reporting_level == "national"), ]

  return(out)

}
