#' Compute various aggregations of PIP statistics
#'
#' @inheritParams pip
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
pip_grp <- function(country         = "ALL",
                    year            = "ALL",
                    povline         = 1.9,
                    group_by        = c("wb", "none"),
                    welfare_type    = c("all", "consumption", "income"),
                    reporting_level = c("all", "national"),
                    lkup,
                    censor          = TRUE,
                    lkup_hash       = lkup$cache_data_id$hash_pip_grp) {

  welfare_type    <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by        <- match.arg(group_by)

  # TEMPORARY UNTIL SELECTION MECHANISM IS BEING IMPROVED
  country <- toupper(country)
  year <- toupper(year)

  # If ref_lkup is not part of lkup throw an error.
  if (!all(c('ref_lkup') %in% names(lkup)))
    stop("You are probably passing more than one dataset as lkup argument.
  Try passing a single one by subsetting it lkup <- lkups$versions_paths$dataset_name_PROD")


  # Custom aggregations only supported at the national level
  # subgroups aggregations only supported for "all" countries
  if (group_by != "none") {
    reporting_level <- "all"
    if (!all(country %in% c("ALL", lkup$query_controls$region$values))) {
      country <- "ALL"
    }
  } else {
    reporting_level <- "national"
  }

  out <- fg_pip(
    country         = country,
    year            = year,
    povline         = povline,
    popshare        = NULL,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    ppp             = NULL,
    lkup           = lkup)

  # return empty dataframe if no metadata is found
  if (nrow(out) == 0) {
    return(pipapi::empty_response_grp)
  }

  # Handles aggregated distributions (like CHN and IND)
  if (tolower(reporting_level) %in% c("national", "all")) {
    out <- add_agg_stats(out,
                         return_cols = lkup$return_cols$ag_average_poverty_stats)
  }

  # Handle potential (insignificant) difference in poverty_line values that
  # may mess-up the grouping
  out$poverty_line <- povline

  # Handle aggregations with sub-groups
  if (group_by != "none") {

    out <- pip_aggregate_by(
      df = out,
      group_lkup = lkup[["pop_region"]],
      country = country,
      return_cols = lkup$return_cols$pip_grp
    )

    out <- estimate_type_var(out,lkup)

    # Censor regional values
    # if (censor) {
    #   out <- censor_rows(out, lkup[["censored"]], type = "regions")
    # }

  } else {
    # Handle simple aggregation
    out <- pip_aggregate(out,
                         return_cols = lkup$return_cols$pip_grp)
    out <- estimate_type_var(out,lkup)
  }

  keep <- lkup$return_cols$pip_grp$cols
  out <- out[, .SD, .SDcols = keep]

  return(out)
}


#' Calculate estimates for aggregates different to the official regional
#' aggregation
#'
#' @param df data.table from `pip_fg()`
#' @param by character: Additional variable to use in `by` when doing the
#'   aggregations. Default is `NULL`, but it should be use to include
#'   aggregations variables
#' @param return_cols list: lkup$return_cols$pip_grp object. Controls returned
#' columns
#'
#' @return data.table
pip_aggregate <- function(df, by = NULL, return_cols) {

  all_cols <- return_cols$cols
  weighted_cols <- return_cols$weighted_average_cols

  ## Assess by parameter ---------

  if (is.null(by)) {

    by_code <- "CUSTOM"
    by_name <- "CUSTOM"

    to_keep <- all_cols[!all_cols %in%  c("pop_in_poverty", "estimate_type")]

  } else {

    if (grepl("code$", by)) {

      by_code <- by
      by_name <- gsub("_code", "", by)

    } else {

      by_code <- paste0(by, "_code")
      by_name <- by

    }

    to_keep <- all_cols[!all_cols %in% c("pop_in_poverty",
                                         "region_code",
                                         "region_name",
                                         "estimate_type")]
    to_keep <- c(by_name, by_code, to_keep)

    by <- c(by_name, by_code)
  }



  # Handle simple aggregation

  df <- df[, .SD, .SDcols = to_keep]

  byvar <- c(by, "reporting_year", "poverty_line")

  # Compute population totals
  pop <- df[, lapply(.SD,
                     base::sum,
                     na.rm = TRUE),
            by = byvar,
            .SDcols = "reporting_pop"
  ]

  # Compute stats weighted average by groups
  df <- df[, lapply(.SD,
                      stats::weighted.mean,
                      w = reporting_pop,
                      na.rm = TRUE),
             by = byvar,
             .SDcols = weighted_cols
  ]

  # Combine results
  df <- df[pop, on = byvar]

  ## Add region code and name -----
  if (is.null(by)) {

    df$region_code <- "CUSTOM"
    df$region_name <- "CUSTOM"

  } else {
    data.table::setnames(df,
                         c(by_name, by_code),
                         c("region_name", "region_code"))
  }

  # Compute population living in poverty
  df <- df[, pop_in_poverty := round(headcount * reporting_pop, 0)]

  return(df)

}

#' Aggregate by predefined groups
#' @param df data.frame: Response from `fg_pip()` or `rg_pip()`.
#' @param group_lkup data.frame: Group lkup table (pop_region)
#' @param country character: Selected countries / regions
#' @param return_cols list: lkup$return_cols$pip_grp object. Controls returned
#' columns
#' @noRd
pip_aggregate_by <- function(df,
                             group_lkup,
                             country = "ALL",
                             return_cols) {

  all_cols <- return_cols$cols
  weighted_cols <- return_cols$weighted_average_cols

  # Keep only rows necessary for regional aggregates
  df <- filter_for_aggregate_by(df)

  to_keep <- all_cols[!all_cols %in% c("pop_in_poverty",
                                       "estimate_type")]

  df <- df[, .SD, .SDcols = to_keep]

  group_lkup <- group_lkup[, c("region_code",
                               "reporting_year",
                               "reporting_pop")]

  # Compute stats weighted average by groups
  rgn <- df[, lapply(.SD, stats::weighted.mean,
                     w = reporting_pop,
                     na.rm = TRUE),
            by = .(region_name,
                   region_code,
                   reporting_year,
                   poverty_line),
            .SDcols = weighted_cols
            ]

  rgn <- group_lkup[rgn,
                    on = .(region_code, reporting_year),
                    allow.cartesian = TRUE
  ]

  if (any(c("ALL", "WLD") %in% country)) {
    # Compute world aggregates
    wld <- compute_world_aggregates(rgn = rgn,
                                    cols = weighted_cols)
    if (length(country) == 1) {
      if (country == "WLD") {
        # Return only world aggregate
        out <- wld
      } else if (country == "ALL") {
        # Combine with other regional aggregates
        out <- rbind(rgn, wld, fill = TRUE)
      }
    } else {
      # Combine with other regional aggregates
      out <- rbind(rgn, wld, fill = TRUE)
      # Return selection only
      if (!"ALL" %in% country) {
        out <- out[region_code %in% country, ]
      }
    }
  } else {
    # Return only selected regions
    out <- rgn
  }


  # Compute population living in poverty
  out$pop_in_poverty <- round(out$headcount * out$reporting_pop, 0)

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
  wld[["region_name"]] <- "World"

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
            by = c("country_code", "reporting_year", "poverty_line")]
  out <- out[out$check == 1 | (out$check > 1 & reporting_level == "national"), ]

  return(out)

}
