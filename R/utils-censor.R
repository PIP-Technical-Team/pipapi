# utils-censor.R
#
# Censoring helpers and estimate_type classifiers.
#
# Functions:
#   censor_rows()            - create tmp_id and dispatch to censor_stats()
#   censor_stats()           - apply censor table (remove rows / set cols to NA)
#   estimate_type_var()      - label regional estimates as actual/projection/nowcast
#   estimate_type_ctr_lnp() - label country lineup estimates as actual/projection/nowcast


#' Censor rows
#'
#' Censors statistics based on a pre-defined censor table.
#'
#' @param df data.table: Table to censor. Output from \code{pip()}.
#' @param censored list: List with censor tables.
#' @param type character: Type of censor table. One of \code{"countries"} or
#'   \code{"regions"}.
#' @return data.table
#' @noRd
censor_rows <- function(df, censored, type = c("countries", "regions")) {
  type <- match.arg(type)

  # Create tmp_id to match with censor table
  if (type == "countries") {
    df$tmp_id <-
      sprintf(
        "%s_%s_%s_%s_%s",
        df$country_code,
        df$reporting_year,
        df$survey_acronym,
        df$welfare_type,
        df$reporting_level
      )
  } else {
    df$tmp_id <-
      sprintf(
        "%s_%s",
        df$region_code,
        df$reporting_year
      )
  }

  # Apply censoring
  out <- censor_stats(df, censored[[type]])
  out$tmp_id <- NULL

  return(out)
}


#' Apply censor table to a data.table
#'
#' Removes rows flagged with \code{statistic == "all"} and sets individual
#' statistic columns to \code{NA} for partial censors.
#'
#' @param df data.table: Table to censor (must have a \code{tmp_id} column).
#' @param censored_table data.table: Censor table with columns \code{id} and
#'   \code{statistic}.
#' @return data.table
#' @keywords internal
censor_stats <- function(df, censored_table) {
  # make sure everything is data.table
  setDT(df)
  setDT(censored_table)

  # Create a binary column to mark rows for removal based on 'all' statistic
  df[, to_remove := FALSE]
  censor_all <- censored_table[statistic == "all", .(id)]
  if (nrow(censor_all) > 0) {
    df[censor_all, on = .(tmp_id = id), to_remove := TRUE]
  }

  # Remove marked rows
  df <- df[to_remove == FALSE]

  # Update specific statistics to NA where not 'all'
  censor_stats <- censored_table[statistic != "all"]
  if (nrow(censor_stats) > 0) {
    # Perform a non-equi join to mark relevant statistics
    # Commenting mult = "first" since with multiple povline values there are more than one rows
    df[
      censor_stats,
      on = .(tmp_id = id),
      unique(censor_stats$statistic) := NA_real_
    ]
  }

  # Clean up the temporary column
  df[, to_remove := NULL]

  return(df)
}


#' Label regional estimates by estimate type
#'
#' Classifies regional estimates as \code{"actual"}, \code{"projection"}, or
#' \code{"nowcast"} and censors specific stats where applicable.
#'
#' @param df data.table: Table to process.
#' @param lkup list: lkup value
#' @keywords internal
estimate_type_var <- function(df, lkup) {
  censored_table <- lkup$censored$regions
  data_dir <- lkup$data_root

  mr <- get_metaregion_table(data_dir = data_dir)

  df[, tmp_id := paste(region_code, reporting_year, sep = "_")]
  # by default all estimates are actual
  df[, estimate_type := "actual"]

  # censored table for all statistics
  censor_all <- censored_table[statistic == "all", .(id)]
  if (nrow(censor_all) > 0) {
    # If censored in all stats, which is equivalent to no coverage,
    # label as "projection"
    df[censor_all, on = .(tmp_id = id), estimate_type := "projection"]
  }

  # Merge metaregion and label those obs with reporting year
  # higher than lineup year as "nowcast"
  df <- mr[df, on = "region_code"]
  df[reporting_year > lineup_year, estimate_type := "nowcast"]

  # Update specific statistics to NA where not 'all'
  censor_stats <- censored_table[statistic != "all"]
  if (nrow(censor_stats) > 0) {
    # Perform a non-equi join to mark relevant statistics
    df[
      censor_stats,
      on = .(tmp_id = id),
      mult = "first",
      (censor_stats$statistic) := NA_real_
    ]
  }
  df[, c("tmp_id", "lineup_year") := NULL]
}


#' Add estimate_type to country-level lineup estimates
#'
#' Labels each row as \code{"actual"} (survey year), \code{"projection"}
#' (interpolated), or \code{"nowcast"} (beyond regional lineup year).
#'
#' @param out data.table: current database
#' @param lkup list: lkup list
#'
#' @return data.table with \code{estimate_type} column added
#' @keywords internal
estimate_type_ctr_lnp <- function(out, lkup) {
  out[,
    estimate_type := fifelse(
      estimation_type == "survey",
      "actual",
      "projection"
    )
  ]
  mr <- get_metaregion_table(lkup$data_root)
  wld <- mr[region_code == "WLD", lineup_year]
  regs <- out[, unique(region_code)]
  mr <- mr[region_code %in% regs]
  mr[, lineup_year := max(lineup_year, wld), by = region_code]

  # Merge metaregion and label those obs with reporting year
  # higher than lineup year as "nowcast"
  out <- mr[out, on = "region_code"]
  out[reporting_year > lineup_year, estimate_type := "nowcast"]

  out[, lineup_year := NULL]
}
