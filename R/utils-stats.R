# utils-stats.R
#
# Functions that enrich a poverty-estimates data.table with pre-computed
# or auxiliary statistics (distributional stats, SPL/SPR, Prosperity Gap,
# distribution type, medians).
#
# Functions:
#   add_dist_stats()          - merge distributional stats (new pathway)
#   add_dist_stats_old()      - merge distributional stats (old pathway)
#   add_distribution_type()   - classify surveys as micro/group/imputed/mixed
#   add_pg()                  - add Prosperity Gap indicators
#   add_spl()                 - add Shared Prosperity Line indicators
#   add_agg_medians()         - add aggregate medians from spr table
#   get_mean_median()         - merge mean and median from dist_stats into FGT
#   add_vars_out_of_pipeline()- orchestrator: calls spl, pg, distribution_type


#' Add pre-computed distributional stats (new pathway)
#'
#' @param df data.table: Data frame of poverty statistics
#' @param lkup list: lookup object containing dist_stats and lineup_dist_stats
#' @param fill_gaps logical: whether lineup-year estimates are being used
#'
#' @return data.table
#' @export
add_dist_stats <- function(df, lkup, fill_gaps) {
  if (fill_gaps) {
    dist_stats <- lkup[["lineup_dist_stats"]]
  } else {
    dist_stats <- lkup[["dist_stats"]]
  }

  if (fill_gaps) {
    df <- df |>
      joyn::joyn(
        y = dist_stats,
        by = c("country_code", "reporting_level", "reporting_year"),
        match_type = "m:1", # multiple poverty lines
        keep_common_vars = FALSE,
        reportvar = FALSE,
        verbose = FALSE,
        keep = "left"
      )
  } else {
    # Keep only relevant columns
    cols <- c(
      "cache_id",
      "reporting_level",
      "gini",
      "polarization",
      "mld",
      sprintf("decile%s", 1:10)
    )
    dist_stats <- dist_stats[, .SD, .SDcols = cols]

    df <- dist_stats[
      df,
      on = .(cache_id, reporting_level),
      allow.cartesian = TRUE
    ]
  }
  df
}


#' Add pre-computed distributional stats (old pathway)
#'
#' @param df data.table: Data frame of poverty statistics
#' @param dist_stats data.table: Distributional stats lookup
#'
#' @return data.table
#' @export
add_dist_stats_old <- function(df, dist_stats) {
  # Keep only relevant columns
  cols <- c(
    "cache_id",
    "reporting_level",
    "gini",
    "polarization",
    "mld",
    sprintf("decile%s", 1:10)
  )
  dist_stats <- dist_stats[, .SD, .SDcols = cols]

  df <- dist_stats[
    df,
    on = .(cache_id, reporting_level),
    allow.cartesian = TRUE
  ]

  return(df)
}


#' Classify surveys by distribution type
#'
#' Uses framework data to classify each survey year as micro, group, imputed,
#' or mixed. For lineup years, mixed classification is applied when a reporting
#' year spans surveys of different types.
#'
#' @param df data frame from \code{fg_pip()} or \code{rg_pip()}
#' @param lkup list: lookup table
#' @inheritParams pip
#'
#' @return data.table
#' @keywords internal
add_distribution_type <- function(df, lkup, fill_gaps) {
  # merge reference table with framework table and get distribution type
  # from framework
  rf <- copy(lkup$ref_lkup) |>
    _[, .(
      country_code,
      reporting_level,
      welfare_type,
      survey_acronym,
      reporting_year,
      surveyid_year
    )][,
      surveyid_year := as.numeric(surveyid_year)
    ]

  fw <- get_aux_table(data_dir = lkup$data_root, "framework") |>
    copy() |>
    _[, .(
      country_code,
      survey_acronym,
      surveyid_year,
      use_imputed,
      use_microdata,
      use_bin,
      use_groupdata
    )]

  dt <- collapse::join(
    x = rf,
    y = fw,
    on = c("country_code", "surveyid_year", "survey_acronym"),
    how = "left",
    validate = "m:1",
    verbose = 0
  )

  if (fill_gaps) {
    # line up years ----------

    by_vars <- c("country_code", "reporting_year", "welfare_type")

    dt[,
      # distribution type by year
      distribution_type := fcase(
        use_groupdata == 1 , "group"   ,
        use_imputed == 1   , "imputed" ,
        default = "micro"
      )
    ][,
      # find interpolation with different distribution type and
      # replace by "mixed"
      uniq_dist := uniqueN(distribution_type),
      by = by_vars
    ][
      uniq_dist != 1,
      distribution_type := "mixed"
    ]

    dt <- dt[,
      # collapse by reporting_year and keep relevant variables
      .(distribution_type = unique(distribution_type)),
      by = by_vars
    ]
  } else {
    # survey years --------------
    by_vars <- c(
      "country_code",
      "surveyid_year",
      "welfare_type",
      "survey_acronym"
    )

    dt[,
      # distribution type by year
      distribution_type := fcase(
        use_groupdata == 1 , "group"   ,
        use_imputed == 1   , "imputed" ,
        default = "micro"
      )
    ]

    dt <- dt[,
      # collapse by reporting_year and keep relevant variables
      .(distribution_type = unique(distribution_type)),
      by = by_vars
    ]
  }

  if (!fill_gaps) {
    df <- df[,
      surveyid_year := as.numeric(surveyid_year)
    ]
  }
  df[dt, on = by_vars, distribution_type := i.distribution_type][,
    # Calculate unique counts of reporting level and add new rows
    unique_replevel := uniqueN(reporting_level),
    by = by_vars
  ]

  # distribution type for national cases when aggregate data

  df[
    unique_replevel == 3 &
      reporting_level == "national" &
      distribution_type == "group",
    distribution_type := "synthetic"
  ][,
    unique_replevel := NULL
  ]

  setorderv(df, by_vars)
  return(invisible(df))
}


#' Add Prosperity Gap indicators
#'
#' @param df data frame inside \code{fg_pip()} or \code{rg_pip()}
#' @param data_dir character: Directory path of auxiliary data (lkup$data_root)
#' @inheritParams pip
#'
#' @return data.table
#' @keywords internal
add_pg <- function(df, fill_gaps, data_dir) {
  if (fill_gaps) {
    table <- "pg_lnp"
  } else {
    table <- "pg_svy"
  }

  pg <- get_pg_table(data_dir = data_dir, table = table)

  df[
    pg,
    on = c(
      "country_code",
      "reporting_year",
      "welfare_type",
      "reporting_level"
    ),
    pg := i.pg
  ]
}


#' Add Shared Prosperity Line indicators
#'
#' @param df data frame inside \code{fg_pip()} or \code{rg_pip()}
#' @param data_dir character: Directory path of auxiliary data (lkup$data_root)
#' @inheritParams pip
#'
#' @return data.table
#' @keywords internal
add_spl <- function(df, fill_gaps, data_dir) {
  if (fill_gaps) {
    table <- "spr_lnp"
  } else {
    table <- "spr_svy"
  }

  spl <- get_spr_table(data_dir = data_dir, table = table)

  out <- df[
    spl,
    on = c(
      "country_code",
      "reporting_year",
      "welfare_type",
      "reporting_level"
    ),
    `:=`(
      spl = i.spl,
      spr = i.spr
    )
  ]

  return(invisible(out))
}


#' Add aggregate medians
#'
#' For lineup years all medians are set to NA; for survey years the existing
#' median is preferred and missing values are filled from the spr table.
#'
#' @param df data frame from \code{fg_pip()} or \code{rg_pip()}
#' @param data_dir character: Directory path of auxiliary data (lkup$data_root)
#' @inheritParams pip
#'
#' @return data.table
#' @keywords internal
add_agg_medians <- function(df, fill_gaps, data_dir) {
  if (fill_gaps) {
    table = "spr_lnp"
    # set all lineup medians to NA.
    df[, median := NA_real_]
  } else {
    # if survey data, we keep the ones already calculated and add those
    # that are missing
    table = "spr_svy"
  }
  med <- get_spr_table(data_dir = data_dir, table = table)

  # join medians to missing data ---------
  df[
    med,
    on = c(
      "country_code",
      "reporting_year",
      "welfare_type",
      "reporting_level"
    ),
    # prefer median in df over the one in med as long as the one in
    # df is not NA. If that is the case, select the one in med.
    median := fcoalesce(median, i.median)
  ]

  return(invisible(df))
}


#' Merge mean and median from dist_stats into an FGT table
#'
#' Early-returns if the lookup is not on the new lineup version.
#'
#' @param fgt data.table with FGT measures
#' @param lkup lkup list
#' @param fill_gaps logical: whether lineup-year estimates are being used
#'
#' @return data.table with FGT, mean and median
#' @keywords internal
get_mean_median <- \(fgt, lkup, fill_gaps) {
  if (isFALSE(lkup$use_new_lineup_version)) {
    return(fgt)
  }

  if (fill_gaps) {
    dist <- get_vars(
      lkup$lineup_dist_stats,
      c("country_code", "reporting_year", "reporting_level", "mean", "median")
    )
    by_var <- c('country_code', "reporting_year", "reporting_level")
  } else {
    dist <- get_vars(
      lkup$dist_stats,
      c(
        "country_code",
        "reporting_year",
        "reporting_level",
        "mean",
        "survey_median_ppp",
        "welfare_type"
      )
    )
    setnames(dist, "survey_median_ppp", "median")

    by_var <- c(
      'country_code',
      "reporting_year",
      "reporting_level",
      "welfare_type"
    )
  }
  join(
    x = fgt,
    y = dist,
    on = by_var,
    how = "left",
    validate = "m:1", # multiple povlines
    verbose = 0L
  )
}


#' Add all variables estimated outside the core pipeline
#'
#' Orchestrates the addition of SPL/SPR, Prosperity Gap, and distribution
#' type. Any future out-of-pipeline variables should be added here.
#'
#' @inheritParams add_distribution_type
#'
#' @return data.table (modified in-place)
#' @keywords internal
add_vars_out_of_pipeline <- function(out, fill_gaps, lkup) {
  ## Add SPL and SPR  ---------------
  out <- add_spl(df = out, fill_gaps = fill_gaps, data_dir = lkup$data_root)

  ## Add prosperity Gap -----------
  out <- add_pg(df = out, fill_gaps = fill_gaps, data_dir = lkup$data_root)

  ## add distribution type -------------
  # based on info in framework data, rather than welfare data
  out <- add_distribution_type(df = out, lkup = lkup, fill_gaps = fill_gaps)

  invisible(out)
}
