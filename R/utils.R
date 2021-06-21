subset_lkup <- function(country,
                        year,
                        welfare_type,
                        svy_coverage,
                        lkup) {

  svy_n <- nrow(lkup)
  keep <- rep(TRUE, svy_n)
  # Select data files based on requested country, year, etc.
  # Select countries
  if (country[1] != "all") {
    keep <- keep & lkup$country_code %in% country
  }
  # Select years
  if (year[1] != "all") {
    keep <- keep & lkup$reporting_year %in% year
  }
  # Select welfare_type
  if (welfare_type[1] != "all") {
    keep <- keep & lkup$welfare_type == welfare_type
  }
  # Select survey coverage
  # To be updated: Fix the coverage variable names in aux data (reporting_coverage?)
  if (svy_coverage[1] != "all") {
    if ("survey_coverage" %in% names(lkup)) {
      keep <- keep &
        (lkup$survey_coverage == svy_coverage |
           lkup$pop_data_level  == svy_coverage)
    } else {
      keep <- keep & lkup$pop_data_level  == svy_coverage
    }
  }

  lkup <- lkup[keep, ]

  return(lkup)
}

get_svy_data <- function(svy_id,
                         svy_coverage,
                         path)
{
  # Each call should be made at a unique pop_data_level (equivalent to reporting_data_level: national, urban, rural)
  # This check should be conducted at the data validation stage
  svy_coverage <- unique(svy_coverage)
  assertthat::assert_that(length(svy_coverage) == 1,
                          msg = "Problem with input data: Multiple pop_data_levels")

  out <- lapply(path, function(x) {
    tmp <- fst::read_fst(x)
    if (svy_coverage %in% c("urban", "rural")) { # Not robust. Should not be hard coded here.
      tmp <- tmp[tmp$area == svy_coverage, ]
    }
    tmp <- tmp[, c("welfare", "weight")]

    return(tmp)
  })

  names_out <- sprintf("df%s",
                       seq_along(svy_id) - 1)
  names(out) <- names_out

  return(out)
}


create_empty_response <- function() {
  out <- data.frame(
           survey_id = character(0),
         region_code = character(0),
        country_code = character(0),
      reference_year = numeric(0),
       surveyid_year = numeric(0),
      reporting_year = numeric(0),
      survey_acronym = character(0),
     survey_coverage = character(0),
         survey_year = numeric(0),
        welfare_type = character(0),
     survey_mean_ppp = numeric(0),
  predicted_mean_ppp = numeric(0),
                 ppp = numeric(0),
       reference_pop = character(0),
       reference_gdp = character(0),
       reference_pce = character(0),
      pop_data_level = character(0),
      gdp_data_level = character(0),
      pce_data_level = character(0),
      cpi_data_level = character(0),
      ppp_data_level = character(0),
   distribution_type = character(0),
             gd_type = character(0),
        poverty_line = numeric(0),
                mean = numeric(0),
              median = numeric(0),
        poverty_rate = numeric(0),
         poverty_gap = numeric(0),
    poverty_severity = numeric(0),
               watts = numeric(0),
                gini = numeric(0),
                 mld = numeric(0),
        polarization = numeric(0),
             decile1 = numeric(0),
             decile2 = numeric(0),
             decile3 = numeric(0),
             decile4 = numeric(0),
             decile5 = numeric(0),
             decile6 = numeric(0),
             decile7 = numeric(0),
             decile8 = numeric(0),
             decile9 = numeric(0),
            decile10 = numeric(0),
     is_interpolated = numeric(0)
  )

  return(out)
}

collapse_rows <- function(df, vars, na_var) {
  tmp_vars <- lapply(df[, .SD, .SDcols = vars], unique, collapse = "|")
  tmp_vars <- lapply(tmp_vars, paste, collapse = "|")
  tmp_var_names <- names(df[, .SD, .SDcols = vars])
  df[[na_var]] <- NA_real_
  for (tmp_var in seq_along(tmp_vars)) {
    df[[tmp_var_names[tmp_var]]] <- tmp_vars[[tmp_var]]
  }
  df <- unique(df)
}

add_dist_stats <- function(df, dist_stats) {
  # Keep only relevant columns
  cols <- c("cache_id",
            "country_code",
            "reporting_year",
            "welfare_type",
            "pop_data_level",
            "survey_median_ppp",
            "gini",
            "polarization",
            "mld",
            sprintf("decile%s", 1:10))
  dist_stats <- dist_stats[, .SD, .SDcols = cols]

  # merge dist stats with main table
  data.table::setnames(dist_stats, "survey_median_ppp", "median")

  df <- dist_stats[df,
                    on = .(cache_id, country_code, reporting_year, welfare_type, pop_data_level),
                    allow.cartesian = TRUE]

  return(df)
}

#' Censor rows
#' Censor statistics based on a pre-defined censor table.
#' @param df data.table: Table to censor, e.g output from `pip()`.
#' @param censored_table data.table: Censor table.
#' @return data.table
#' @noRd
censor_rows <- function(df, censored_table) {

  df$tmp_id <-
    sprintf('%s_%s_%s',
            df$country_code, df$reporting_year,
            df$welfare_type)

  if (any(df$tmp_id %in% censored_table$id)) {
    for (i in seq_len(nrow(df))) {
      for (y in seq_len(nrow(censored_table))) {
        if (df$tmp_id[i] == censored_table$id[y]) {
          df[[censored_table$statistic[y]]][i] <- NA
        }
      }
    }
  }
  df$tmp_id <- NULL

  return(df)
}
